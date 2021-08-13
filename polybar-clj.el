;;; polybar-clj.el --- Display active CIDER connections in polybar -*- lexical-binding: t -*-

;; Copyright © 2021 Mark Dawson <markgdawson@gmail.com>

;; Author: Mark Dawson <markgdawson@gmail.com>
;; URL: https://github.com/markgdawson/polybar-clj-emacs
;; Keywords: project, convenience
;; Version: 0.0.1-snapshot
;; Package-Requires: ((emacs "25.1") dash)

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides an interface to a polybar module which tracks
;; the current CIDER connection and the connection status.
;;
;;; Code:

(require 'dash)

;; ---------------------------------------------------------
;; Store connections
;; ---------------------------------------------------------

(defvar pbclj-connection-states (make-hash-table)
  "Hash table to store the busy state of connections.")

(defvar pbclj-connection-aliases (make-hash-table)
  "Hash table to store display strings for connections.")

(defun pbclj-set-connection-busy (connection)
  "Mark CONNECTION as busy."
  (puthash connection t pbclj-connection-states))

(defun pbclj-set-connection-idle (connection)
  "Mark CONNECTION as idle."
  (puthash connection nil pbclj-connection-states))

(defun pbclj-connection-busy-p (connection)
  "Return non-nil if CONNECTION is marked as busy."
  (gethash connection pbclj-connection-states))

;; ---------------------------------------------------------
;; Store and update connection for current buffer
;; ---------------------------------------------------------

(defvar pbclj--current-connection nil)

(defun pbclj--on-buffer-change ()
  "If current buffer has changed store new buffer and run `(pbclj-polybar-update)`."
  (let ((cider-repl-buffer (cider-current-repl-buffer)))
    (unless (equal cider-repl-buffer pbclj--current-connection)
      (setq pbclj--current-connection cider-repl-buffer)
      (pbclj-polybar-update))))

(defun pbclj--emphasize-if-current (connection string)
  "Format STRING as current connection when CONNECTION is the current connection."
  (if (pbclj--connection-is-current connection)
      (format "%%{F#839496}%s%%{F-}" string)
    (format "%%{F#}%s%%{F-}" string)))

;; ---------------------------------------------------------
;; Connection Display Strings
;; ---------------------------------------------------------
(defcustom pbclj-busy-color "#d87e17"
  "Colour to display when connection is busy.")

(defcustom pbclj-idle-current-color "#839496"
  "Colour to display when connection is current and idle.")

(defcustom pbclj-idle-other-color "#4a4e4f"
  "Colour to display when connection is not current and idle.")

(defun pbclj--connection-is-current (connection)
  "Return non-nil if CONNECTION is the connection in the current buffer."
  (equal pbclj--current-connection connection))

(defun pbclj-connection-string (connection)
  "Format the display STRING for CONNECTION."
  (let* ((connection-name (buffer-name connection))
         (color (cond ((pbclj-connection-busy-p connection) pbclj-busy-color)
                      ((pbclj--connection-is-current connection) pbclj-idle-current-color)
                      (pbclj-idle-other-color)))
         (string (cond ((string-match "repvault-connect" connection-name) "RC")
                       ((string-match "repvault-backend" connection-name) "RVB")
                       ((string-match "repvault-test" connection-name) "RT")
                       ((string-match "repvault-graphql-gateway" connection-name) "RGG")
                       ((string-match "document-index-server" connection-name) "DIS")
                       ((pbclj-default-project-name connection)))))
    (format "%%{F%s}%s%%{F-}" color string)))

;; ---------------------------------------------------------
;; Status string printing
;; ---------------------------------------------------------
(defcustom pbclj-separator "%{F#4a4e4f} | %{F-}"
  "Separator between REPL instances.")

(defun pbclj--connections ()
  "Return list of connections as passed to nrepl."
  (mapcar #'cadr (sesman-sessions 'CIDER)))

(defun pbclj-status-string ()
  "Return status string displayed by polybar."
  (string-join (mapcar #'pbclj-connection-string
                       (pbclj--connections))
               pbclj-separator))

(defun pbclj-default-project-name (buffer)
  "Pick the default project name for connection in BUFFER as the name of
the session root directory."
  (if-let (project (sesman-project 'CIDER))
      (file-name-nondirectory (directory-file-name (file-name-directory project)))
    (buffer-name buffer)))

;; ---------------------------------------------------------
;; Polybar Connections
;; ---------------------------------------------------------

(defun pbclj-polybar-update ()
  "Forced polybar to update the cider component."
  (interactive)
  (mgd/polybar-send-hook "cider" 1))

(defun polybar-clojure-start-spinner (connection)
  "Called when CONNECTION becomes busy."
  (pbclj-set-connection-busy connection)
  (pbclj-polybar-update))

(defun polybar-clojure-stop-spinner (connection)
  "Called when CONNECTION becomes idle (may be called multiple times)."
  (pbclj-set-connection-idle connection)
  (pbclj-polybar-update))

;; ---------------------------------------------------------
;; nrepl integration
;; ---------------------------------------------------------

(defun nrepl-send-request--pbclj-around (fn request callback connection &optional tooling)
  "Around advice for wrapping nrepl-send-request.
FN is the unwrapped nrepl-send-request function.
REQUEST CALLBACK CONNECTION and TOOLING have the same meaning as nrepl-send-request."
  (lexical-let ((callback-fn callback)
                (conn connection))
    (polybar-clojure-start-spinner conn)
    (funcall fn request (lambda (response)
                          (funcall callback-fn response)
                          (polybar-clojure-stop-spinner conn))
             conn
             tooling)))

;; ---------------------------------------------------------
;; Minor mode
;; ---------------------------------------------------------

(defun pbclj-turn-on ()
  (advice-add 'nrepl-send-request :around #'nrepl-send-request--pbclj-around)
  (add-hook 'buffer-list-update-hook #'pbclj--on-buffer-change)
  (add-hook 'cider-connected-hook #'pbclj-polybar-update)
  (add-hook 'cider-disconnected-hook #'pbclj-polybar-update))

(defun pbclj-turn-off ()
  (mapcar #'polybar-clojure-stop-spinner (pbclj--connections))
  (advice-remove 'nrepl-send-request #'nrepl-send-request--pbclj-around)
  (remove-hook 'buffer-list-update-hook #'pbclj--on-buffer-change)
  (remove-hook 'cider-connected-hook #'pbclj-polybar-update)
  (remove-hook 'cider-disconnected-hook #'pbclj-polybar-update)
  (pbclj-polybar-update))

(pbclj-turn-on)

(provide 'polybar-clj)
