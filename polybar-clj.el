;;; polybar-clj.el --- Display active CIDER connections in polybar -*- lexical-binding: t -*-

;; Copyright © 2021 Mark Dawson <markgdawson@gmail.com>

;; Author: Mark Dawson <markgdawson@gmail.com>
;; URL: https://github.com/markgdawson/polybar-clj-emacs
;; Keywords: project, convenience
;; Version: 0.0.1-snapshot
;; Package-Requires: ((emacs "25.1") dash cider sesman)

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
(require 'cider)
(require 'sesman)

;; ---------------------------------------------------------
;; Store connections
;; ---------------------------------------------------------

(defvar polybar-clj-connection-states (make-hash-table)
  "Hash table to store the busy state of connections.")

(defvar polybar-clj-connection-aliases (make-hash-table)
  "Hash table to store display strings for connections.")

(defun polybar-clj-set-connection-busy (connection)
  "Mark CONNECTION as busy."
  (puthash connection t polybar-clj-connection-states))

(defun polybar-clj-set-connection-idle (connection)
  "Mark CONNECTION as idle."
  (puthash connection nil polybar-clj-connection-states))

(defun polybar-clj-connection-busy-p (connection)
  "Return non-nil if CONNECTION is marked as busy."
  (gethash connection polybar-clj-connection-states))

;; ---------------------------------------------------------
;; Store and update connection for current buffer
;; ---------------------------------------------------------

(defvar polybar-clj--current-connection nil)

(defun polybar-clj--update-current ()
  "Ensure the connection for the current buffer is up to date and call \
POLYBAR-CLJ-POLYBAR-UPDATE on connection change.

This is usually called as a hook following an event that \
could change the current connection.  Does not record the minibuffer as a buffer change."
  (let ((cider-repl-buffer (cider-current-repl-buffer)))
    (unless (or (equal cider-repl-buffer polybar-clj--current-connection)
                (window-minibuffer-p))
      (setq polybar-clj--current-connection cider-repl-buffer)
      (polybar-clj-polybar-update))))

(defun polybar-clj--emphasize-if-current (connection string)
  "Format STRING as current connection when CONNECTION is the current connection."
  (if (polybar-clj--connection-is-current connection)
      (format "%%{F#839496}%s%%{F-}" string)
    (format "%%{F#}%s%%{F-}" string)))

;; ---------------------------------------------------------
;; Connection Display Strings
;; ---------------------------------------------------------
(defcustom polybar-clj-busy-color "#d87e17"
  "Colour to display when connection is busy.")

(defcustom polybar-clj-idle-current-color "#839496"
  "Colour to display when connection is current and idle.")

(defcustom polybar-clj-idle-other-color "#4a4e4f"
  "Colour to display when connection is not current and idle.")

(defun polybar-clj--connection-is-current (connection)
  "Return non-nil if CONNECTION is the connection in the current buffer."
  (equal polybar-clj--current-connection connection))

(defun polybar-clj-connection-string (connection)
  "Format the display STRING for CONNECTION."
  (let* ((connection-name (buffer-name connection))
         (color (cond ((polybar-clj-connection-busy-p connection) polybar-clj-busy-color)
                      ((polybar-clj--connection-is-current connection) polybar-clj-idle-current-color)
                      (polybar-clj-idle-other-color)))
         (string (cond ((string-match "repvault-connect" connection-name) "RC")
                       ((string-match "repvault-backend" connection-name) "RVB")
                       ((string-match "repvault-test" connection-name) "RT")
                       ((string-match "repvault-graphql-gateway" connection-name) "RGG")
                       ((string-match "document-index-server" connection-name) "DIS")
                       ((polybar-clj-default-project-name connection)))))
    (format "%%{F%s}%s%%{F-}" color string)))

;; ---------------------------------------------------------
;; Status string printing
;; ---------------------------------------------------------
(defcustom polybar-clj-separator "%{F#4a4e4f} | %{F-}"
  "Separator between REPL instances.")

(defun polybar-clj--connections ()
  "Return list of connections as passed to nrepl."
  (mapcar #'cadr (sesman-sessions 'CIDER)))

(defun polybar-clj-status-string ()
  "Return status string displayed by polybar."
  (string-join (mapcar #'polybar-clj-connection-string
                       (polybar-clj--connections))
               polybar-clj-separator))

(defun polybar-clj-default-project-name (buffer)
  "Pick the default project name for connection in BUFFER \
as the name ofcthe session root directory."
  (if-let (project (with-current-buffer buffer (sesman-project 'CIDER)))
      (file-name-nondirectory (directory-file-name (file-name-directory project)))
    (buffer-name buffer)))

;; ---------------------------------------------------------
;; Polybar Connections
;; ---------------------------------------------------------

(defun polybar-clj-polybar-update ()
  "Forced polybar to update the cider component."
  (interactive)
  (mgd/polybar-send-hook "cider" 1))

(defun polybar-clojure-start-spinner (connection)
  "Called when CONNECTION becomes busy."
  (polybar-clj-set-connection-busy connection)
  (polybar-clj-polybar-update))

(defun polybar-clojure-stop-spinner (connection)
  "Called when CONNECTION becomes idle (may be called multiple times)."
  (polybar-clj-set-connection-idle connection)
  (polybar-clj-polybar-update))

;; ---------------------------------------------------------
;; nrepl integration
;; ---------------------------------------------------------

(defun nrepl-send-request--polybar-clj-around (fn request callback connection &optional tooling)
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
;; Swap sessions in this buffer
;; ---------------------------------------------------------

(defun polybar-clj--next-sesman-session ()
  "Get the next sesman session in cyclic order."
  (let ((sessions (sesman-sessions 'CIDER)))
    (cadr (-drop-while
           (lambda (session)
             (not (equal polybar-clj--current-connection (cadr session))))
           (append sessions sessions)))))

(defun polybar-clj-cycle-sessions-buffer ()
  "Cycle through CIDER sessions for current buffer."
  (interactive)
  (sesman-link-session 'CIDER (polybar-clj--next-sesman-session) 'buffer (current-buffer)))

(defun polybar-clj-cycle-sessions-project ()
  "Cycle through CIDER sessions for current projects."
  (interactive)
  (let ((next-session (polybar-clj--next-sesman-session)))
    ;; note that unlinking can change the current session/connection.
    (sesman-unlink-all-buffer)
    (sesman-link-session 'CIDER next-session 'project )))

;; ---------------------------------------------------------
;; Minor mode
;; ---------------------------------------------------------

(defun polybar-clj-turn-on ()
  "Turn on polybar-clj-mode."
  (advice-add 'nrepl-send-request :around #'nrepl-send-request--polybar-clj-around)
  (add-hook 'buffer-list-update-hook #'polybar-clj--update-current)
  (add-hook 'cider-connected-hook #'polybar-clj-polybar-update)
  (add-hook 'cider-disconnected-hook #'polybar-clj-polybar-update)
  (add-hook 'sesman-post-command-hook #'polybar-clj--update-current)
  ;; update polybar when mode turned on
  (polybar-clj--update-current))

(defun polybar-clj-turn-off ()
  "Turn off polybar-clj-mode."
  (mapcar #'polybar-clojure-stop-spinner (polybar-clj--connections))
  (advice-remove 'nrepl-send-request #'nrepl-send-request--polybar-clj-around)
  (remove-hook 'buffer-list-update-hook #'polybar-clj--update-current)
  (remove-hook 'cider-connected-hook #'polybar-clj-polybar-update)
  (remove-hook 'cider-disconnected-hook #'polybar-clj-polybar-update)
  (remove-hook 'sesman-post-command-hook #'polybar-clj--update-current)
  (polybar-clj-polybar-update))

;;;###autoload
(define-minor-mode polybar-clj-mode
  "Toggle polybar-clj-mode."
  :lighter    " pb-clj"
  :init-value nil
  :global     t
  :group      'polybar-clj
  (cond
   (noninteractive (setq polybar-clj-mode nil))
   (polybar-clj-mode (polybar-clj-turn-on))
   (t (polybar-clj-turn-off))))

(provide 'polybar-clj)
