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

(defgroup polybar-clj nil
  "Manage polybar-clj customization options."
  :prefix "polybar-clj-"
  :group 'polybar-clj)

(defcustom polybar-clj-color-not-current
  "#4a4e4f"
  "Colour to display when connection is not used by the current buffer."
  :type 'color)

(defcustom polybar-clj-color-busy
  "#d87e17"
  "Colour to display when connection is busy."
  :type 'color)

(defcustom polybar-clj-color-current-idle "#839496"
  "Colour to display when connection is current and idle."
  :type 'color)

(defcustom polybar-clj-connection-format-string "%%{F%s}%s%%{F-}"
  "Format string for polybar color and display name."
  :type 'string)

(defcustom polybar-clj-polybar-msg "polybar-msg hook cider-clj 1"
  "The message to send to polybar to request display update."
  :type 'string)

(defcustom polybar-clj-separator-character "|"
  "Separator character between repl instances."
  :type 'character)

(defcustom polybar-clj-separator-color "#4a4e4f"
  "Colour of separator character between repl instances."
  :type 'color)

(defcustom polybar-clj-connection-name-patterns
  '((".*" . polybar-clj-default-project-name))
  "Alist of regular expression and replacement to apply to the connection name.

If CDR is nil then return the project directory name."
  :type '(alist :key-type string)
  :options '((".*" . polybar-clj-default-project-name)))

(defun polybar-clj-separator ()
  "Return the polybar separator."
  (format "%%{F%s} %s %%{F-}"
          polybar-clj-separator-color
          polybar-clj-separator-character)
  "Separator between REPL instances.")

;; ---------------------------------------------------------
;; Store connections
;; ---------------------------------------------------------

(defvar polybar-clj-connection-states (make-hash-table)
  "Hash table to store the busy state of connections.")

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

(defun polybar-clj--connection-buffer->connection (connection-buffer)
  "Return sesman connection for CONNECTION-BUFFER."
  (-find (-lambda ((conn-name buffer))
           (equal buffer connection-buffer))
         (polybar-clj--connections)))

(defun polybar-clj--connection->connection-buffer (connection)
  "Return repl buffer for sesman connection CONNECTION."
  (cadr connection))

(defun polybar-clj--current-buffer-connection ()
  "Return connection for the current buffer."
  (polybar-clj--connection-buffer->connection
   (cider-current-repl-buffer)))

(defun polybar-clj--update-current ()
  "Ensure the connection for the current buffer is up to date and call \
POLYBAR-CLJ-POLYBAR-UPDATE on connection change.

This is usually called as a hook following an event that \
could change the current connection.  Does not record the minibuffer as a buffer change."
  (let ((buffer-connection (polybar-clj--current-buffer-connection)))
    (unless (or (equal buffer-connection polybar-clj--current-connection)
                (window-minibuffer-p))
      (setq polybar-clj--current-connection buffer-connection)
      (polybar-clj-polybar-update))))

;; ---------------------------------------------------------
;; Connection Display
;; ---------------------------------------------------------

(defun polybar-clj--connection-current-p (connection)
  "Return non-nil if CONNECTION is the connection in the current buffer."
  (equal polybar-clj--current-connection connection))

(defun polybar-clj--connection-name-find-matcher (connection-name)
  "Find the first machine pattern in POLYBAR-CLJ-CONNECTION-NAME-PATTERNS  \
for a given CONNECTION-NAME."
  (-find (-lambda ((pattern-re . action))
           (string-match-p pattern-re connection-name))
         polybar-clj-connection-name-patterns))

(defun polybar-clj-connection-display-name (connection)
  "Return current display name for CONNECTION."
  (-let ((action (cdr (polybar-clj--connection-name-find-matcher (car connection)))))
    (if (stringp action)
        action
      (funcall action connection))))

(defun polybar-clj-connection-display-color (connection)
  "Return current display colour for CONNECTION."
  (cond ((polybar-clj-connection-busy-p connection) polybar-clj-color-busy)
        ((polybar-clj--connection-current-p connection) polybar-clj-color-current-idle)
        (polybar-clj-color-not-current)))
(defun polybar-clj-connection-string (connection)
  "Format the display STRING for CONNECTION."
  (format polybar-clj-connection-format-string
          (polybar-clj-connection-display-color connection)
          (polybar-clj-connection-display-name connection)))

;; ---------------------------------------------------------
;; Status string printing
;; ---------------------------------------------------------

(defun polybar-clj--connections ()
  "Return list of connections."
  (sesman-sessions 'CIDER))

(defun polybar-clj-status-string ()
  "Return status string displayed by polybar."
  (if polybar-clj-mode
      (string-join (mapcar #'polybar-clj-connection-string
                           (polybar-clj--connections))
                   (polybar-clj-separator))
    "polybar-clj-mode disabled"))

(defun polybar-clj--project-dir (connection)
  "Return project root directory for CONNECTION or nil when no project."
  (when-let ((repl-buffer (polybar-clj--connection->connection-buffer connection)))
    (expand-file-name (with-current-buffer repl-buffer (sesman-project 'CIDER)))))

(defun polybar-clj-default-project-name (connection)
  "Pick the default project name for connection in CONNECTION \
as the name of the sesman project root directory."
  (if-let (project (polybar-clj--project-dir connection))
      (file-name-nondirectory (directory-file-name project))
    (buffer-name (polybar-clj--connection->connection-buffer connection))))

;; ---------------------------------------------------------
;; Polybar Connections
;; ---------------------------------------------------------

(defun polybar-clj-polybar-update ()
  "Force polybar to update the cider component."
  (interactive)
  (start-process-shell-command "polybar-msg" nil polybar-clj-polybar-msg))

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

(defun nrepl-send-request--polybar-clj-around (fn request callback connection-buffer &optional tooling)
  "Around advice for wrapping nrepl-send-request. \
FN is the unwrapped nrepl-send-request function. \
REQUEST CALLBACK CONNECTION-BUFFER and TOOLING have the same \
meaning as nrepl-send-request."
  (lexical-let ((callback-fn callback)
                (conn (polybar-clj--connection-buffer->connection connection-buffer)))
    (polybar-clojure-start-spinner conn)
    (funcall fn request (lambda (response)
                          (funcall callback-fn response)
                          (polybar-clojure-stop-spinner conn))
             connection-buffer
             tooling)))

;; ---------------------------------------------------------
;; Swap sessions in this buffer
;; ---------------------------------------------------------

(defun polybar-clj--next-sesman-session ()
  "Get the next sesman session in cyclic order."
  (let ((sessions (polybar-clj--connections)))
    (cadr (-drop-while
           (lambda (session)
             (not (equal polybar-clj--current-connection session)))
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
  (polybar-clj--update-current)
  ;; this is to ensure that the polybar disabled message
  ;; disappears when polybar mode is turned on.
  (polybar-clj-polybar-update))

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
