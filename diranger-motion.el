;;; diranger-core.el --- Some Dired Improvements -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 29 August 2023
;; Homepage: N/A
;; Keywords: dired
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

;;; Commentary:

;; I like ranger, but its too opinionated about bindings :(

;;; Code:
(require 'diranger-core)
(require 'ivy)
(defvar *diranger-focused-entry* nil)

(defun kill-buffer-safe (buffer-name)
  "Kill buffer with BUFFER-NAME without erroring if it does not exist."
  (when-let ((buffer (get-buffer buffer-name)))
    (kill-buffer buffer)))

(defun diranger-quit ()
  "Kill all diranger buffers."
  (interactive)
  (delete-other-windows)
  (mapcar #'kill-buffer *diranger-buffers*)
  (setq *diranger-focused-entry* nil))

(defmacro defun-diranger-motion (name comment &rest body)
  "Create a diranger motion function with NAME and COMMENT to execute BODY."
  `(defun ,name ()
     ,comment
     (interactive)
     ;;(condition-case err
	 (progn ,@body)

      ;; (error (progn
      ;;		(message "Diranger hit error \"%s\", exiting..."
      ;;			 (error-message-string err))
	 ;;		(diranger-quit))))
	 ))

(defun diranger-set-focused-entry (entry)
  "Set `*diranger-focused-entry* to ENTRY."
  (if (string-suffix-p "/" entry)
      (setq *diranger-focused-entry* entry)
    (setq *diranger-focused-entry* (format "%s/" entry))))

(defun diranger-get-focused-entry ()
  "Get the focused entry."
  *diranger-focused-entry*)

(defun diranger-selected-entry ()
  "Get the current diranger selected entry."
  (format "%s/%s"
	  *diranger-focused-entry*
	  (diranger-directory-item-at-point)))

;; TODO: Maybe this should just be diranger-refresh in core
(defun diranger-motion-refresh ()
  "Refresh diranger after some motion event."
  (interactive)
  (let ((selected-entry (diranger-selected-entry)))
    (save-window-excursion
      (diranger-preview-buffer *diranger-focused-entry* selected-entry))
    (diranger-focused-buffer-add-metadata selected-entry)))

(defun diranger-vertical-motion (direction)
  "Move diranger selection in DIRECTION.

  Use `-n` for up n lines and `n` for down n lines."
  (forward-line direction)
  (diranger-motion-refresh))

(defun-diranger-motion diranger-forward-line
    "Move diranger selection down one line."
  (diranger-vertical-motion 1))

(defun-diranger-motion diranger-backward-line
    "Move diranger selection up one line."
  (diranger-vertical-motion -1))

(defun-diranger-motion diranger-into
    "Move diranger into the selection."
  (let ((selected-entry (diranger-selected-entry)))
    (cond ((file-directory-p selected-entry)
	   (progn
	     (diranger-set-focused-entry selected-entry)
	     (diranger-refresh-layout selected-entry)))
	  ((file-symlink-p selected-entry)
	   (let ((file (-> selected-entry
			   file-attributes
			   file-attribute-type)))
	     (diranger-quit)
	     (find-file file)))
	  (t
	   (progn
	     (diranger-quit)
	     (find-file selected-entry))))))

(defun-diranger-motion diranger-out
    "Move diranger up a directory."
  (let ((new-focus (file-name-parent-directory *diranger-focused-entry*)))
    (message new-focus)
    (if new-focus
	(progn
	  (diranger-set-focused-entry new-focus)
	  (diranger-refresh-layout new-focus))
      (progn
	(message "No parent directory for %s" *diranger-focused-entry*)
	(diranger-motion-refresh)))))

(defun-diranger-motion diranger-jump
    "Interactively select entry to move to."
  (let* ((start (point-min))
	 (end (point-max))
	 (lines (split-string
		 (buffer-substring-no-properties start end)
		 "\n"))
	 (entries (->> lines
		       (drop 1)
		       (filter (lambda (line) (not (equal line ""))))))
	 (entry (ivy-completing-read "Jump to: " entries nil t))
	 (line-num (+ (cl-position entry entries :test #'equal) 2)))
    (goto-line line-num)
    (diranger-motion-refresh)))


(provide 'diranger-motion)
;;; diranger-motion.el ends here
