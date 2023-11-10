;;; diranger-operation.el --- Some Dired Improvements -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 29 August 2023
;; Homepage: N/A
;; Keywords: dired
;; SPDX-License-Identifier: MIT
;; Version: 0.1

;;; Commentary:

;; Functions for doing operations on the filesystem.

;;; Code:

(require 'diranger-core)
(require 'diranger-motion)
(require 'diranger-utils)

(defvar *diranger-kill-ring* '())

(defun diranger-kill-ring-get (old-path)
  "Get the path for OLD-PATH from diranger's kill ring."
  (alist-get old-path *diranger-kill-ring* nil nil #'equal))

(defun diranger-kill-ring-entry-exists-p (path)
  "Predicate indicating whether a PATH is already on the kill ring."
  (diranger-kill-ring-get path))

(defun kill-ring-remove-last ()
  "Remove the oldest item from diranger's kill ring."
  (setq *diranger-kill-ring*
	(funcall (-compose #'reverse #'cdr #'reverse) *diranger-kill-ring*)))

(defun diranger-kill-ring-add (old-path new-path)
  "Add an binding from OLD-PATH to NEW-PATH to diranger's kill ring."
  (push (cons old-path new-path) *diranger-kill-ring*)
  (when (and (<= 20 (length *diranger-kill-ring*))
	     (not (diranger-queue-entry-exists-p old-path)))
    (kill-ring-remove-last))
  (kill-new new-path))

(defun diranger-kill-ring-update (key new-value)
  "Update diranger's kill ring with NEW-VALUE at KEY."
  (let ((result '())
	(found? nil))
    (doalist (k v *diranger-kill-ring)
      (if (equal key k)
	  (progn
	    (push (cons key new-value) result)
	    (setq found? t))
	(push (cons k v) result)))
    (setq *diranger-kill-ring* (reverse result))
    (kill-new new-value)
    (unless found?
      (diranger-kill-ring-add key new-value))))

(defun diranger-kill-ring-list ()
  "List the paths on diranger's kill ring."
  *diranger-kill-ring*)

(defun diranger-kill-ring-current-item ()
  "Get the current item from diranger's kill ring."
  (car *diranger-kill-ring*))

(defmacro defun-diranger-operation (name args comment &rest body)
  "Create a function with NAME, ARGS, COMMENT, and BODY."
  (let ((current-line (gensym)))
    `(defun ,name ,args
       ,comment
       (interactive)
       (let ((,current-line (line-number-at-pos)))
	 (progn ,@body)
	 (diranger-refresh-layout *diranger-focused-entry*)
	 (goto-line ,current-line)))))

(defun-diranger-operation diranger-mark-operation-at-point (operation-fn)
  "Complete OPERATION-FN at the current line for a mark.

   OPERATION-FN is expected to take three arguments for the
   current file and directory."
  (let* ((file (diranger-directory-item-at-point))
	 (dir  (expand-file-name (diranger-get-focused-entry))))
    (funcall operation-fn dir file)))

(defun diranger-set-mark-at-point (type)
  "Set mark of TYPE on the item at point."
  (diranger-mark-operation-at-point
   (lambda (file dir)
     (diranger-record-mark file dir type))))

(defun diranger-unset-mark-at-point ()
  "Remove mark from line.  If it's not marked do nothing."
  (interactive)
  (diranger-mark-operation-at-point
   (lambda (directory file)
     (let ((unmarked-file (string-trim (substring file 0 -2))))
       (diranger-pop-mark directory unmarked-file)))))

(defun diranger-unset-all-marks ()
  "Unset any recorded diranger mark."
  (interactive)
  (diranger-mark-operation-at-point
   (lambda (_ _)
     (diranger-clear-marks))))

(defun diranger-mark-delete ()
  "Mark the current line for deletion."
  (interactive)
  (diranger-set-mark-at-point :delete))

(defun diranger-delete-item (item)
  "Delete the ITEM."
  (if (file-directory-p item)
      (delete-directory item t)
    (delete-file item)))

(defun diranger-execute-marks ()
  "Execute any existing diranger mark."
  (interactive)
  (do-diranger-marks (dir file op)
    (let ((file (expand-file-name (file-name-concat dir file))))
      (cl-case op
	(:delete
	 (progn
	   (diranger-delete-item file)
	   (message (format "Deleted %s" file)))))))
  (diranger-clear-marks)
  (diranger-refresh-layout *diranger-focused-entry*))

(defun diranger-yank ()
  "Yank the item at path to diranger's kill ring."
  (interactive)
  (let ((entry (expand-file-name (diranger-selected-entry))))
    (diranger-kill-ring-add entry entry)
    (message (format "Yanked %s" entry))))

(defun-diranger-operation diranger-paste-internal (original from)
  "Paste the what  was located at ORIGINAL and is located at FROM to current dir."
  (let ((to (expand-file-name *diranger-focused-entry*)))
    (if (file-directory-p from)
	(let ((to-name (->> from
			    directory-file-name
			    file-name-base
			    (file-name-concat to)
			    expand-file-name)))
	  (copy-directory from to-name)
	  (message (format "Copied %s to %s" original to-name)))
      (let ((to-name (file-name-concat to (file-name-nondirectory from))))
	(copy-file from to-name)
	(message (format "Copied %s to %s" original to-name))))))

(defun-diranger-operation diranger-paste ()
  "Copy the last yanked entry to the current dir."
  (interactive)
  (cl-destructuring-bind (original . from)
      (diranger-kill-ring-current-item)
    (diranger-paste-internal original from)))

(defun diranger-paste-from-ring ()
  "Prompt the user for an item to paste and copy it to the current dir."
  (let* ((options (mapcar #'car (diranger-kill-ring-list)))
	 (original (ivy-completing-read "Select an entry to paste: "
					options nil t))
	 (from (diranger-kill-ring-get original)))
    (diranger-paste-internal original from)))

(defun-diranger-operation diranger-cut ()
  "Remove the file at point, saving a temporary copy."
  (interactive)
  (let* ((item (expand-file-name (diranger-selected-entry)))
	 (file-name (file-name-nondirectory item))
	 (temp-file (file-name-concat "/tmp" file-name)))
    (diranger-kill-ring-update item temp-file)
    (if (file-directory-p item)
	(progn
	  (copy-directory item temp-file)
	  (delete-directory item t))
      (progn
	(copy-file item temp-file t)
	(delete-file item)))))


;; TODO: for copy and rename maybe also support a batch case where the
;; TO is specified by a regex? seems like a feature I would never use.
(defun-diranger-operation diranger-copy ()
  "Diranger: Copy one file to another."
  (interactive)
  (let ((from (expand-file-name (diranger-selected-entry)))
	(to   (find-file-read-args "Copy to: " nil)))
    (cond ((file-directory-p from)
	   (copy-directory from to))
	  (t
	   (copy-file from to t)))))

(defun-diranger-operation diranger-rename ()
  "Diranger: Rename one file to another."
  (interactive)
  (let ((from (expand-file-name (diranger-selected-entry)))
	(to (car (find-file-read-args "Rename to: " nil))))
    (cond ((file-directory-p from)
	   (progn
	     (copy-directory from to)
	     (delete-directory from )))
	  (t
	   (progn
	     (copy-file from to t)
	     (delete-file from))))))

(defun-diranger-operation diranger-delete ()
  "Delete the entry at point."
  (interactive)
  (let* ((to-delete (expand-file-name (diranger-selected-entry)))
	 (delete? (y-or-n-p
		   (format "Would you like to delete %s? " to-delete))))
    (when delete?
      (diranger-delete-item to-delete))))

(defun-diranger-operation diranger-refresh ()
  "Refresh the current focused buffer.")

(provide 'diranger-operation)
;;; diranger-operation.el ends here
