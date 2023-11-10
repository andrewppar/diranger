;;; diranger-core.el --- Some Dired Improvements -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 29 August 2023
;; Homepage: N/A
;; Keywords: dired

;; SPDX-License-Identifier: MIT
;; Version: 0.1

;;; Commentary:

;; Core functions for diranger.  Includes things like: setting up layout,
;; getting information about the filesystem, and managing the preview buffer.

;;; Code:
;; TODO: replace any (format "%s/%s"...) with the right file-... function

(require 'diranger-utils)
(require 'dash)

(defvar *diranger-previous-buffer* "*diranger-previous*")
(defvar *diranger-focused-buffer* "*diranger-focus*")
(defvar *diranger-preview-buffer* "*diranger-preview*")
(defvar *diranger-metadata-buffer* "*diranger-metadata*")
(defvar *diranger-buffers* (list *diranger-previous-buffer*
				 *diranger-focused-buffer*
				 *diranger-preview-buffer*
				 *diranger-metadata-buffer*))
(defvar *diranger-buffer->file-properties* '())
(defvar *diranger-marks* '())

(defmacro with-diranger-buffer (buffer &rest body)
  "Execute BODY in diranger BUFFER."
  (declare (indent 1))
  `(progn
     (save-window-excursion
       (select-window (get-buffer-window ,buffer))
       (read-only-mode -1)
       (erase-buffer)
       (progn ,@body)
       (read-only-mode 1))))

(defmacro do-diranger-marks (spec &rest body)
  "Iterate over the mark cache executing BODY binding SPEC."
  (declare (indent defun))
  (let ((dir-var (car spec))
	(file-var (cadr spec))
	(type-var (caddr spec)))
    `(doalist (,dir-var inner-list *diranger-marks*)
       (doalist (,file-var ,type-var inner-list)
	 (progn ,@body)))))

(defun diranger-record-mark (directory file type)
  "Update the mark map with DIRECTORY FILE and TYPE."
  (setq *diranger-marks*
	(alist-update-in
	 *diranger-marks*
	 (list directory file)
	 (lambda (&rest _) type))))

(defun diranger-clear-marks ()
  "Clear any diranger mark that has been recorded."
  (setq *diranger-marks* nil))

(defun diranger-get-mark (directory file)
  "Get the mark for DIRECTORY and FILE if it exists."
  (alist-get-in *diranger-marks* (list directory file)))

(defun diranger-pop-mark (directory file)
  "Remove the mark for DIRECTORY and FILE if it exists."
  (let ((result '()))
    (doalist (dir inner-list *diranger-marks*)
      (if (equal directory dir)
	  (let ((new-inner-list '()))
	    (doalist (f mark inner-list)
	      (unless (equal f file)
		(push (cons f mark) new-inner-list)))
	    (push (cons dir new-inner-list) result))
	(push (cons dir inner-list) result)))
    (setq *diranger-marks* result)))

(defun diranger-switch-to-focused-buffer ()
  "If `*diranger-focused-buffer*` exists, go to it.

   Otherwise create it and set it's minor modes."
  (if (get-buffer *diranger-focused-buffer*)
      (switch-to-buffer *diranger-focused-buffer*)
    (progn
      (switch-to-buffer *diranger-focused-buffer*))))

(defun buffer-visible-p (buffer-or-string)
  "Check where BUFFER-OR-STRING is in a window."
  (let ((visible-buffers (mapcar #'window-buffer (window-list))))
    (if (bufferp buffer-or-string)
	(filter
	 (lambda (buffer) (equal buffer buffer-or-string))
	 visible-buffers)
      (filter
       (lambda (buffer) (equal (buffer-name buffer) buffer-or-string))
       visible-buffers))))

(defun diranger-visible-p ()
  "Ensure all and only diranger buffers are visible."
  (and (buffer-visible-p *diranger-previous-buffer*)
       (buffer-visible-p *diranger-focused-buffer*)
       (buffer-visible-p *diranger-preview-buffer*)
       (-every-p
	(lambda (buffer-name)
	  (member buffer-name *diranger-buffers*))
	(mapcar
	 (-compose #'buffer-name #'window-buffer)
	 (window-list)))))

(defun diranger-dired-dir-linep (line)
  "Predicate to check if LINE is a Dired directory."
  (string-prefix-p "  d" line))

(defun diranger-dired-file-linep (line)
  "Predicate to check if LINE is Dired file."
  (string-prefix-p "  -" line))

(defun diranger-dired-link-linep (line)
  "Predicate to check if LINE is Dired link."
  (string-prefix-p "  l" line))

(defun diranger-dired-path-name (line)
  "The name of the path at LINE."
  (let ((line-list (split-string line " " t "\n")))
    (string-join (-drop 8 line-list) " ")))

(defun diranger-dired-metadata (line)
  "Get Dired metadata for LINE."
  (-> line
      (split-string " " t "\n")
      reverse
      cdr
      reverse
      (string-join " ")))

(defun diranger-color-for-mark-type (type)
  "Return the hex color for a mark of type TYPE."
  (cond ((eql type :delete) "#C85BFF")
	(t nil)))

(defun diranger-mark-for-mark-type (type)
  "Return the mark for a mark of type TYPE."
  (cond ((eql type :delete) "X")
	(t nil)))

(defun diranger-make-marked-line (path-name mark longest-line)
  "Produce a new line for PATH-NAME with MARK separated by LONGEST-LINE."
  (let ((color (diranger-color-for-mark-type mark))
	(mark-string (diranger-mark-for-mark-type mark))
	(padding (make-string (- longest-line (length path-name)) 32)))
    (propertize
     (format "%s%s %s" path-name padding mark-string)
     'face `(:foreground ,color))))

(defun diranger-process-dired-line-for-preview (full-path line longest-line)
  "Given a Dired LINE process it to be displayed by diranger preview.

   Save any metadata about a LINE under FULL-PATH in
   *diranger-buffer->file-properties*."
  (let* ((path-name (diranger-dired-path-name line))
	 (metadata (diranger-dired-metadata line))
	 (mark (diranger-get-mark full-path path-name)))
    (setq *diranger-buffer->file-properties*
	  (alist-update *diranger-buffer->file-properties*
			(expand-file-name (format "%s/%s" full-path path-name))
			(lambda (&rest _)
			  metadata)))
    (cond
      (mark
       (diranger-make-marked-line path-name mark longest-line))
      ((diranger-dired-dir-linep line)
       (propertize path-name 'face '(:foreground "#5D8FBE")))
      ((diranger-dired-file-linep line)
       path-name)
      ((diranger-dired-link-linep line)
       (propertize path-name 'face '(:foreground "#94BE54" )))
      (t path-name))))

(defun diranger-directory-item-at-point ()
  "Get the current directory item at point."
  (let ((line (buffer-substring-no-properties
	       (line-beginning-position)
	       (line-end-position))))
    (string-trim (car (split-string line " -> ")))))

(defun diranger-show-directory (entry)
  "Insert the results of listing ENTRY into current buffer."
  (let ((full-path (expand-file-name entry)))
    (insert
     (propertize
      (format "%s\n" full-path) 'face '(:foreground "#7AD6FF")))
    (let ((dired-buffer (dired-noselect entry "-lah"))
	  (contents nil)
	  (longest-line 0))
      (with-current-buffer dired-buffer
	(setq contents
	      (-> (point-min)
		  (buffer-substring-no-properties (point-max))
		  (split-string "\n"))
	      longest-line (->> contents
				(mapcar (-compose #'length #'diranger-dired-path-name))
				(apply #'max))))
      (kill-buffer dired-buffer)
      (insert
       (string-join
	(->> contents
	     (-drop 1)
	     (mapcar
	      (lambda (line)
		(diranger-process-dired-line-for-preview
		 full-path line longest-line))))
	"\n"))
      (goto-char (point-min))
      (forward-line 1))))

(defun diranger-preview-show-file (entry)
  "Generate a preview of ENTRY."
  (let ((file-size-mb (-> entry
			  file-attributes
			  file-attribute-size
			  (/ 1000000))))
    (if (< file-size-mb 2)
	(let* ((existing-buffer (get-file-buffer entry))
	       (buffer (or existing-buffer (find-file entry)))
	       (contents nil))
	  (with-current-buffer buffer
	    (setq contents (buffer-substring-no-properties
			    (point-min) (point-max))))
	  (unless existing-buffer
	    (kill-buffer buffer))
	  (insert contents))
      (insert (propertize "File too big to preview"
			  'face '(:foreground "#2080C0"))))))

(defun diranger-preview-show-link (entry)
  "Generate a preview of ENTRY."
  (-> entry
      file-attributes
      file-attribute-type
      diranger-preview-show-file))

(defun diranger-refresh-layout (focused-entry)
  "Refresh the frame for diranger on FOCUSED-ENTRY."
  (delete-other-windows)
  (switch-to-buffer *diranger-previous-buffer*)
  (with-diranger-buffer *diranger-previous-buffer*
    (let ((parent (file-name-parent-directory focused-entry)))
      (if parent
	  (diranger-show-directory parent)
	(insert (format "No parent directory for %s" focused-entry)))))
  (let ((focus-window (split-window-right)))
    (other-window 1)
    (diranger-switch-to-focused-buffer)
    (with-diranger-buffer *diranger-focused-buffer*
      (diranger-show-directory focused-entry)
      (setq default-directory focused-entry))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer *diranger-preview-buffer*)
    (select-window focus-window)
    (balance-windows)
    (let ((metadata-window (split-window-below (- (window-height) 1))))
      (select-window metadata-window)
      (switch-to-buffer *diranger-metadata-buffer*)
      (display-line-numbers-mode -1)
      (select-window focus-window))))

(defun diranger-preview-buffer (focused-entry selected-entry)
  "Update `*diranger-preview-buffer*` with SELECTED-ENTRY in FOCUSED-ENTRY.

  If either `*diranger-preview-buffer*` or *diranger-focused-buffer*`
  are not visible, refocus diranger."
  (if (not (diranger-visible-p))
      (progn
	(diranger-refresh-layout focused-entry)
	(diranger-preview-buffer focused-entry selected-entry))
    (with-diranger-buffer *diranger-preview-buffer*
      (cond ((file-directory-p selected-entry)
	     (diranger-show-directory selected-entry))
	    ((file-symlink-p selected-entry)
	     (diranger-preview-show-link selected-entry))
	    ((file-regular-p selected-entry)
	     (diranger-preview-show-file selected-entry))
	    (t
	     (insert (propertize "No preview for this file type."
			  'face '(:foreground "#2080C0"))))))))

(defun diranger-focused-buffer-add-metadata (selected-entry)
  "Add any metadata about SELECTED-ENTRY to BUFFER."
  (let* ((metadata (alist-get
		    (expand-file-name selected-entry)
		    *diranger-buffer->file-properties*
		    nil nil #'equal)))
    (when metadata
      (cl-destructuring-bind
	    (permissions links user group size &rest time-data)
	  (split-string metadata " " t "\n")
	(let ((perms (propertize permissions
				 'face '(:foreground "#5D8FBE")))
	      (user-group (propertize (format "%s %s" user group)
				      'face '(:foreground "#94BE54")))
	      (timestamp (propertize (string-join time-data " ")
				     'face '(:foreground "#FFC63F"))))
	  (with-diranger-buffer *diranger-metadata-buffer*
	    (insert (string-join (list perms links user-group size timestamp) " "))))))))

(provide 'diranger-core)
;;; diranger-core.el ends here
