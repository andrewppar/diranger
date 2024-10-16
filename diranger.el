;;; diranger.el --- Some Dired Improvements -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 29 August 2023
;; Homepage: N/A
;; Keywords: dired
;; Package-Requires: ((emacs "28") (dash "2.19.1") (ivy "0.1.7"))
;; SPDX-License-Identifier: MIT
;; Version: 0.1

;;; Commentary:

;; I like ranger, but it's too opinionated about it's bindings.  I've
;; been using this on a local machine for a while but want it in
;; my other Emacs configurations.

;;; Code:

(require 'diranger-mode)

(defun diranger-read-file ()
  "Find a file using Diranger."
  (interactive)
  (ivy-read "Find file: "
	    #'read-file-name-internal
	    :matcher #'counsel--find-file-matcher
	    :initial-input nil
	    :action #'counsel-find-file-action
	    :preselect (counsel--preselect-file)
	    :require-match 'confirm-after-completion
	    :history 'file-name-history
	    :keymap counsel-find-file-map
	    :caller 'diranger-find-file))

;;;###autoload
(defun diranger (&optional filepath)
  "Start a diranger session for optionally FILEPATH.

   If FILEPATH is not provided you'll be prompted for one."
  (interactive)
  (let ((file (or filepath (diranger-read-file))))
    (if (file-directory-p file)
	(diranger-start file)
      (if (equal major-mode 'diranger-mode)
	  (progn
	    (diranger-quit)
	    (find-file file))
	(save-window-excursion
	  (diranger-quit)
	  (find-file file))))))

(provide 'diranger)
;;; diranger.el ends here
