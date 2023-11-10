;;; diranger-mode.el --- Some Dired Improvements -*- lexical-binding: t -*-

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
(require 'diranger-motion)
(require 'diranger-operation)

(defvar diranger-mode-map (make-sparse-keymap))

(defvar diranger-mode)
(define-derived-mode diranger-mode nil
  "Diranger"
  "Major mode emulating terminal file managers.

\\{diranger-mode-map}"
  :group 'diranger
  (setq-local cursor-type nil)
  (use-local-map diranger-mode-map))

(defun diranger-start (&optional filename)
  "Start diranger either using `default-directory` or passed FILENAME."
  (interactive)
  (let ((entry (or filename default-directory)))
    (setq *diranger-focused-entry* entry)
    (when entry
      (hl-line-mode)
      (diranger-refresh-layout entry)
      (diranger-mode)
      (let ((selected-entry (format
			     "%s%s"
			     entry (diranger-directory-item-at-point))))
	(save-window-excursion
	  (diranger-preview-buffer entry selected-entry))))))

(evil-define-key 'normal diranger-mode-map
  "C" #'dirnager-copy
  "D" #'diranger-delete
  "f" #'diranger-jump
  "gr" #'diranger-motion-refresh
  "h" #'diranger-out
  "j" #'diranger-forward-line
  "k" #'diranger-backward-line
  "l" #'diranger-into
  "mx" #'diranger-mark-delete
  "mu" #'diranger-unset-mark-at-point
  "mU" #'diranger-unset-all-marks
  "pp" #'diranger-paste
  "pr" #'diranger-paste-from-ring
  "q" #'diranger-quit
  "R" #'diranger-rename
  "x" #'diranger-execute-marks
  "yy" #'diranger-yank
  "yx" #'diranger-cut
  (kbd "RET") #'diranger-into)

(provide 'diranger-mode)
;;; diranger-mode.el ends here
