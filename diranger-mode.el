;;; diranger-mode.el --- Some Dired Improvements -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 29 August 2023
;; Homepage: N/A
;; Keywords: dired
;; SPDX-License-Identifier: MIT
;; Version: 0.1

;;; Commentary:

;; The functionality around the diranger major mode.

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

(defun diranger-check-exit ()
  "If it's time to exit, leave quit diranger."
  (unless (diranger-visible-p)
    (delete-other-windows)
    (diranger-quit)))

(defun diranger-start (&optional filename)
  "Start diranger either using `default-directory` or passed FILENAME."
  (interactive)
  (let ((entry (or filename default-directory)))
    (setq *diranger-focused-entry* entry)
    (when entry
      (hl-line-mode)
      (diranger-refresh-layout entry)
      (diranger-mode)
      (add-hook 'window-configuration-change-hook 'diranger-check-exit)
      (let ((selected-entry (format
			     "%s%s"
			     entry (diranger-directory-item-at-point))))
	(save-window-excursion
	  (diranger-preview-buffer entry selected-entry))))))

(provide 'diranger-mode)
;;; diranger-mode.el ends here
