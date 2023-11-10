;;; diranger-utils.el --- Some Dired Improvements -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 29 August 2023
;; Homepage: N/A
;; Keywords: dired
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

;;; Commentary:

;; These are functions and macros I like to use.  I should probably have another
;; library for them.

;;; Code:

;;; Association Lists
(defmacro doalist (spec &rest body)
  "Iterate over SPEC `(key value alist)` executing BODY."
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 3 (length spec) 4)
    (signal 'wrong-number-of-arguments (list '(3 . 4) (length spec))))
  (let ((keys (gensym))
        (key  (gensym)))
    `(let ((,keys (mapcar #'car ,(caddr spec))))
       (dolist (,key ,keys)
         (let ((,(car spec) ,key)
               (,(cadr spec) (alist-get
                              ,key ,(caddr spec) nil nil #'equal)))
           ,@body)))))

(defun alist-get-in (alist keys)
  "Get the value of ALIST at nested KEYS."
  (let ((value (alist-get (car keys) alist nil nil #'equal)))
    (if (= (length keys) 1)
	value
      (alist-get-in value (cdr keys)))))

(defun alist-update (alist key update-function &rest args)
  "Change the value of ALIST at KEY with UPDATE-FUNCTION applied to ARGS."
  (let* ((old-value (alist-get key alist nil t #'equal))
	 (new-alist (assoc-delete-all key alist #'equal))
	 (new-value (apply update-function (cons old-value args))))
    (cons (cons key new-value) new-alist)))

(defun alist-update-in (alist keys update-function &rest args)
  "Change the vlaue of ALIST at nested KEYS with UPDATE-FUNCTION applied to ARGS."
  (let ((key (car keys)))
    (if (= (length keys) 1)
	(apply #'alist-update alist key  update-function args)
      (let* ((old-value (alist-get key alist nil t #'equal))
	     (new-alist (assoc-delete-all key alist #'equal))
	     (new-value (apply #'alist-update-in old-value (cdr keys) update-function args)))
	(cons (cons key new-value) new-alist)))))

(provide 'diranger-utils)
;;; diranger-utils.el ends here
