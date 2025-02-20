;;; org-setq.el --- use Org to store elisp variables -*- lexical-scope: t -*-
;; Copyright (C) 2025 Joshua Barrett
;; Author: Joshua Barrett <jjbarr@ptnote.dev>
;; Created: 20th Feb 2025
;; Keywords: org config
;; Version: 0.1
;; URL: https://github.com/jjbarr/org-setq
;; Package-Requires: ((emacs "29.1") cl-lib (org "9.7"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; This is pretty hacky and I wouldn't lean on it too hard.  It really just
;; exists to solve a problem I had where I wanted some configuration to exist in
;; an org file.
;;
;; Each variable is defined by the contents under a headline with the property
;; :SETQ: and the variable name to set as its value
;;
;; Configuration is expected in the following format:
;; - lists become lists.
;; - The first element of a plain-text paragraph is taken, the rest is ignored.
;;   It is expcted that this will be formatted in some way, and it is an error
;;   for it not to be.
;; - =verbatim= text becomes strings
;; - ~code~ is passed to read-from-string
;; - definition :: lists become alists. The value being defined will be consed
;;   onto the head of what comes after.
;; - the first form in an emacs lisp source block is read and evalled.
;;
;; In order to load configuration from an elisp file, invoke
;; org-setq-load-from-file.  org-setq-bindings-from-file can be invoked to
;; inspect a set of bindings programmatically without setting them.  Needless to
;; say, do not attempt to invoke this on an untrusted file: by its nature, this
;; package performs arbitrary code execution
;;
;; It's also worth noting that this probably doesn't actually require Emacs 29
;; and org 9.7. However, I haven't tested it with lower versions.
;;
;;; Code:

(require 'org-element)
(require 'cl-lib)

;;;###autoload
(defun org-setq-load-from-file (file)
  "Load org-setq variable definitions from FILE."
  (let ((bindings (org-setq-bindings-for-file file)))
    (dolist ((binding bindings))
      (set (car binding) (cdr binding)))))

;;;###autoload
(defun org-setq-bindings-for-file (file)
  "list the variable bindings for FILE"
  (org-with-file-buffer file
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (when-let ((var (org-element-property :SETQ headline nil))
                   (value (org-setq--parse-value
                           (car (cl-remove 'property-drawer
                                           (cddar (org-element-contents
                                                   headline))
                                           :key #'car)))))
          ;; the format of the org-element-contents is (elem-name props
          ;; children) The value we're going to use is the first child that
          ;; ISN'T property-drawer.
          (cons (intern var) value))))))

(defun org-setq--parse-value (obj)
  (cl-case (org-element-type obj t)
    (paragraph
     (org-setq--parse-value (caddr obj)))
    (verbatim (org-element-property :value obj nil))
    (code (car (read-from-string (org-element-property :value obj nil))))
    (src-block
     (unless (equal "emacs-lisp" (org-element-property :language obj nil))
       (error "Unknown Source Language %s"
              (org-element-property :language obj "(none)")))
     (eval (car (read-from-string (org-element-property :value obj nil))) t))
    (plain-list
     (mapcar #'org-setq--parse-value (cddr obj)))
    (item
     (let ((contents (if (not (cdddr obj))
                         (org-setq--parse-value (caddr obj))
                       (mapcan (lambda (chobj)
                                 (let ((val (org-setq--parse-value chobj)))
                                   (if (not (listp val))
                                       (list val)
                                     val)))
                               (cddr obj)))))
       (if-let ((tag (org-element-property :tag obj nil)))
           (cons (org-setq--parse-value (car tag))
                 contents)
         contents)))
    (anonymous (error "Unexpected anonymous node %s" obj))))

(provide 'org-setq)
;;; org-setq.el ends here
