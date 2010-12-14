;; ari.el - Common functions or macros in ari.el

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(require 'cl)
(require 'ari/seq)

(defvar ari-version 0.1)

(defun ari:%g!-symbol-p (s)
  "Returns whether a symbol starts with G!"
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (downcase (substring (symbol-name s) 0 2)) "g!")))

(defun ari:%o!-symbol-p (s)
  "Returns whether a symbol starts with O!"
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (downcase (substring (symbol-name s) 0 2)) "o!")))

(defun ari:%o!-symbol-to-g!-symbol (s)
  "Convert a o!-symbol to a g!-symbol and return that."
  (make-symbol (concat "g!" (substring (symbol-name s) 2))))

(defmacro ari:defmacro/g! (name args &rest body)
  (declare (indent defun))
  "`defmacro` with auto-gensym."
  (let ((symbs (remove-duplicates
                (remove-if-not #'ari:%g!-symbol-p
                               (ari-seq:flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(substring
                               (symbol-name s)
                               0 2))))
              symbs)
         ,@body))))

(defmacro ari:defmacro* (name args &rest body)
  (declare (indent defun))
  "`defmacro` with auto-gensym and once-only."
  (let* ((os (remove-if-not #'ari:%o!-symbol-p args))
         (gs (mapcar #'ari:%o!-symbol-to-g!-symbol os)))
    `(ari:defmacro/g! ,name ,args
       `(let ,(mapcar* #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

(defmacro ari:with-gensyms (names &rest body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro ari:once-only (names &rest body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro ari:aif (test then &optional else)
  (declare (indent 2))
  `(let ((it ,test))
     (if it ,then ,else)))

(provide 'ari)
;; ari ends here
