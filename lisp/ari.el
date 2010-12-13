;; ari.el - Common functions or macros in ari.el

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(require 'cl)
(require 'ari/seq)

(defvar ari-version 0.1)

(defun ari:%sharp-symbol-p (s)
  "Returns whether a symbol ends with #"
  (when (symbolp s)
    (let* ((str (symbol-name s))
           (len (length str)))
      (string= (substring str (1- len) len) "#"))))

(defun ari:%percent-symbol-p (s)
  "Returns whether a symbol ends with %"
  (when (symbolp s)
    (let* ((str (symbol-name s))
           (len (length str)))
      (string= (substring str (1- len) len) "%"))))

(defun ari:%percent-symbol-to-sharp-symbol (s)
  "Convert a percent-ended symbol to a sharp-ended symbol and return that."
  (let ((str (symbol-name s)))
    (make-symbol
     (concat (substring str 0 (1- (length str))) "#"))))

(defmacro ari:defmacro/g! (name args &body body)
  "`defmacro` with auto-gensym."
  (let ((symbs (remove-duplicates
                (remove-if-not #'ari:%sharp-symbol-p
                               (ari-seq:flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(substring
                               (symbol-name s)
                               2))))
              symbs)
         ,@body))))

(defmacro ari:defmacro* (name args &body body)
  "`defmacro` with auto-gensym and once-only."
  (let* ((os (remove-if-not #'ari:%percent-symbol-p args))
         (gs (mapcar #'ari:%percent-symbol-to-sharp-symbol os)))
    `(ari:defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

(defmacro ari:aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(provide 'ari)
;; ari ends here
