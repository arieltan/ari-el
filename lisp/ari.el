;; ari.el - Common functions or macros in ari.el

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(require 'cl)
(require 'ari/seq)

(defvar ari-version 0.1)

(defmacro ari:require (lib &rest body)
  "Require a library safely."
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body t))

(defmacro ari:autoload (func lib &rest body)
  "Autoload a library safely."
  `(when (locate-library ,lib)
     ,@(mapcar (lambda (f) `(autoload ',f ,lib nil t)) func)
     (eval-after-load ,lib
       '(progn
         ,@body)) t))

(defmacro ari:add-hook-fn (name &rest body)
  "Add a hook as a lambda.".
  `(add-hook ,name #'(lambda () ,@body)))

(defmacro ari:global-set-key-fn (key &rest body)
  `(global-set-key ,key #'(lambda () (interactive) ,@body)))

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

(defmacro ari:awhen (test &rest body)
  `(ari:aif ,test (progn ,@body)))

(defmacro ari:aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(ari:aif ,(car args) (ari:aand ,@(cdr args))))))

(defmacro ari:alambda (params &rest body)
  `(labels ((self ,params ,@body))
     #'self))

(ari:defmacro* ari:dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
          (lambda (d)
            `(,(if (eq t (car d))
                   t
                   (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                           g!args
                           `(cdr ,g!args)))))
          ds))))

(defmacro ari:alet (letargs &rest body)
  (declare (indent 2))
  `(let ((this) ,@(ari-seq:group letargs 2))
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(ari:defmacro* ari:acond (&rest clauses)
  (declare (indent 2))
  (unless (null clauses)
    (let ((cl1 (car clauses)))
      `(let (it (,g!sym ,(car cl1)))
         (if ,g!sym
             (let ((it ,g!sym)) ,@(cdr cl1))
             (ari:acond ,@(cdr clauses)))))))

(provide 'ari)
;; ari ends here
