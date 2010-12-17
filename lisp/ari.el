;; ari.el - Common functions or macros in ari.el

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT.

;;; Code:

(require 'cl)
(require 'ari-seq)
(require 'ari-string)
(require 'ari-symbol)

(defvar ari-version 0.1)

(defvar ari:*packages*
    (cons 'ari
          (mapcar #'(lambda (file)
                      (intern (substring file 0 (- (length file) 3))))
                  (directory-files (file-name-directory
                                    (or load-file-name buffer-file-name)) nil
                                    "^ari-.+el$"))))

;; NOTE: Should to raise any warnings?
(defmacro ari:when-require (lib &rest body)
  (declare (indent 1))
  "Require a library safely."
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body t))

;; NOTE: Should to raise any warnings?
(defmacro ari:when-autoloads (fn-lst lib &rest body)
  (declare (indent 1))
  "Autoload a library safely."
  `(when (locate-library ,lib)
     ,@(mapcar (lambda (f) `(autoload ',f ,lib nil t)) fn-lst)
     (eval-after-load ,lib
       '(progn
         ,@body)) t))

(defmacro ari:add-hook-fn (name &rest body)
  "Add a hook as a lambda."
  `(add-hook ,name #'(lambda () ,@body)))

(defmacro ari:global-set-key-fn (key &rest body)
  `(global-set-key ,key #'(lambda () (interactive) ,@body)))

(defmacro ari:defadvice-many (fn-name-list class &rest body)
  "Define advices, having same body forms."
  `(progn
     ,@(mapcar
        (lambda (fn)
          `(defadvice ,fn (,class ,(intern (concat (symbol-name fn) "-" (symbol-name class) "-advice")) activate)
             ,@body)) fn-name-list)))

(defun ari:define-key-many (keymap key-table &optional includes)
  "Batch to define keys."
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
        (define-key keymap key cmd))))
  keymap)

(eval-when-compile
  (defun ari:%g!-symbol-p (s)
    "Returns whether a symbol starts with G!"
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (downcase (substring (symbol-name s) 0 2)) "g!")))

  (defun ari:%o!-symbol-p (s)
    "Returns whether a symbol starts with O!"
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (downcase (substring (symbol-name s) 0 2)) "o!"))))

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
              #'(lambda (s)
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

(defun ari:ari-symbol (symb &optional package-name)
  "Return a qualified symbol of ari-package."
  (loop for pkg in (if package-name
                       (list package-name)
                       ari:*packages*)
        for ari-symb = (ari-symbol:symbol-concat pkg ': symb)
        if (or (boundp ari-symb) (fboundp ari-symb))
          return ari-symb))

(ari:defmacro* ari:with-package (pkg &rest body)
  (declare (indent 2))
  "Allow unqualified symbols in specified package in the body."
  (let ((symbs (remove-duplicates (ari-seq:flatten body))))
    (multiple-value-bind (val fn mac)
        (loop for s in symbs
              for ari-symb = (ari:ari-symbol s pkg)
              with val-lst = nil
              with fn-lst = nil
              with mac-lst = nil
              when ari-symb do
                (cond
                  ((functionp (ignore-errors (symbol-function ari-symb)))
                   (add-to-list 'fn-lst `(,s ,ari-symb)))
                  ((ignore-errors (symbol-function ari-symb))
                   (add-to-list 'mac-lst `(,s ,ari-symb)))
                  (t (add-to-list 'val-lst `(,s ,ari-symb))))
              finally return (values val-lst fn-lst mac-lst))
      `(macrolet ,(loop for (s m) in mac
                        collect `(,s (&rest ,g!args1)
                                     (apply (cdr (symbol-function ',m))
                                            ,g!args1)))
         (flet ,(loop for (s f) in fn
                      collect `(,s (&rest ,g!args2)
                                   (apply (symbol-function ',f) ,g!args2)))
           (let ,(loop for (s v) in val
                       collect `(,s ,v))
             ,@body))))))

(defmacro ari:with-ari-package (&rest body)
  (declare (indent 2))
  "Allow unqualified `ari-*' symbols in the body. This is a magic."
  `(ari:with-package nil ,@body))

(provide 'ari)
;; ari ends here
