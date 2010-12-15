;; ari/fn.el - Function Utilities.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(require 'cl)

;; These macros are heavily inspired by f-underscore.

(defmacro ari-fn:f (args &body body)
  "a synonym for LAMBDA"
  `(lambda ,args ,@body))

(defmacro ari-fn:f0 (&body body)
  "a LAMBDA that takes 0 arguments (aka a 'thunk')"
  `(lambda () ,@body))

(defmacro ari-fn:f_ (&body body)
  "a LAMBDA that takes 1 argument: `_'"
  `(lambda (_) ,@body))

(defmacro ari-fn:f_n (&body body)
  "a LAMBDA that takes 1 &REST argument: `_'"
  `(lambda (&rest _) ,@body))

(defmacro ari-fn:f_% (&body body)
  "a LAMBDA that takes 1 &REST that it ignores"
  (let ((% (gensym "ignore")))
    `(lambda (&rest ,%)
       (declare (ignore ,%))
       ,@body)))

;; NOTE: Is this should be here?
(defmacro ari-fn:m (macro-lambda-list &body body)
  "a LAMBDA that has a macro-lambda-list instead of an ordinary lambda-list"
  (let ((% (gensym "macro-lambda-list")))
    (if (and (stringp (car body))
             (cdr body))
        `(lambda (&rest ,%)
           ,(car body)
           (destructuring-bind ,macro-lambda-list ,%
             ,@(cdr body)))
        `(lambda (&rest ,%)
           (destructuring-bind ,macro-lambda-list ,%
             ,@body)))))

(provide 'ari/fn)
;; ari/fn.el ends here
