;; ari/debug.el - Debug Utilities.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(defun ari-debug:macroexpand-1 (form)
  "Expand macro once."
  (let ((fn (ignore-errors (symbol-function (car form)))))
    (if (and fn (listp fn))
        (apply (cdr fn) (cdr form))
        form)))

(provide 'ari/debug)
;; ari ends here
