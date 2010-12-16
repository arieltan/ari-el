;; ari-string.el - String utilities.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT.

;;; Code:

(defun ari-string:upper-camelcase (str)
  "Convert argument to upper-capitalized form and return that."
  (replace-regexp-in-string "-" "" (capitalize str)))

(defun ari-string:ensure-string (val)
  "Convert an argument to string and return it."
  (cond
    ((symbolp val) (symbol-name val))
    ((numberp val) (number-to-string val))
    (t val)))

(provide 'ari-string)
;; ari-string.el ends here
