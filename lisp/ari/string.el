;; ari/string.el - String utilities.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(defun ari-string:upper-camelcase (str)
  "Convert argument to upper-capitalized form and return that."
  (replace-regexp-in-string "-" "" (capitalize str)))

(provide 'ari/string)
;; ari/string.el ends here
