;; ari-symbol.el - Utilities for symbols.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT.

;;; Code:

(defun ari-symbol:symbol-concat (&rest symbols)
  "Concatenate symbols and return that as one symbol."
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))

(provide 'ari-symbol)
;; ari-symbol.el ends here
