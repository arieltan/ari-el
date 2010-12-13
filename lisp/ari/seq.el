;; ari/seq.el - Sequence utilities.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(require 'cl)

(defun ari-seq:append-to-list (to-list elements &optional append compare-fn)
  "Append elements to to-list."
  (let ((elems-nodup (loop for elem in elements
                            unless (find elem to-list
                                         :test (or compare-fn #'equal))
                              collect elem)))
    (setq to-list
          (if append
              (append to-list elems-nodup)
              (append elems-nodup to-list)))))

(provide 'ari/seq)
;; ari/seq.el ends here
