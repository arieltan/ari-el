;; Tests for ari/seq.el

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(require 'el-expectations)
(require 'ariel/seq)

(expectations
  (desc "ari-seq:append-to-list")
  (expect '(r i l e m a c s)
    (let ((to-list '(e m a c s)))
      (ari-seq:append-to-list to-list '(a r i e l))
      to-list))
  (expect '(e m a c s r i l)
    (let ((to-list '(e m a c s)))
      (ari-seq:append-to-list to-list '(a r i e l) t)
      to-list))
  (expect '(r i l "e" "m" "a" "c" "s")
    (let ((to-list '("e" "m" "a" "c" "s")))
      (ari-seq:append-to-list to-list '(a r i e l) nil #'string=)
      to-list)))
