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
      to-list))

  (desc "ari-seq:flatten")
  (expect '(1 2 3 4)
    (ari-seq:flatten '(1 (2 3) 4)))

  (desc "ari-seq:group")
  (expect '((e m) (a c) (s))
    (ari-seq:group '(e m a c s) 2))

  (desc "ari-seq:n..")
  (expect '(3 4 5 6 7)
    (ari-seq:n.. 3 7))

  (desc "ari-seq:1..")
  (expect '(1 2 3 4 5)
    (ari-seq:1.. 5))

  (desc "ari-seq:0..")
  (expect '(0 1 2 3 4)
    (ari-seq:0.. 4)))
