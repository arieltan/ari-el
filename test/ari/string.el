;; Tests for ari-string.el

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(require 'el-expectations)
(require 'ari-string)

(expectations
  (desc "ari-string:upper-camelcase")
  (expect "ArielNetworksInc"
    (ari-string:upper-camelcase "ariel-networks-inc")))
