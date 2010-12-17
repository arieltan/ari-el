;; ari-install.el - Installer for ari.el.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(require 'auto-install)

(defvar ari-install:*ari-repository-uri*
    "http://github.com/arielnetworks/ari-el/raw/master/lisp/")
(defvar ari-install:*ari-file-names*
    '("ari-cursor.el"
      "ari-debug.el"
      "ari-fn.el"
      "ari-net.el"
      "ari-seq.el"
      "ari-string.el"
      "ari-symbol.el"
      "ari-util.el"
      "ari-ext-yasnippet.el"
      "ari.el"))

(add-to-list 'auto-install-batch-list
             `("ari.el"
               nil nil
               ,(mapcar #'(lambda (name)
                            (concat ari-install:*ari-repository-uri* name))
                        ari-install:*ari-file-names*)))

(auto-install-batch "ari.el")
;; ari-install.el ends here
