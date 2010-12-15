;; ari/net.el - Utilities about Network.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(defun ari-net:machine-ip-address (dev)
  "Return IP address of a network device."
  (aif (network-interface-info dev)
       (format-network-address (car it) t)))

(provide 'ari/net)
;; ari/net.el ends here
