;; ari/ext/yasnippet.el - Utilities for YASnippet.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(defvar ari-ext-yasnippet:*oneshot-snippet* nil
  "Store a snippet for a temporaly purpose.")

(defun ari-ext-yasnippet:register-oneshot-snippet (s e)
  "Register a temporaly snippet."
  (interactive "r")
  (setq ari-ext-yasnippet:*oneshot-snippet* (buffer-substring-no-properties s e))
  (delete-region s e)
  (yas/expand-oneshot-snippet)
  (message "%s" (substitute-command-keys "Press \\[yas/expand-oneshot-snippet] to expand.")))

(defun ari-ext-yasnippet:expand-oneshot-snippet ()
  "Expand a temporaly snippet, stored in ari-ext-yasnippet:*oneshot-snippet*."
  (interactive)
  (if (string< "0.6" yas/version)
      (yas/expand-snippet ari-ext-yasnippet:*oneshot-snippet*)
      (yas/expand-snippet (point) (point) ari-ext-yasnippet:*oneshot-snippet*)))

(defun ari-ext-yasnippet:field-current ()
  "Return current field."
  (overlay-get yas/active-field-overlay 'yas/field))

(defun ari-ext-yasnippet:field-current-number ()
  "Return a number of current field."
  (yas/field-number (yas/field-current)))

(defvar ari-ext-yasnippet:%*replace-prev-field-by* ""
  "Store a string, it needs to replace the previous field.")

(defun ari-ext-yasnippet:replace-prev-field (to)
  "Replace the previous field by ari-ext-yasnippet:%*replace-prev-field-by*."
  (setq ari-ext-yasnippet:%*replace-prev-field-by* to)
  (add-hook 'post-command-hook 'ari-ext-yasnippet:replace-prev-field-once 'append 'local)
  "")

(defun ari-ext-yasnippet:replace-prev-field-once ()
  "Replace the previous field once and quit."
  (let ((to ari-ext-yasnippet:%*replace-prev-field-by*)
        (from (yas/field-value (1- (ari-ext-yasnippet:field-current-number)))))
    (search-backward from nil t)
    (replace-match
     (if (functionp to) (funcall to from) to) nil t)
    (yas/next-field))
  (setq ari-ext-yasnippet:%*replace-prev-field-by* nil)
  (remove-hook 'post-command-hook 'ari-ext-yasnippet:replace-prev-field-once 'local))

(provide 'ari/ext/yasnippet)
;; ari/ext/yasnippet.el ends here
