;; ari/cursor.el - Utilities to move a cursor.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(defvar ari-cursor:%*last-search-char* nil
  "Store a character last searched.")
(defvar ari-cursor:%*last-search-direction* 'forward
  "Store a direction last searched.")

(defun ari-cursor:search-forward-with-char (char)
  "Search just one character and move there quickly."
  (interactive "cMove to Char: ")
  ;; don't hit when it equals a character, under the cursor.
  (if (eq (char-after (point)) char) (forward-char))
  (and (search-forward (char-to-string char) nil t)
       (backward-char))
  (setq ari-cursor:%*last-search-char* char
        ari-cursor:%*last-search-direction* 'forward))

(defun ari-cursor:search-backward-with-char (char)
  "Search just one character backward and move there quickly."
  (interactive "cMove backward to Char: ")
  (search-backward (char-to-string char) nil t)
  (setq ari-cursor:%*last-search-char* char
        ari-cursor:%*last-search-direction* 'backward))

(defun ari-cursor:search-repeat-with-char ()
  "Search just one character repeatedly."
  (interactive)
  (let ((last-search-char ari-cursor:%*last-search-char*)
        (last-search-direction ari-cursor:%*last-search-direction*))
    (cond
      ((eq nil last-search-char) (message "You haven't searched yet. Stupid!"))
      ((eq last-search-direction 'forward)
       (or (ari-cursor:search-forward-with-char last-search-char) (backward-char)))
      ((eq last-search-direction 'backward) (ari-cursor:search-backward-with-char last-search-char)))))

(provide 'ari/cursor)
;; ari/cursor.el ends here
