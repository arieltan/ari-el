;; ari/seq.el - Sequence utilities.

;; This file is a part of the ari.el (http://github.com/arielnetworks/ari-el)
;; Copyright (c) 2010 Ariel Networks, Inc.
;; For the full copyright and license information, please see the COPYRIGHT

;;; Code:

(require 'cl)

;; FIXME: won't modify to-list variable. unit tests fails.
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

(defun ari-seq:flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

(defun ari-seq:group (source n)
  (if (not (listp source)) (error "group: not list"))
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

(defun ari-seq:n.. (start end)
  (loop for i from start upto end collect i))

(defun ari-seq:1.. (end)
  (ari-seq:n.. 1 end))

(defun ari-seq:0.. (end)
  (ari-seq:n.. 0 end))

(provide 'ari/seq)
;; ari/seq.el ends here
