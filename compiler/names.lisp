(in-package :varjo)

(defvar +ascii-alpha-num+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defun safe-glsl-name-string (name)
  (if (valid-user-defined-name name)
      (let ((name (symbol-name name)))
        (format nil "~@[~a~]~{~a~}"
                (when (not (and (find (elt name 0) +ascii-alpha-num+)
                                (alpha-char-p (elt name 0))))
                  "_")
                (map 'list (lambda (_)
                             (if (find _ +ascii-alpha-num+) _
                                 (if (char= _ #\-) #\_
                                     (format nil "~a" (char-code _)))))
                     name)))
      (error 'name-unsuitable :name name)))
