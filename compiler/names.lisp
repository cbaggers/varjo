(in-package :varjo)

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

(defun gen-reserved-var-string (name-symbol)
  (let* ((name-string (symbol-name name-symbol))
         (split-name (split-sequence #\- name-string :test #'equal)))
    (format nil "gl_~{~a~}" (loop :for part :in split-name
                               :if (not (equal part "GL")) :collect
                               (if (<= (length part) 2)
                                   (string-upcase part)
                                   (string-capitalize part))))))

(defun valid-user-defined-name (name-symbol)
  (not (glsl-var-namep name-symbol)))

;; {TODO} why not fk- or -sc- ? I guess this is some relic of the past
(defun glsl-var-namep (name-symbol)
  (let ((name (symbol-name name-symbol)))
    (or (when (> (length name) 2) (equal "GL-" (subseq name 0 3)))
        (when (> (length name) 2) (equal "FK-" (subseq name 0 3)))
        (when (> (length name) 3) (equal "-SC-" (subseq name 0 4))))))

(defun %get-free-glsl-name (symbol name-map)
  (error "NOT IMPLEMENTED YET ~s ~s" symbol name-map)
  (let (())
    )
  ;; loop up from
  )

;;[TODO] this smells a bit, it is only used for glsl strings, and we should
;;       rename this to that end
(let ((num 0))
  (defun free-name (name)
    (declare (ignore env))
    (when counter (setf num counter))
    (let ((package (symbol-package name)))
      (if (valid-user-defined-name name)
	  (progn (incf num) (p-symb :varjo.free-vars
				    (if package
					(package-name package)
					nil)
				    name '- num 'v))
	  (error 'name-unsuitable :name name)))))
