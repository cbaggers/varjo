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

;; {TODO} Why is this needed? Surely we use the name straight from the spec
(defun gen-reserved-var-string (name-symbol)
  (let* ((name-string (symbol-name name-symbol))
         (split-name (split-sequence #\- name-string :test #'equal)))
    (format nil "gl_~{~a~}" (loop :for part :in split-name
                               :if (not (equal part "GL")) :collect
                               (if (<= (length part) 2)
                                   (string-upcase part)
                                   (string-capitalize part))))))


;;[TODO] this smells a bit, it is only used for glsl strings, and we should
;;       rename this to that end
(let ((num 0))
  (defun free-name (name)
    (let ((package (symbol-package name)))
      (if (valid-user-defined-name name)
	  (progn (incf num) (p-symb :varjo.free-vars
				    (if package
					(package-name package)
					nil)
				    name '- num 'v))
	  (error 'name-unsuitable :name name)))))

;;-------------------------------------------------------------------------

;; AH. One of the reasons for free-name was scoping, we may want many unique
;; glsl strings for the same lisp symbol
;;
;; (let ((x 1))
;;   (let ((x x))
;;     x))
;;
;; Needs to be somehting like
;; int x = 1;
;; int x0 = x;
;; x0;

(defun glsl-var-namep (name-symbol)
  "Returns true if the name is reserved"
  (let ((name (symbol-name name-symbol)))
    (or (when (> (length name) 2) (equal "GL-" (subseq name 0 3)))
        (when (> (length name) 2) (equal "FK-" (subseq name 0 3))) ;; fk use for fake structs
        (when (> (length name) 3) (equal "SYM-" (print (subseq name 0 4)))))))

(defun valid-user-defined-name (name-symbol)
  "Returns false if name is reserved"
  (not (glsl-var-namep name-symbol)))

(defun lisp-name->glsl-name (symbol env)
  (assert (symbolp symbol))
  (assert (valid-user-defined-name symbol) () 'name-unsuitable :name symbol)
  (let ((name-map (v-name-map env)))
    (or (gethash symbol name-map)
        (let ((str (if (symbol-package symbol)
                       (%get-free-glsl-name symbol name-map)
                       (%get-gensym-name symbol))))
          (add-lisp->glsl-name-mapping name-map symbol str)
          str))))

(defun add-lisp->glsl-name-mapping (name-map symbol string)
  (setf (gethash string name-map) symbol)
  (setf (gethash symbol name-map) string)
  nil)

(defun %get-free-glsl-name (symbol name-map)
  (let ((str-name (gen-glsl-string-for-symbol symbol)))
    (loop :until (not (gethash str-name name-map)) :for i :from 0 :do
       (setf str-name (format nil "~a~a" str-name i)))
    str-name))

(defun %get-gensym-name (symbol)
  (format nil "SYM_~a" symbol))

(defun gen-glsl-string-for-symbol (name)
  (let ((name (symbol-name name)))
    (format nil "~@[~a~]~{~a~}"
            (when (not (and (find (elt name 0) +ascii-alpha-num+)
                            (alpha-char-p (elt name 0))))

              "_")
            (map 'list (lambda (_)
                         (if (find _ +ascii-alpha-num+) _
                             (if (char= _ #\-) #\_
                                 (format nil "~a" (char-code _)))))
                 name))))



;; {TODO} remove
;; (defvar *v-gensym-count* 0)
;; (defun v-gensym (&optional name)
;;   (intern (format nil "SYM-~a-~a" name (incf *v-gensym-count*)) :varjo-syms))
