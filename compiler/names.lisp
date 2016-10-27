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

;; The (safe-glsl-name-string (free-name ..)) combo can be replaced with
;; (new-lisp-name->glsl-name ..)

;; safe-glsl-name-string was used on it's own those when we wanted a direct
;; translation from lisp name. For example with in-args/uniforms/structs

;; free-name used on it's own returned a symbol and feels like
;; (new-lisp-name->glsl-name (gensym ..)) to me

(defun glsl-var-namep (name-symbol)
  "Returns true if the name is reserved"
  (let ((name (symbol-name name-symbol)))
    (or (uiop:string-prefix-p "GL-" name)
        (uiop:string-prefix-p "FK-" name) ;; fk use for fake structs
        (uiop:string-prefix-p "SYM-" name))))

(defun valid-user-defined-name (name-symbol)
  "Returns false if name is reserved"
  (not (glsl-var-namep name-symbol)))

(defun new-lisp-name->glsl-name (symbol env)
  (assert (symbolp symbol))
  (assert (valid-user-defined-name symbol) () 'name-unsuitable :name symbol)
  (let ((name-map (v-name-map env)))
    (let ((str (if (symbol-package symbol)
                   (%get-free-glsl-name symbol name-map)
                   (%get-gensym-name symbol))))
      (add-lisp->glsl-name-mapping name-map symbol str)
      str)))

(defun add-lisp->glsl-name-mapping (name-map symbol string)
  (setf (gethash string name-map) symbol)
  nil)

(defun %get-free-glsl-name (symbol name-map)
  (let* ((orig-str-name (gen-glsl-string-for-symbol symbol))
         (curr-str-name orig-str-name))
    (loop :until (not (gethash curr-str-name name-map)) :for i :from 0 :do
       (setf curr-str-name (format nil "~a~a" orig-str-name i)))
    curr-str-name))

(defun %get-gensym-name (symbol)
  (format nil "g_~a" (gen-glsl-string-for-symbol symbol)))

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
