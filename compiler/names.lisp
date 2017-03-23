(in-package :varjo)

(defun safe-glsl-name-string (name)
  "return a namestring that follows GLSL conventions"
  (if (glsl-var-namep name) 
      (error 'name-unsuitable :name name) ;name is used as a variable
      (let ((name (symbol-name name)))
        (format nil "~@[~a~]~{~a~}"
                (unless (find (elt name 0) +ascii-alpha-num+ :start 10)
                  "_") ;; first character must be alpha or _
                (map 'list (lambda (_)
			     (cond ((find _ +ascii-alpha-num+) _ ) ;no change
			           ((char= #\-) #\_) ;replace - with _
			           (t (format nil "~a" (char-code _)))));numify
                     name)))))

;;-------------------------------------------------------------------------

;; safe-glsl-name-string was used on it's own those when we wanted a direct
;; translation from lisp name. For example with in-args/uniforms/structs

(defun glsl-var-namep (name-symbol)
  "Returns true if the name is reserved"
  (let ((name (symbol-name name-symbol)))
    (or (uiop:string-prefix-p "GL-" name)
        (uiop:string-prefix-p "FK-" name) ;; fk use for fake structs
        (uiop:string-prefix-p "SYM-" name))))

(defun valid-user-defined-name (name-symbol)
  "Returns false if name is reserved"
  (not (glsl-var-namep name-symbol)))

(defun lisp-name->glsl-name (symbol env)
  (assert (symbolp symbol))
  (assert (valid-user-defined-name symbol) () 'name-unsuitable :name symbol)
  (let ((name-map (v-name-map env)))
    (let ((str (if (symbol-package symbol)
                   (%get-free-glsl-name symbol name-map)
                   (%get-gensym-name symbol))))
      (add-lisp->glsl-name-mapping name-map symbol str)
      str)))

(defun add-lisp-name (symbol env &optional glsl-name)
  (assert (symbolp symbol))
  (assert (valid-user-defined-name symbol) () 'name-unsuitable :name symbol)
  (%add-lisp-name symbol env glsl-name nil))

(defun add-reserved-lisp-name (symbol env &optional glsl-name)
  (assert (symbolp symbol))
  (%add-lisp-name symbol env glsl-name t))

(defun %add-lisp-name (symbol env glsl-name allow-collisions-for-same-symbol)
  (let ((name-map (v-name-map env)))
    (let ((str (or glsl-name (gen-glsl-string-for-symbol symbol))))
      (if allow-collisions-for-same-symbol
          (let ((symb (gethash str name-map)))
            (when (and symb (not (eq symb symbol)))
              (error 'name-mismatch :lisp symbol :glsl str :taken symb)))
          (assert (not (gethash str name-map)) ()
                  'name-clash :lisp symbol :glsl str))
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
