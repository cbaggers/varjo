(in-package :varjo.internals)

;;-------------------------------------------------------------------------

(defvar *glsl-reserved-names*
  (let ((ht (make-hash-table :test #'equal)))
    (loop :for name :in *base-reserved* :do
       (setf (gethash name ht) t))
    ht))

;;-------------------------------------------------------------------------

(macrolet ((define-ascii-char-ranges ()
              (let ((ranges)
                    (min nil)
                    (max nil))
                (loop :for i :in (sort (map 'list #'char-code +ascii-alpha-num+) #'<) :do
                   (if min
                       (if (= (- i max) 1)
                           (setf max i)
                           (progn
                             (push (list min max) ranges)
                             (setf min i
                                   max i)))
                       (setf min i
                             max i))
                   :finally (push (list min max) ranges))
                (let* ((ranges (reverse ranges)))
                  `(progn
                     (defun glsl-alphanumeric-p (char)
                       (declare (optimize (speed 3) (debug 1) (safety 1))
                                (type character char))
                       (let ((code (char-code char)))
                         (or ,@(loop :for (min max) :in ranges :collect
                                  `(and (>= code ,min) (<= code ,max)))))))))))
  (define-ascii-char-ranges))

;;-------------------------------------------------------------------------

;; safe-glsl-name-string was used on it's own those when we wanted a direct
;; translation from lisp name. For example with in-args/uniforms/structs

(defun safe-glsl-name-string (name)
  (if (valid-user-defined-name name)
      (gen-glsl-string-for-symbol name)
      (error 'name-unsuitable :name name)))

(defun gen-glsl-string-for-symbol (name)
  (let ((name (symbol-name name)))
    (avoid-reserved
     (format nil "~@[~a~]~{~a~}"
             (when (not (and (glsl-alphanumeric-p (elt name 0))
                             (alpha-char-p (elt name 0))))

               "_")
             (map 'list #'replace-char-in-name
                  (replace-substrings-in-name name))))))

(defun replace-substrings-in-name (name)
  (ppcre:regex-replace "->" name "-TO-"))

(defun replace-char-in-name (c)
  (if (glsl-alphanumeric-p c)
      c
      (if (char= c #\-)
          #\_
          (char-name-or-code-str c))))

(defun avoid-reserved (name)
  (let ((orig-str-name name)
        (curr-str-name name))
    (loop
       :until (not (gethash curr-str-name *glsl-reserved-names*))
       :for i :from 0
       :do
       (setf curr-str-name (format nil "~a~a" orig-str-name i))
       (incf i))
    curr-str-name))

(defun char-name-or-code-str (char)
  (declare (type character char))
  (cond
    ((char= char #\·) "_DOT_")
    ((char= char #\²) "_SQUARED")
    ((char= char #\³) "_CUBED")
    (t (let ((name (char-name char)))
         (or (when (and (> (length name) 21)
                        (equal "GREEK_CAPITAL_LETTER_" (subseq name 0 21)))
               (subseq name 21))
             (when (and (> (length name) 19)
                        (equal "GREEK_SMALL_LETTER_" (subseq name 0 19)))
               (format nil "SMALL_~a" (subseq name 19)))
             (format nil "~a" (char-code char)))))))

;;-------------------------------------------------------------------------

(defun register-reserved-name (name)
  (check-type name string)
  (setf (gethash name *glsl-reserved-names*) t)
  name)

(defun glsl-var-namep (name-symbol)
  "Returns true if the name is reserved"
  (let ((name (symbol-name name-symbol)))
    (or (uiop:string-prefix-p "GL-" name)
        (uiop:string-prefix-p "FK-" name) ;; fk use for fake structs
        (uiop:string-prefix-p "SYM-" name))))

(defun valid-user-defined-name (name-symbol)
  "Returns false if name is reserved"
  (or (not (glsl-var-namep name-symbol))
      (not (eq name-symbol t))
      (null name-symbol)))

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

(defun declare-glsl-name-taken (env glsl-name)
  (let ((name-map (v-name-map env)))
    (setf (gethash glsl-name name-map) nil))
  glsl-name)

(defun %get-free-glsl-name (symbol name-map)
  (let* ((orig-str-name (gen-glsl-string-for-symbol symbol))
         (curr-str-name orig-str-name)
         (i 0)
         (done nil))
    (loop
       :do
       (setf done t)
       (loop :until (not (gethash curr-str-name name-map)) :do
          (setf curr-str-name (format nil "~a~a" orig-str-name i)
                done nil)
          (incf i))
       (loop :until (not (gethash curr-str-name *glsl-reserved-names*)) :do
          (setf curr-str-name (format nil "~a~a" orig-str-name i)
                done nil)
          (incf i))
       :until done)
    curr-str-name))

(defun %get-gensym-name (symbol)
  (format nil "g_~a" (gen-glsl-string-for-symbol symbol)))
