(in-package :varjo)

(defun safe-glsl-name-string (name)
  (if (valid-user-defined-name name) 
      (string-downcase 
       (cl-ppcre:regex-replace-all "[-]" (symbol-name (symb name)) "_"))
      (error 'name-unsuitable :name name)))

(defun gen-reserved-var-string (name-symbol)
  (let* ((name-string (symbol-name name-symbol))
         (split-name (split-sequence #\- name-string :test #'equal)))
    (format nil "gl_~{~a~}" (loop :for part :in split-name 
                               :if (not (equal part "GL")) :collect
                               (if (<= (length part) 2)
                                   (string-upcase part)
                                   (string-capitalize part))))))

(defun num-suffix (type)
  (or (assocr (type->type-spec type) '((v-float . "f") (v-uint . "u"))) ""))

(defun gen-number-string (number type)
  (format nil "~a~a" number (num-suffix type)))

(defun gen-variable-string (var-name v-value)
  (format nil "~a" (or (v-glsl-name v-value)
                       (string-downcase (string var-name)))))

(defun gen-function-string (func arg-objs)
  (apply #'format nil (v-glsl-string func) (mapcar #'current-line arg-objs)))

(defun gen-function-transform (name args)
  (format nil "~a(~{~a~^,~})" name (loop for i in args collect "~a")))

(defun gen-function-signature (name args return-type)
  (format nil "~a ~a(~(~{~{~a ~a~}~^,~^ ~}~));"
          (v-glsl-string return-type)
          name
          args))

(defun gen-function-body-string (name args type body-obj)
  (format nil "~a ~a(~(~{~{~a ~a~}~^,~^ ~}~)) {~%~{~a~%~}~@[~a~%~]}~%"
          (v-glsl-string type)
          (string-downcase (string name)) 
          args
          (remove "" (to-block body-obj) :test #'equal) 
          (current-line (end-line body-obj))))

(defun gen-assignment-string (place val)
  (format nil "~a = ~a" (current-line place) (current-line val)))

(defun gen-out-var-assignment-string (var-name val)
  (format nil "~a = ~a" (safe-glsl-name-string var-name) (current-line val)))

(defun gen-if-string (test-obj then-obj else-obj)
  (if else-obj
      (format nil "~a~&if (~a) {~{~%~a~}~%~a~%} else {~{~%~a~}~%~a~%}"
              (or (to-block test-obj) "") 
              (current-line test-obj)
              (or (to-block then-obj) nil) 
              (current-line then-obj)
              (or (to-block else-obj) nil) 
              (current-line else-obj))
      (format nil "~a~&if (~a) {~{~%~a~}~%~a~%}"
              (or (to-block test-obj) "") 
              (current-line test-obj)
              (or (to-block then-obj) nil)
              (current-line then-obj))))

(defun gen-while-string (test-obj body-obj)
  (format nil "~{~a~%~}while (~a) {~%~{~a~%~}~a;~%}"
          (to-block test-obj)
          (current-line test-obj)
          (to-block body-obj)
          (current-line body-obj)))

(defun gen-swizzle-string (vec-obj components-string)
  (format nil "~a.~a" (current-line vec-obj) (string-downcase components-string)))

(defun gen-for-loop-string (var-string condition-obj update-obj body-obj)
  (format nil "for (~a;~a;~a) {~%~{~a~%~}~a~%}"
          var-string
          (current-line condition-obj)
          (current-line update-obj)
          (to-block body-obj)
          (current-line body-obj)))

(defun gen-switch-string (test-obj keys clause-body-objs
                          &optional (default-symb 'default))
  (let* ((default-clause nil)
         (format-clauses 
          (loop :for key :in keys
             :for obj :in clause-body-objs
             :append
             (if (eq key default-symb) 
                 (progn (setf default-clause (list "default" nil "jam")) nil)
                 (list key 
                       (or (to-block obj) nil) 
                       (current-line obj))) :into result
             :finally (return (append result default-clause))))) 
    (format nil "~a~%switch (~a) {~{~%case ~a:~%~{~a~^~%~}~a;~%break;~}}"
            (or (to-block test-obj) "") 
            (current-line test-obj)
                  format-clauses)))

(defun qualify (obj &rest qualifiers)
  (%qualify obj qualifiers))

(defun %qualify (obj qualifiers)
  (merge-obs obj :current-line (format nil "~(~{~a ~}~)~a" 
                                       (string-downcase (string qualifiers)) 
                                       (current-line obj))))

(defun prefix-type-to-string (type line-string &optional qualifiers storage-qual)
  (let* ((line (cond ((typep type 'v-array) (format nil (v-glsl-string type)
                                                    line-string))
                     ((typep type 'v-type) (format nil "~a ~a" 
                                                   (v-glsl-string type)
                                                   line-string))
                     (t (error "dont know how to add the type here")))))
    (if qualifiers
        (format nil "~{~a~^ ~}~@[~( ~a~)~] ~a" 
                (loop :for q :in qualifiers :collect (string-downcase (string q)))
                storage-qual
                line)
        (format nil "~@[~(~a ~)~]~a" storage-qual line))))

(defun prefix-type-declaration (code-obj &optional qualifiers storage-qual)
  (prefix-type-to-string (code-type code-obj) (current-line code-obj) qualifiers
                         storage-qual))

(defun gen-out-var-string (name qualifiers value)
  (let ((name (if (stringp name) 
                  (string-downcase
                   (cl-ppcre:regex-replace-all "[-]" (symbol-name (symb name)) "_"))
                  (safe-glsl-name-string name))))
    (format nil "~a;" (prefix-type-to-string (v-type value) name qualifiers 'out))))

(defun gen-in-var-string (name type qualifiers &optional layout)
  (let ((name (if (stringp name) 
                  (string-downcase
                   (cl-ppcre:regex-replace-all "[-]" (symbol-name (symb name)) "_"))
                  (safe-glsl-name-string name))))
    (format nil "~@[layout(location = ~a) ~]~a;" layout
            (prefix-type-to-string type name qualifiers 'in))))

(defun gen-uniform-decl-string (name type)
  (format nil "uniform ~a;" (prefix-type-to-string type (safe-glsl-name-string name))))

;;[TODO] make this properly
(defun lisp-name->glsl-name (name)
  (string name))

(defun gen-shader-string (code-obj env)
  (format nil "#version ~a~%~{~%~{~a~%~}~}" (get-version-from-context env)
          (loop :for part :in 
             (list (used-types code-obj)
                   (mapcar #'fourth (v-in-args env))
                   (mapcar #'fourth (out-vars code-obj))
                   (mapcar #'third (v-uniforms env))
                   (signatures code-obj)
                   (to-top code-obj))
             :if part :collect part)))

;;----------------------------------------------------------------------

(defgeneric indent (input))

(defmethod indent ((input string))
  (mapcar #'(lambda (x) (format nil "    ~a" x))
          (split-sequence:split-sequence #\newline input)))

(defmethod indent ((input list))
  (mapcan #'indent input))

(defun indent-ob (code-obj)
  (merge-obs code-obj
             :to-block (indent (to-block code-obj))))
