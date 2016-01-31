(in-package :varjo)

(defun gen-reserved-var-string (name-symbol)
  (let* ((name-string (symbol-name name-symbol))
         (split-name (split-sequence #\- name-string :test #'equal)))
    (format nil "gl_~{~a~}" (loop :for part :in split-name
                               :if (not (equal part "GL")) :collect
                               (if (<= (length part) 2)
                                   (string-upcase part)
                                   (string-capitalize part))))))

(defun num-suffix (type)
  (or (assocr (type->type-spec type) '((v-double . "lf") (v-float . "f") (v-uint . "u"))) ""))

(defun gen-number-string (number type)
  (format nil "~a~a" number (num-suffix type)))

(defun gen-variable-string (var-name v-value)
  (format nil "~a" (or (v-glsl-name v-value) ()
                       (string-downcase (string var-name)))))

(defun gen-function-string (func arg-objs &optional out-strings)
  (apply #'format nil (v-glsl-string func)
         (append (mapcar #'current-line arg-objs)
                 out-strings
                 (mapcar #'v-glsl-name (implicit-args func)))))

(defun gen-function-transform (name args &optional out-args implicit-args)
  (format nil "~a(~{~a~^,~})" name
          (loop :for i :below (+ (length args) (length out-args)
                                 (length implicit-args))
             :collect "~a")))

(defun gen-implicit-arg-pairs (implicit-args)
  (loop :for a :in implicit-args :collect
     `(,(v-glsl-string (v-type a)) ,(v-glsl-name a))))

(defun gen-arg-string (arg-pairs &optional out-pairs)
  (let ((arg-string (format nil "~{~{~a ~a~}~^,~^ ~}" arg-pairs)))
    (if out-pairs
        (if (> (length arg-string) 0)
            (format nil "~a, ~{~{out ~a ~a~}~^,~^ ~}" arg-string out-pairs)
            (format nil "~{~{out ~a ~a~}~^,~^ ~}" out-pairs))
        arg-string)))

(defun gen-glsl-function-body-string (name args type glsl-string)
  (format nil "~a ~a(~a) {~%~a~%}~%"
          (v-glsl-string type)
          (string-downcase (string name))
          (gen-arg-string args)
          glsl-string))

(defun gen-function-signature (name args out-args return-types implicit-args)
  (let ((args (append args (gen-implicit-arg-pairs implicit-args))))
    (format nil "~a ~a(~a);"
            (v-glsl-string return-types)
            name
            (gen-arg-string args out-args))))

(defun gen-function-body-string (name args out-args type body-obj implicit-args)
  (let ((args (append args (gen-implicit-arg-pairs implicit-args))))
    (format nil "~a ~a(~a) {~%~{~a~%~}~@[~a~%~]}~%"
            (v-glsl-string type)
            (string name)
            (gen-arg-string args out-args)
            (remove "" (to-block body-obj) :test #'equal)
            (current-line (end-line body-obj)))))

(defun gen-assignment-string (place val)
  (format nil "~a = ~a" (current-line place) (current-line val)))

(defun %gen-assignment-string (lhs rhs)
  (format nil "~a = ~a" lhs rhs))

(defun gen-setq-assignment-string (old-value new-value-code-obj)
  (format nil "~a = ~a" (v-glsl-name old-value)
	  (current-line new-value-code-obj)))

(defun gen-out-var-assignment-string (glsl-name val)
  (format nil "~a = ~a" glsl-name (current-line val)))

(defun gen-bool-or-string (objs)
  (format nil "~{~a~^ || ~}" (mapcar #'current-line objs)))

(defun gen-bool-and-string (objs)
  (format nil "~{~a~^ && ~}" (mapcar #'current-line objs)))

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
          (or (remove nil (to-block body-obj)) (list ""))
          (or (current-line body-obj) "")))

(defun gen-switch-string (test-obj keys clause-body-objs
                          &optional (default-symb 'default))
  (let* ((default-clause nil)
         (format-clauses
          (loop :for key :in keys
             :for obj :in clause-body-objs
             :append
             (if (eq key default-symb)
                 ;; {TODO}                     WTF! -vvvvvvvvvvvvvvv
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
  (copy-code obj :current-line (format nil "~(~{~a ~}~)~a"
                                       (string-downcase (string qualifiers))
                                       (current-line obj))
	     :multi-vals nil
	     :place-tree nil
	     :flow-ids (flow-ids obj)))

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

(defun gen-out-var-string (glsl-name type qualifiers &optional layout)
  (when (typep type 'v-none)
    (error 'none-type-in-out-vars :glsl-name glsl-name))
  (format nil "~@[layout(location = ~a) ~]~a;" layout
          (prefix-type-to-string type glsl-name qualifiers 'out)))

(defun gen-in-var-string (glsl-name type qualifiers &optional layout)
  (format nil "~@[layout(location = ~a) ~]~a;" layout
          (prefix-type-to-string type glsl-name qualifiers 'in)))

(defun gen-uniform-decl-string (glsl-name type qualifiers)
  (format nil "uniform ~a;" (prefix-type-to-string type glsl-name)))

;;[TODO] make this properly
(defun lisp-name->glsl-name (name)
  (string name))

(defun gen-shader-string (post-proc-obj)
  (with-slots (code env) post-proc-obj
    (format nil "#version ~a~%~{~%~{~a~%~}~}" (get-version-from-context env)
	    (loop :for part :in
	       (list (used-types post-proc-obj)
		     (mapcar #'last1 (in-args post-proc-obj))
		     (mapcar #'last1 (out-vars post-proc-obj))
		     (concatenate 'list
				  (mapcar #'last1 (uniforms post-proc-obj))
				  (mapcar #'third (stemcells post-proc-obj)))
		     (signatures code)
		     (to-top code))
	       :if part :collect part))))

;;----------------------------------------------------------------------

;; storage_qualifier block_name
;; {
;;   <define members here>
;; } instance_name;

(defun write-interface-block (storage-qualifier block-name slots
                              &optional (layout "std140"))
  (format nil "~@[layout(~a) ~]~a ~a~%{~%~{~a~%~}} ~a;"
          layout
          (string-downcase (symbol-name storage-qualifier))
          (format nil "_UBO_~a" block-name)
          (mapcar #'gen-interface-block-slot-string slots)
          block-name))

(defun gen-interface-block-slot-string (slot)
  (destructuring-bind (slot-name slot-type &key accessor) slot
    (let ((name (or accessor slot-name))
          (type-obj (type-spec->type slot-type)))
      (format nil "    ~{~a ~}~a"
                ;;(loop :for q :in qualifiers :collect (string-downcase (string q)))
nil
                (if (typep type-obj 'v-array)
                    (format nil "~a ~a[~a];"
                            (v-glsl-string (type->type-spec (v-element-type type-obj)))
                            (safe-glsl-name-string name)
                            (v-dimensions type-obj))
                    (format nil "~a ~a;"
                            (v-glsl-string type-obj)
                            (safe-glsl-name-string name)))))))

;;----------------------------------------------------------------------

(defgeneric indent (input))

(defmethod indent ((input string))
  (mapcar #'(lambda (x) (format nil "    ~a" x))
          (split-sequence:split-sequence #\newline input)))

(defmethod indent ((input list))
  (mapcan #'indent input))

(defun indent-ob (code-obj)
  (copy-code code-obj :to-block (indent (to-block code-obj))
	     :multi-vals nil
	     :place-tree nil
	     :flow-ids (flow-ids code-obj)))
