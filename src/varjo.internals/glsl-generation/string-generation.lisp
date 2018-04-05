(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defun gen-number-string (number type)
  (typecase type
    (v-double (format nil "~flf" number))
    (v-float (format nil "~ff" number))
    (v-uint (format nil "~au" number))
    (otherwise (format nil "~a" number))))

(defun gen-variable-string (var-name v-value)
  (let ((type (v-type-of v-value)))
    (if (typep type 'v-block-struct)
        (block-name type)
        (or (glsl-name v-value)
            (string-downcase (string var-name))))))

(defun gen-function-call-string (func arg-objs &optional out-strings)
  ;; This will include ephemeral types if passed. It is the responsibility
  ;; of the calling code to handle this
  (when (v-glsl-string func)
    (let* ((arg-glsl (mapcar λ(current-line _ t) arg-objs))
           (rest-pos (&rest-pos func))
           (arg-glsl (if rest-pos
                         (append (subseq arg-glsl 0 rest-pos)
                                 (list (subseq arg-glsl rest-pos)))
                         arg-glsl)))
      (apply #'format nil (v-glsl-string func)
             (append arg-glsl
                     out-strings
                     (mapcar #'glsl-name (implicit-args func)))))))

(defun gen-function-transform (name args &optional out-args implicit-args)
  (format nil "~a(~{~a~^,~})" name
          (loop :for i :below (+ (length args) (length out-args)
                                 (length implicit-args))
             :collect "~a")))

(defun gen-implicit-arg-tripples (implicit-args)
  (loop :for a :in implicit-args :collect
     `(nil ,(v-glsl-string (v-type-of a)) ,(glsl-name a))))

(defun gen-in-out-arg-tripples (implicit-args)
  (loop :for a :in implicit-args :collect
     `("inout" ,(v-glsl-string (v-type-of a)) ,(glsl-name a))))

(defun gen-arg-string (arg-tripples &optional out-pairs)
  (let ((arg-string (format nil "~{~{~@[~a ~]~a ~a~}~^,~^ ~}" arg-tripples)))
    (if out-pairs
        (if (> (length arg-string) 0)
            (format nil "~a, ~{~{out ~a ~a~}~^,~^ ~}" arg-string out-pairs)
            (format nil "~{~{out ~a ~a~}~^,~^ ~}" out-pairs))
        arg-string)))

(defun gen-function-signature (name args out-args return-types implicit-args
                               in-out-args)
  (let ((args (append (mapcar λ(cons nil _) args)
                      (gen-implicit-arg-tripples implicit-args)
                      (gen-in-out-arg-tripples in-out-args))))
    (format nil "~a ~a(~a);"
            (v-glsl-string return-types)
            name
            (gen-arg-string args out-args))))

(defun gen-function-body-string (name args out-args type body-obj implicit-args
                                 in-out-args)
  (let ((args (append (mapcar λ(cons nil _) args)
                      (gen-implicit-arg-tripples implicit-args)
                      (gen-in-out-arg-tripples in-out-args))))
    (glsl-chunk-splicing
      :line (glsl-line "~a ~a(~a)"
                       (v-glsl-string type)
                       (string name)
                       (gen-arg-string args out-args))
      :line (glsl-line "{")
      :chunk (indent (glsl-chunk-from-compiled body-obj))
      :line (glsl-line "}"))))

(defun gen-bin-op-string (op-symbol place val)
  (assert (symbolp op-symbol))
  ;; {TODO} what logic do we want here?
  (format nil "~a ~a ~a" (current-line place) op-symbol (current-line val)))

(defun %gen-assignment-string (lhs rhs)
  (format nil "~a = ~a" lhs rhs))

(defun gen-setq-assignment-string (old-value new-value-code-obj)
  (format nil "~a = ~a" (glsl-name old-value)
          (current-line new-value-code-obj)))

(defun gen-out-var-assignment-string (glsl-name val)
  (format nil "~a = ~a" glsl-name (current-line val)))

(defun gen-bool-or-string (objs)
  (format nil "~{~a~^ || ~}" (mapcar #'current-line objs)))

(defun gen-bool-and-string (objs)
  (format nil "~{~a~^ && ~}" (mapcar #'current-line objs)))

(defun gen-while-chunk (test-obj body-obj)
  (glsl-chunk-splicing
    :chunk (to-block test-obj)
    :line (glsl-line "while (~a)" (current-line test-obj))
    :line (glsl-line "{")
    :chunk (indent (glsl-chunk-from-compiled body-obj))
    :line (glsl-line "}")))

(defun gen-swizzle-string (vec-obj components-string)
  (format nil "~a.~a" (current-line vec-obj)
          (string-downcase components-string)))

(defun remove-empty (list)
  (labels ((empty-p (x)
             (uiop:emptyp
              (if (stringp x)
                  (string-trim '(#\space) x)
                  x))))
    (remove-if #'empty-p list)))

(defun gen-for-loop-chunk (decl-chunk
                           var-string
                           condition-obj
                           update-obj
                           body-obj)
  ;; We rely on end-line to create the semicolons that
  ;; seperate the clauses in the for statement
  (glsl-chunk-splicing
    :chunk decl-chunk
    :line (glsl-line "for (~a ~a ~a)"
                     (glsl-line-string-part var-string)
                     (current-line condition-obj)
                     (current-line update-obj))
    :line (glsl-line "{")
    :chunk (indent (glsl-chunk-from-compiled body-obj))
    :line (glsl-line "}")))

(defun gen-switch-chunk (test-obj keys clause-body-objs
                          &optional (default-symb 'default))
  (let* ((cases-chunk
          (join-glsl-chunks
              (loop :for key :in keys
                 :for obj :in clause-body-objs
                 :collect
                 (if (eq key default-symb)
                     (error "Varjo Bug: switch default not implemented") ;; {TODO}
                     (indent
                      (glsl-chunk-splicing
                        :line (glsl-line "case ~a:" key)
                        :chunk (glsl-chunk-from-compiled obj)
                        :line (glsl-line "break;" key))))))))
    (glsl-chunk-splicing
      :chunk (to-block test-obj)
      :line (glsl-line "switch (~a)" (current-line test-obj))
      :line (glsl-line "{")
      :chunk cases-chunk
      :line (glsl-line "}"))))

(defun prefix-type-to-string (type line-string
                              &optional qualifiers storage-qual)
  (let* ((line (cond ((typep type 'v-type) (format nil "~a ~a"
                                                   (v-glsl-string type)
                                                   line-string))
                     (t (error "dont know how to add the type here")))))
    (if qualifiers
        (format nil "~{~a~^ ~}~@[~( ~a~)~] ~a"
                (remove nil (mapcar #'glsl-string qualifiers))
                storage-qual
                line)
        (format nil "~@[~(~a ~)~]~a" storage-qual line))))

(defun prefix-type-declaration (code-obj &optional qualifiers storage-qual)
  (prefix-type-to-string (primary-type code-obj)
                         (current-line code-obj)
                         qualifiers
                         storage-qual))

(defun gen-out-var-string (glsl-name type qualifiers &optional layout)
  (format nil "~@[layout(location = ~a) ~] ~a;" layout
          (prefix-type-to-string type glsl-name
                                 (append qualifiers (list *out-qualifier*)))))

(defun gen-in-var-string (glsl-name type qualifiers &optional layout)
  (format nil "~@[layout(location = ~a) ~] ~a;" layout
          (prefix-type-to-string type glsl-name
                                 (append qualifiers (list *in-qualifier*)))))

(defun gen-uniform-decl-string (glsl-name type qualifiers)
  (declare (ignore qualifiers))
  (format nil "uniform ~a;" (prefix-type-to-string type glsl-name)))

(defun gen-shared-decl-string (glsl-name type qualifiers)
  (declare (ignore qualifiers))
  (format nil "shared ~a;" (prefix-type-to-string type glsl-name)))

(defun gen-geom-input-primitive-string (primitive)
  (format nil "layout (~a) in;" (glsl-string primitive)))

(defun gen-geom-output-primitive-string (metadata)
  (with-slots (vari.cl:kind vari.cl:max-vertices) metadata
    (assert (find vari.cl:kind '(:points :line-strip :triangle-strip)) ()
            'invalid-output-primitive-for-geometry
            :kind vari.cl:kind)
    (format nil "layout (~a, max_vertices = ~a) out;"
            (glsl-string (primitive-name-to-instance vari.cl:kind))
            vari.cl:max-vertices)))

(defun gen-tess-con-output-primitive-string (metadata)
  (with-slots (vari.cl:vertices) metadata
    (format nil "layout (vertices = ~a) out;" vari.cl:vertices)))

(defun gen-compute-local-size-layout-string (metadata)
  (with-slots (vari.cl::x vari.cl::y vari.cl::z) metadata
    (format nil "layout(local_size_x = ~a, local_size_y = ~a, local_size_z = ~a) in;"
            (or vari.cl::x 1)
            (or vari.cl::y 1)
            (or vari.cl::z 1))))

(defun gen-tess-eval-output-primitive-string (metadata)
  (with-slots (vari.cl:primitive vari.cl:spacing vari.cl:order) metadata
    (let ((primitive (primitive-name-to-instance vari.cl:primitive)))
      (format nil "layout (~a, ~a, ~a) in;"
              (glsl-string primitive)
              (ecase vari.cl:spacing
                (:equal "equal_spacing")
                (:equal-spacing "equal_spacing")
                (:fractional-even "fractional_even_spacing")
                (:fractional-even-spacing "fractional_even_spacing")
                (:fractional-odd "fractional_odd_spacing")
                (:fractional-odd-spacing "fractional_odd_spacing"))
              (ecase vari.cl:order
                (:cw "cw")
                (:ccw "ccw"))))))

(defun gen-shader-string (post-proc-obj)
  (with-slots (env) post-proc-obj
    (format
     nil "// ~a~%#version ~a~%~{~%~{~a~%~}~}"
     (string-downcase (type-of (stage env)))
     (get-version-from-context env)
     (remove nil
             (list (used-user-structs post-proc-obj)
                   (in-declarations post-proc-obj)
                   (input-variable-glsl post-proc-obj)
                   (out-declarations post-proc-obj)
                   (output-variable-glsl post-proc-obj)
                   (shared-decls post-proc-obj)
                   (remove-empty
                    (append
                     (mapcar #'%glsl-decl (uniforms post-proc-obj))
                     (mapcar #'%glsl-decl (stemcells post-proc-obj))))
                   (signatures env)
                   (let* ((funcs (all-functions post-proc-obj))
                          (funcs (remove-if (lambda (f) (= 0 (call-count f)))
                                            funcs))
                          (code (remove nil (mapcar #'glsl-code funcs))))
                     (reverse code)))))))

(defmethod out-block-name-for ((stage stage))
  (assert (not (typep stage 'fragment-stage)) ()
          "Fragment shaders cannot have 'out' interface blocks")
  (let ((stage
         (if (typep stage 'compiled-stage)
             (starting-stage stage)
             stage)))
    (symb :from_ (type-of stage))))

(defmethod in-block-name-for ((stage stage))
  (assert (not (typep stage 'vertex-stage)) ()
          "Vertex shaders cannot have 'in' interface blocks")
  (let ((prev (previous-stage stage)))
    (if prev
        (out-block-name-for prev)
        *fallback-block-name*)))

(defmethod block-name-string (block-name)
  (substitute #\_ #\- (format nil "_~a_" block-name)))

(defmethod block-name-string ((var output-variable))
  (when (block-name var)
    (block-name-string (block-name var))))

(defun requires-out-interface-block (stage out-var-index)
  (not (or (typep stage 'fragment-stage)
           (= out-var-index 0))))

(defun requires-in-interface-block (stage)
  (not (typep stage 'vertex-stage)))

;;----------------------------------------------------------------------

;; storage_qualifier block_name
;; {
;;   <define members here>
;; } instance_name;

(defun write-interface-block (storage-qualifier block-name var-strs
                              &key layout instance-name length)
  (assert (not (and length (null instance-name))))
  (format nil "~@[layout(~a) ~]~a ~a~%{~%~{    ~a~%~}}~a;"
          layout
          (string-downcase (symbol-name storage-qualifier))
          (block-name-string block-name)
          var-strs
          (if instance-name
              (format nil " ~a~@[[~a]~]" instance-name
                      (if (and (symbolp length) (string= length '*))
                          ""
                          length))
              "")))

(defun block-memory-layout-string (target-name target-kind layout)
  (assert (typep layout 'qualifier))
  (assert (block-memory-layout-qualfier-p layout) ()
          'unknown-layout-specifier
          :name target-name
          :target-kind target-kind
          :specifier layout)
  (glsl-string layout))

(defun write-ubo-block (storage-qualifier block-name slots layout)
  (format nil "~@[layout(~a) ~]~a ~a~%{~%~{~a~%~}} ~a;"
          (block-memory-layout-string block-name :ubo layout)
          (glsl-string storage-qualifier)
          (format nil "_UBO_~a" block-name)
          (mapcar #'gen-interface-block-slot-string slots)
          block-name))

(defun write-ssbo-block (storage-qualifier block-name slots layout)
  (format nil "~@[layout(~a) ~]~a ~a~%{~%~{~a~%~}} ~a;"
          (block-memory-layout-string block-name :ssbo layout)
          (glsl-string storage-qualifier)
          (format nil "_SSBO_~a" block-name)
          (mapcar #'gen-interface-block-slot-string slots)
          block-name))

(defun gen-interface-block-slot-string (slot)
  (destructuring-bind (slot-name slot-type accessor transform-string) slot
    (declare (ignore transform-string accessor))
    (let ((name slot-name)
          (type-obj slot-type))
      (format nil "    ~{~a ~}~a"
              ;;(loop :for q :in qualifiers :collect (string-downcase (string q)))
              nil
              (format nil "~a ~a;"
                      (v-glsl-string type-obj)
                      (safe-glsl-name-string name))))))

;;----------------------------------------------------------------------

(defun cast-string (type code-obj)
  (when (current-line code-obj)
    (format nil "~a(~a)"
            (v-glsl-string type)
            (current-line code-obj))))

;;----------------------------------------------------------------------

(defun gen-array-literal-string (elements element-type)
  (assert (every λ(v-type-eq element-type (primary-type _)) elements))
  (let ((arr-type (v-array-type-of element-type (length elements) (flow-id!))))
    (format nil "~a(~{~a~^, ~})"
            (v-glsl-string arr-type)
            (mapcar #'current-line elements))))
