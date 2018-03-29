(in-package :varjo.internals)
(in-readtable fn:fn-reader)

(defun compile-bool (code env)
  (declare (ignore env))
  (let* ((flow-id (flow-id!))
         (bool (type-spec->type 'v-bool flow-id))
         (type-set (make-type-set bool)))
    (if code
        (make-compiled
         :type-set type-set
         :current-line "true"
         :used-types (list bool)
         :pure t)
        (make-compiled
         :type-set type-set
         :current-line "false"
         :used-types (list bool)
         :pure t))))

(defun get-number-type (x)
  ;; [TODO] How should we specify numbers unsigned?
  (typecase x
    (single-float (type-spec->type 'v-float))
    (double-float (type-spec->type 'v-double))
    (integer (type-spec->type 'v-int))
    (otherwise (error "Varjo: Do not know the type of the number '~s'" x))))

(defun compile-number (code env)
  (declare (ignore env))
  (let* ((flow-id (flow-id!))
         (num-type (set-flow-id (get-number-type code) flow-id))
         (type-set (make-type-set num-type)))
    (make-compiled
     :type-set type-set
     :current-line (gen-number-string code num-type)
     :used-types (list num-type)
     :pure t)))

(defun compile-array-literal (arr env)
  (assert (= (array-rank arr) 1) (arr)
          'multi-dimensional-array
          :dimensions (array-dimensions arr))
  (let* ((len (length arr))
         (elements (map 'list λ(compile-literal _ env) arr))
         (types (mapcar #'primary-type elements))
         (element-type (apply #'find-mutual-cast-type types))
         (array-type (v-array-type-of element-type len (flow-id!)))
         (cast-objs (cast-for-array-literal element-type elements))
         (glsl (gen-array-literal-string cast-objs element-type))
         (type-set (make-type-set array-type)))
    (make-compiled :type-set type-set
                   :current-line glsl
                   :used-types (list element-type)
                   :pure t)))

(defun compile-string-literal (str env)
  (let* ((len-1 (1- (length str)))
         (unsigned (char= #\u (aref str len-1)))
         (str (if unsigned (subseq str 0 len-1) str))
         (is (or (handler-case (parse-integer str)
                   (error () nil))
                 (handler-case (parse-float:parse-float str)
                   (error () nil)))))
    (typecase is
      (integer (if unsigned
                   (compile-form `(glsl-symbols:uint ,is) env)
                   (compile-form is env)))
      (float (if unsigned
                 (error "Varjo: ~s is an invalid float literal" str)
                 (compile-form `(vari.cl:glsl-expr ,str :float) env)))
      (otherwise (error 'invalid-glsl-numeric-literal :str str)))))
