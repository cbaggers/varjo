(in-package :varjo)
(in-readtable fn:fn-reader)

;;
;; Spaces
;;

;;-------------------------------------------------------------------------
;; Varjo types

(def-v-type-class v-space (v-ephemeral-type) ())

(def-v-type-class v-svec (v-type)
  ((glsl-string :initform "vec4" :reader v-glsl-string)))

;;-------------------------------------------------------------------------
;; Metadata types

(def-metadata-kind space-meta ()
  uniform-name)

(def-metadata-kind spatial-meta ()
  in-space)

;;-------------------------------------------------------------------------
;; Spatial Vectors

(defun get-space-from-svec (svec-code-obj env)
  (in-space (metadata-for-flow-id 'spatial-meta (flow-ids svec-code-obj) env)))

(defun add-space-to-meta (svec-flow-id space-type env)
  (assert (v-typep space-type 'v-space))
  (setf (metadata-for-flow-id svec-flow-id env)
        (make-instance 'spatial-meta :in-space (get-current-space env))))

(v-defspecial sv! ((x :float) (y :float) (z :float) (w :float))
  :return
  (let ((space (get-symbol-binding '*current-space* nil env)))
    (if space
        (let* ((space-type (v-type space))
               (flow-id (flow-id!))
               (type (type-spec->type 'v-svec flow-id))
               (args (list x y z w)))
          (add-space-to-meta flow-id space-type env)
          (values
           (merge-obs args
                      :type type
                      :current-line (format nil "vec4(~a, ~a, ~a, ~a)"
                                            (current-line x)
                                            (current-line y)
                                            (current-line z)
                                            (current-line w))
                      :to-block (mapcat #'to-block args)
                      :node-tree (ast-node!
                                  'v!
                                  (mapcar #'node-tree args)
                                  type
                                  env
                                  env))
           env))
        (compile-form `(v! ,x ,y ,z ,w) env))))

(defmethod combine-metadata ((meta-a null)
                             (meta-b standard-value-metadata))
  (values nil nil))

(defmethod combine-metadata ((meta-a spatial-meta)
                             (meta-b spatial-meta))
  (let ((space-a (in-space meta-a))
        (space-b (in-space meta-b)))
    (if (eq space-a space-b)
        space-a
        (error "Space Analysis Failed: Could not establish at compile time which
space the resulting svec was in between:
~a
and
~a" space-a space-b))))


;;-------------------------------------------------------------------------
;; Vector Space

(defun get-current-space (env)
  (or (variable-uniform-name '*current-space* env)
      (error "Could not aquire the current-space.")))

(v-defmacro in (&environment env space &body body)
  (assert (variable-is-uniform-p space env) ()
          "The 'in' macros takes the name of a uniform as it's argument. However we found ~a instead."
          space)
  (let* ((vars (variables-in-scope env))
         (svecs (remove-if-not λ(typep (variable-type _ env) 'v-svec) vars))
         (gvecs (mapcar λ(gensym (symbol-name _)) svecs))
         (spaces (mapcar λ(in-space (metadata-for-variable _ 'spatial-meta env))
                         svecs))
         (forms (mapcar λ`(space-boundary-convert
                           (let* ((*current-space* ,_1))
                             ,_))
                        gvecs
                        spaces))
         (pairs (mapcar #'list svecs forms)))
    `(let ,(mapcar #'list gvecs svecs)
       (space-boundary-convert
        (symbol-macrolet ,pairs
          (let ((*current-space* ,space))
            ,@body))))))

(defmacro in (space &body body)
  (declare (ignore space body))
  (error "the 'in' macro can only be used inside shaders"))

(v-defspecial get-transform ((space-a v-space) (space-b v-space))
  :return
  ;;
  ;; when we have to transform from *screen-space* or *ndc-space* we are in
  ;; a more tricky situation as the transform is a function of the vector,
  ;; a matrix alone cannot capture the transform.
  ;; To handle this situation (in a way that won't kill performace) we
  ;; require that the source space (screen or ndc) is explicit, in code, at
  ;; compile time.
  ;;
  (labels ((uname (s)
             (let ((id (flow-ids s)))
               (first (find id (v-uniforms env) :test #'id=
                            :key λ(flow-ids (second _)))))))
    (let* ((from-name (or (uname space-a)
                          (error "get-transform: The first argument doesnt appear to be from a uniform")))
           (to-name (or (uname space-b)
                        (error "get-transform: The first argument doesnt appear to be from a uniform"))))
      (if (or (eq from-name '*screen-space*) (eq from-name '*ndc-space*))
          (error "CEPL: get-transform is not currently supported for transform from *screen-space* or *ndc-space*")
          (values (compile-implicit-mat4 from-name to-name env)
                  env)))))

(v-defspecial space-boundary-convert ((form-obj v-type))
  :return
  (if (v-typep (code-type form-obj) 'v-svec)
      (if (variable-in-scope-p '*current-space* env)
          (let* ((inner-name (get-space-from-svec form-obj env))
                 (outer-name (get-current-space env)))
            (convert-between-spaces form-obj inner-name outer-name env))
          (values
           (let ((ast (node-tree form-obj)))
             (copy-code
              form-obj
              :node-tree (ast-node! 'space-boundary-convert
                                    (list ast)
                                    (ast-return-type ast)
                                    (ast-starting-env ast)
                                    (ast-ending-env ast))))
           env))
      (values form-obj env)))

(defun convert-between-spaces (form-obj from-name to-name env)
  (vbind (obj env)
      (if (or (eq from-name '*screen-space*) (eq from-name '*ndc-space*))
          (inject-clip-or-ndc-reverse-transform form-obj from-name to-name env)
          (inject-regular-space-transform form-obj from-name to-name env))
    ;;
    (add-space-to-meta (flow-ids obj) (variable-type to-name env) env)
    ;;
    (values obj env)))

(defun inject-regular-space-transform (form-obj from-name to-name  env)
  ;; we need to add the transform uniform and get it's name
  (let* ((injected (compile-implicit-mat4 from-name to-name env )))
    ;; and here is the replacement code for our crossing the spaces
    (compile-form `(svec-* ,form-obj ,injected) env)))

(defun compile-implicit-mat4 (from-name to-name env)
  ;; Inject a conversion uniform
  (let ((implicit-uniform-name (symb from-name :-to- to-name :-mat4)))
    (inject-implicit-uniform
     implicit-uniform-name :mat4 env
     `(get-transform ,from-name ,to-name))))

(defun inject-clip-or-ndc-reverse-transform (form-obj from-name to-name env)
  (cond
    ;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    ;; The cases we dont yet handle
    ((eq from-name '*ndc-space*)
     (error 'from-ndc))
    ((eq to-name '*screen-space*)
     (error 'to-ndc-or-screen))
    ((eq to-name '*ndc-space*)
     (error 'to-ndc-or-screen))
    ;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    ;; The case we do handle
    ((eq from-name '*screen-space*)
     (let* ((vpp-name 'viewport-params)
            (injected (inject-implicit-uniform
                       vpp-name :vec4 env `(viewport-params-to-vec4))))
       ;; lets make the code to transform to clip-space
       (let ((code `(screen-space-to-clip-space ,form-obj ,injected)))
         ;; now we have a transform to clip-space, but it is likely we need to
         ;; go further, if that might be the case then tell the next pass that
         ;; we are in clip-space and let the compiler take care of the rest
         (compile-form (if (eq to-name '*clip-space*)
                           `(sv! ,code)
                           `(in *clip-space* (sv! ,code)))
                       env))))
    (t (error "compile bug"))))


;; this func isn't in the ast, which means the build won't
;; stabilize. Add a copy-code and replace the svec-* ast-node
(v-defun svec-* (a b) "(~a * ~a)" (v-mat4 v-svec) 1)
(v-defun svec-* (a b) "(~a * ~a)" (v-svec v-mat4) 0)
(v-defun svec-* (a b) "(~a * ~a)" (v-mat4 :vec4) 1)
(v-defun svec-* (a b) "(~a * ~a)" (:vec4 v-mat4) 0)

(defmethod combine-metadata ((meta-a space-meta)
                             (meta-b space-meta))
  (let ((u-a (uniform-name meta-a))
        (u-b (uniform-name meta-b)))
    (if (eq u-a u-b)
        u-a
        (error "Space Analysis Failed: Could not establish at compile time which
space was returned between:
~a
and
~a" u-a u-b))))

;;-------------------------------------------------------------------------


;; TODO
;;
;; - sv! taking vectors
;; - rename to vec-space

;;-------------------------------------------------------------------------

(deferror from-ndc-or-screen-cpu-side () ()
    "Cepl.Spaces: Limitations in cpu-side space transforms from
                   *screen-space* & *ndc-space*

CEPL's `spaces` feature can transform between most spaces in the graph, however
there are limitations for *screen-space* & *ndc-space*

Transforming from *screen-space* or *ndc-space* back to *clip-space* (or earlier)
is not expressible as a matrix. Instead it is a function using data only[0]
available in the fragment shader (the gl-frag-coord).

CEPL therefore requires you to explicitly specify the variable *screen-space*
or *ndc-space* in your shader code and does not allow you to pass in either as a
uniform[1].

By having this limitation we guarentee we can give you the most performance we
can for these cases.

Notes:
[0] technically we could return a function that you then called with the
    screen-space vec4, however this would mean you couldnt be certain if you
    were going to recieve a matrix or a function requiring more conditionals
    and incurring a performance hit.
    CEPL prefers to optimize for the normal case and clarify the details of the
    edge cases in the documentation (and this error message)

[1] One possible way of avoiding this issue would be to upload a struct that
    contained a matrix or a flag specifying that a [ndc/screen]-space->space function
    should be used. However this would require lots of 'if' statements inside your
    glsl code, which is a great way to destroy the performace of your code.")

(deferror to-ndc-or-screen () ()
    "Cepl.Spaces: Un-implemented Transform Feature

Sorry we do not currently support transforming to *ndc-space* or *screen-space*
This is a gap in the api and will be looked at in future versions.")

(deferror from-ndc () ()
    "Cepl.Spaces: Un-implemented Transform Feature

Sorry we do not currently support transforming from *ndc-space*
This is a gap in the api and will be looked at in future versions.")

;; (defun-g screen-space-to-clip-space ((ss-pos :vec4) (viewport :vec4))
;;   (/ (v! (- (* (v:s~ ss-pos :xy) 2.0)
;; 	    (/ (* (v:s~ viewport :xy) 2.0)
;; 	       (* (v:s~ viewport :zw) 2.0))
;; 	    (v! 1 1))
;; 	 (/ (- (* 2.0 (v:z gl-frag-coord))
;; 	       (near gl-depth-range)
;; 	       (far gl-depth-range))
;; 	    (- (near gl-depth-range) (far gl-depth-range)))
;; 	 1.0)
;;      (v:w gl-frag-coord)))
