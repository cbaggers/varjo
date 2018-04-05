(in-package :varjo.internals)
(in-readtable :fn.reader)

;;----------------------------------------------------------------------

(defclass compiled ()
  ((type-set :initarg :type-set :reader type-set)
   (is-ephemeral :initarg :is-ephemeral)
   (current-line :initarg :current-line :initform "")
   (to-block :initarg :to-block :initform nil :reader to-block)
   (return-set :initarg :return-set :initform nil :reader return-set)
   (emit-set :initarg :emit-set :initform nil :reader emit-set)
   (used-types :initarg :used-types :initform nil :reader used-types)
   (stem-cells :initarg :stemcells :initform nil :reader stemcells)
   (out-of-scope-args :initarg :out-of-scope-args :initform nil
                      :reader out-of-scope-args)
   (pure :initarg :pure :initform nil :reader pure-p)
   (place-tree :initarg :place-tree :initform nil :reader place-tree)))

(defgeneric current-line (code-obj &optional even-when-ephemeral))

;;----------------------------------------------------------------------

(defclass post-compile-process ()
  ((all-functions :initarg :all-functions :accessor all-functions)
   (env :initarg :env :accessor env)
   (stage :initarg :stage :accessor stage)
   (in-decl :initform nil :initarg :in-decl :accessor in-declarations)
   (input-variable-glsl :initarg :input-variable-glsl
                        :accessor input-variable-glsl)
   (output-variable-glsl :initarg :output-variable-glsl
                         :accessor output-variable-glsl)
   (out-set :initarg :out-set :accessor out-set)
   (raw-out-set :initarg :raw-out-set :accessor raw-out-set)
   (out-decl :initform nil :initarg :out-decl :accessor out-declarations)
   (output-variables :initarg :output-variables :accessor output-variables)
   (uniforms :initarg :uniforms :accessor uniforms)
   (shared-decls :initarg :shared-decls :accessor shared-decls)
   (stemcells :initarg :stemcells :accessor stemcells)
   (input-variables :initarg :input-variables :accessor input-variables)
   (used-user-structs :initarg :used-user-structs :accessor used-user-structs)
   (used-external-functions :initarg :used-external-functions
                            :accessor used-external-functions)
   (main-metadata :initarg :main-metadata :accessor main-metadata)
   (primitive-out :initarg :primitive-out :accessor primitive-out)))

(defmethod primitive-in ((pp post-compile-process))
  (primitive-in (stage pp)))

;;----------------------------------------------------------------------

(defclass compiled-function-result ()
  ((function-obj :initarg :function-obj :reader function-obj)
   (glsl-code :initarg :glsl-code :reader glsl-code)
   (signatures :initarg :signatures :reader signatures)
   (used-types :initarg :used-types :reader used-types)
   (stemcells :initarg :stemcells :reader stemcells)
   (return-set :initarg :return-set :reader return-set)
   (emit-set :initarg :emit-set :reader emit-set)
   (top-level-scoped-metadata :initarg :top-level-scoped-metadata
                              :initform nil
                              :reader top-level-scoped-metadata)
   (call-count :initform 0 :initarg :call-count :accessor call-count)
   (calls :initform nil :initarg :calls :accessor calls)
   (inline-candidate :initform nil :initarg :inline-candidate
                     :accessor inline-candidate)))

(defmethod name ((compiled-func compiled-function-result))
  (let ((func (function-obj compiled-func)))
    (assert func () "BUG: compiled-function without v-function~%~a"
            compiled-func)
    (name func)))

;;----------------------------------------------------------------------

(defclass stage ()
  ((input-variables :initarg :input-variables :accessor input-variables)
   (uniform-variables :initarg :uniform-variables :accessor uniform-variables)
   (shared-variables :initarg :shared-variables :accessor shared-variables)
   (context :initarg :context :accessor context)
   (lisp-code :initarg :lisp-code :accessor lisp-code)
   (stemcells-allowed :initarg :stemcells-allowed :accessor stemcells-allowed)
   (previous-stage :initarg :previous-stage :accessor previous-stage
                   :initform nil)
   (primitive-in :initarg :primitive-in :accessor primitive-in)))

(defclass tessellation-stage () ())

(defclass vertex-stage (stage) ())
(defclass tessellation-control-stage (stage tessellation-stage) ())
(defclass tessellation-evaluation-stage (stage tessellation-stage) ())
(defclass geometry-stage (stage) ())
(defclass fragment-stage (stage) ())
(defclass compute-stage (stage) ())

;;----------------------------------------------------------------------
;; Compiler output

(defclass compiled-stage ()
  ((glsl-code :initarg :glsl-code :accessor glsl-code)
   (output-variables :initarg :output-variables :accessor output-variables)
   (starting-stage :initarg :starting-stage :accessor starting-stage)
   (implicit-uniforms :initarg :implicit-uniforms :accessor implicit-uniforms)
   (used-external-functions :initarg :used-external-functions
                            :reader used-external-functions)
   (primitive-out :initarg :primitive-out :accessor primitive-out)))

(defclass compiled-vertex-stage
    (vertex-stage compiled-stage) ())

(defclass compiled-tessellation-control-stage
    (tessellation-control-stage compiled-stage) ())

(defclass compiled-tessellation-evaluation-stage
    (tessellation-evaluation-stage compiled-stage) ())

(defclass compiled-geometry-stage
    (geometry-stage compiled-stage) ())

(defclass compiled-fragment-stage
    (fragment-stage compiled-stage) ())

(defclass compiled-compute-stage
    (compute-stage compiled-stage) ())

(defclass shader-variable ()
  ((name :initarg :name :reader name)
   (glsl-name :initarg :glsl-name :reader glsl-name)
   (type :initarg :type :reader v-type-of)))

(defclass input-variable (shader-variable) ())

(defclass uniform-variable (shader-variable)
  ((glsl-decl :initarg :glsl-decl :reader %glsl-decl)))

(defclass shared-variable (shader-variable) ())

(defclass implicit-uniform-variable (uniform-variable)
  ((cpu-side-transform :initarg :cpu-side-transform
                       :reader cpu-side-transform)))

(defclass output-variable (shader-variable)
  ((location :initarg :location :reader location)
   (block-name :initarg :block-name :reader block-name)))

(defclass qualifier ()
  ((name :initarg :name :reader name)
   (glsl-string :initarg :glsl-string :reader glsl-string)))

(defclass feedback-qualifier (qualifier)
  ((group :initarg :group :reader feedback-group)))

;;----------------------------------------------------------------------

(defclass environment ()
  ((base-env
    :initform nil :initarg :base-env)
   (parent-env
    :initform nil :initarg :parent-env :reader v-parent-env)
   (previous-env-with-form-bindings
    :initform nil
    :initarg :previous-env-with-form-bindings
    :reader v-previous-env-with-form-bindings)
   (context
    :initform nil :initarg :context :reader v-context)
   (symbol-bindings
    :initform nil :initarg :symbol-bindings :reader v-symbol-bindings)
   (form-bindings
    :initarg :form-bindings :reader v-form-bindings)
   (multi-val-base
    :initform nil :initarg :multi-val-base :reader v-multi-val-base)
   (multi-val-safe
    :initform nil :initarg :multi-val-safe :reader v-multi-val-safe)
   (function-scope
    :initform 0 :initarg :function-scope :reader v-function-scope)
   (env-depth
    :initarg :env-depth :reader env-depth)
   (allowed-outer-vars
    :initform nil :initarg :allowed-outer-vars :reader v-allowed-outer-vars)
   (local-metadata :initform (make-hash-table :test #'eql))
   (ext-func-compile-chain :initform nil :initarg :ext-func-compile-chain
                           :reader ext-func-compile-chain)))

(defclass base-environment (environment)
  ((stage
    :initform (error "Varjo: stage is mandatory in environment")
    :initarg :stage :reader stage)
   (expanded-input-variables :initform nil :initarg :expanded-input-variables
                             :accessor expanded-input-variables)
   (uniforms :initform nil :initarg :uniforms :accessor v-uniforms)
   (shared :initform nil :initarg :shared :accessor v-shared)
   (context :initform nil :initarg :context :reader v-context)
   (stemcell->flow-id :initform (make-hash-table :test #'eq)
                      :initarg :stemcell->flow-id)
   (name-map :initform (make-hash-table :test #'equal))
   (compiled-functions :initform (make-hash-table :test #'eq))
   (value-metadata :initform (make-hash-table :test #'eql))
   (stemcells-allowed :initform t :initarg :stemcells-allowed
                      :reader allows-stemcellsp)
   (env-depth :initform 0 :initarg :env-depth :reader env-depth)))

;;----------------------------------------------------------------------

(defclass external-function ()
  ((name :initarg :name :reader name)
   (in-args :initarg :in-args :reader in-args)
   (&rest-pos :initarg :&rest-pos :reader &rest-pos)
   (doc-string :initform nil :initarg :doc-string :reader v-doc-string)
   (uniforms :initarg :uniforms :reader uniforms)
   (code :initarg :code :reader code)
   (glsl-versions :initarg :glsl-versions :reader glsl-versions)
   (declaimed-inline :initform nil :accessor declaimed-inline)))

;;----------------------------------------------------------------------

(defclass flow-identifier ()
  ((ids :initform nil :initarg :ids :reader ids)))

(defclass bare-flow-id ()
  ((val :initarg :val)
   (return-pos :initform 0 :initarg :return-pos)))

;;----------------------------------------------------------------------

(defclass rolling-result ()
  ((remaining-stages :initform *stage-type-names* :initarg :remaining-stages)
   (compiled-stages :initform nil :initarg :compiled-stages)))

;;----------------------------------------------------------------------
;;
;; A match has 2 components to it's score:
;;
;; The primary score measures how many arguments needed casting to match
;; the target signature's type.
;;
;; The secondary-score measures how many superclasses you have to cross
;; to travel from the matched type to the target signature's type

(defclass func-match ()
  ((score :initarg :score :reader score)
   (secondary-score :initarg :secondary-score :reader secondary-score)
   (tertiary-score :initarg :tertiary-score :reader tertiary-score)
   (func :initarg :func :reader func)
   (arguments :initarg :arguments :reader arguments)))

;;----------------------------------------------------------------------

(defclass stemcell ()
  ((name :initarg :name :reader name)
   (string-name :initarg :string-name)
   (type :initarg :type :reader v-type-of)
   (flow-id :initarg :flow-id :reader flow-ids)
   (cpu-side-transform :initarg :cpu-side-transform)))

;;----------------------------------------------------------------------

(defclass v-value ()
  ((type :initarg :type :initform nil :accessor v-type-of)
   (glsl-name :initarg :glsl-name :accessor glsl-name)
   (function-scope :initarg :function-scope :initform 0
                   :accessor v-function-scope)
   (read-only :initarg :read-only :initform nil :reader v-read-only)))

(defclass uninitialized-value (v-value) ())

;;----------------------------------------------------------------------

(defclass captured-var ()
  ((name :initarg :name :reader name)
   (value :initarg :value :reader v-value)
   (origin-env :initarg :origin-env :reader origin-env)))

;;----------------------------------------------------------------------

(defclass v-symbol-macro ()
  ((expansion :initarg :expansion :initform nil :reader expansion)
   (function-scope :initarg :function-scope :initform 0
                   :accessor v-function-scope)))

;;----------------------------------------------------------------------

(defclass v-regular-macro ()
  ((name :initform nil :initarg :name :reader name)
   (args :initform nil :initarg :args :accessor arguments)
   (macro-function :initarg :macro-function :initform nil
                   :reader v-macro-function)
   (function-scope :initarg :function-scope :initform 0
                   :accessor v-function-scope)
   (context :initform nil :initarg :context :reader v-context)))

;;----------------------------------------------------------------------

(defclass v-compiler-macro ()
  ((name :initform nil :initarg :name :reader name)
   (args :initform nil :initarg :args :accessor arguments)
   (&rest-pos :initarg :&rest-pos :reader &rest-pos)
   (context :initform nil :initarg :context :reader v-context)
   (function-scope :initform 0 :accessor v-function-scope :allocation :class)
   (argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (macro-function :initarg :macro-function :initform nil
                   :reader v-macro-function)))

;;----------------------------------------------------------------------

(defclass extended-environment ()
  ((env :initarg :env)))

(defclass expansion-env (extended-environment)
  ((macro-obj :initarg :macro-obj)))

(defclass macro-expansion-environment (expansion-env) ())

(defclass compiler-macro-expansion-environment (expansion-env)
  ((args :initarg :args)))

;;----------------------------------------------------------------------

(defclass typed-external-name ()
  ((type :initarg :type :reader v-type-of)
   (glsl-name :initarg :glsl-name :reader glsl-name)))

;;----------------------------------------------------------------------

(defclass standard-metadata () ())
(defclass standard-scope-metadata (standard-metadata) ())
(defclass standard-value-metadata (standard-metadata) ())

;;-------------------------------------------------------------------------

(defclass primitive () ())

(defclass draw-mode (primitive) ())

(defclass geometry-primitive (primitive) ())
(defclass tessellation-in-primitive (primitive) ())
(defclass tessellation-out-primitive (primitive) ())

(defclass dynamic (draw-mode) ())

(defclass points (draw-mode geometry-primitive tessellation-out-primitive)
  ((lisp-name :initform :points :reader lisp-name)
   (vertex-count :initform 1 :reader vertex-count)
   (glsl-string :initform "points" :reader glsl-string)))

(defclass lines (draw-mode geometry-primitive)
  ((lisp-name :initform :lines :reader lisp-name)
   (vertex-count :initform 2 :reader vertex-count)
   (glsl-string :initform "lines" :reader glsl-string)))

(defclass iso-lines (tessellation-out-primitive)
  ((lisp-name :initform :iso-lines :reader lisp-name)
   (vertex-count :initform 2 :reader vertex-count)
   (glsl-string :initform "iso_lines" :reader glsl-string)))

(defclass line-loop (draw-mode)
  ((lisp-name :initform :line-loop :reader lisp-name)
   (glsl-string :initform "line_loop" :reader glsl-string)))

(defclass line-strip (draw-mode)
  ((lisp-name :initform :line-strip :reader lisp-name)
   (glsl-string :initform "line_strip" :reader glsl-string)))

(defclass lines-adjacency (draw-mode geometry-primitive)
  ((lisp-name :initform :lines-adjacency :reader lisp-name)
   (vertex-count :initform 4 :reader vertex-count)
   (glsl-string :initform "lines_adjacency" :reader glsl-string)))

(defclass line-strip-adjacency (draw-mode)
  ((lisp-name :initform :line-strip-adjacency :reader lisp-name)
   (glsl-string :initform "line_strip_adjacency" :reader glsl-string)))

(defclass triangles (draw-mode geometry-primitive tessellation-out-primitive)
  ((lisp-name :initform :triangles :reader lisp-name)
   (vertex-count :initform 3 :reader vertex-count)
   (glsl-string :initform "triangles" :reader glsl-string)))

(defclass triangle-fan (draw-mode)
  ((lisp-name :initform :triangle-fan :reader lisp-name)
   (glsl-string :initform "triangle_fan" :reader glsl-string)))

(defclass triangle-strip (draw-mode)
  ((lisp-name :initform :triangle-strip :reader lisp-name)
   (glsl-string :initform "triangle_strip" :reader glsl-string)))

(defclass triangles-adjacency (draw-mode geometry-primitive)
  ((lisp-name :initform :triangle-adjacency :reader lisp-name)
   (vertex-count :initform 6 :reader vertex-count)
   (glsl-string :initform "triangles_adjacency" :reader glsl-string)))

(defclass triangle-strip-adjacency (draw-mode)
  ((lisp-name :initform :triangle-strip-adjacency :reader lisp-name)
   (glsl-string :initform "triangle_strip_adjacency" :reader glsl-string)))

(defclass quads (draw-mode tessellation-out-primitive)
  ((lisp-name :initform :quads :reader lisp-name)
   (glsl-string :initform "quads" :reader glsl-string)))

(defclass patches (draw-mode tessellation-in-primitive)
  ((lisp-name :initform :patches :reader lisp-name)
   (vertex-count :initarg :vertex-count :reader vertex-count)))

(defmethod make-load-form ((prim primitive) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(type-of prim)))

(defmethod make-load-form ((prim patches) &optional environment)
  (declare (ignore environment))
  `(make-instance 'patches :vertex-count ,(vertex-count prim)))

(defun primitive-name-to-instance (name)
  (if (listp name)
      (dbind (name length) name
        (assert (and (string= name "PATCH")
                     (integerp length)
                     (> length 1)))
        (make-instance 'patches :vertex-count length))
      (ecase name
        (:dynamic (make-instance 'dynamic))
        (:points (make-instance 'points))
        (:lines (make-instance 'lines))
        (:iso-lines (make-instance 'iso-lines))
        (:line-loop (make-instance 'line-loop))
        (:line-strip (make-instance 'line-strip))
        (:lines-adjacency (make-instance 'lines-adjacency))
        (:line-strip-adjacency (make-instance 'line-strip-adjacency))
        (:triangles (make-instance 'triangles))
        (:triangle-fan (make-instance 'triangle-fan))
        (:triangle-strip (make-instance 'triangle-strip))
        (:triangles-adjacency (make-instance 'triangles-adjacency))
        (:triangle-strip-adjacency (make-instance 'triangle-strip-adjacency))
        (:quads (make-instance 'quads))
        (:patches (error 'underspecified-patch-primitive)))))

(defun valid-primitive-name-p (name)
  (if (listp name)
      (dbind (name length) name
        (and (string= name "PATCH")
             (integerp length)
             (> length 1)))
      (not (null
            (find name
                  '(:dynamic :points :lines :iso-lines
                    :line-loop :line-strip :lines-adjacency
                    :line-strip-adjacency :triangles :triangle-fan
                    :triangle-strip :triangles-adjacency
                    :triangle-strip-adjacency :quads))))))

;;-------------------------------------------------------------------------

(defclass v-function ()
  ((versions :initform nil :initarg :versions :accessor v-versions)
   (argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (&rest-pos :initarg :&rest-pos :reader &rest-pos)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (glsl-name :initarg :glsl-name :accessor glsl-name)
   (return-spec :initform nil :initarg :return-spec :accessor v-return-spec)
   (v-place-index :initform nil :initarg :v-place-index :reader v-place-index)
   (name :initform nil :initarg :name :reader name)
   (implicit-args :initform nil :initarg :implicit-args :reader implicit-args)
   (in-out-args :initform nil :initarg :in-out-args :reader in-out-args)
   ;; as of now the flow-ids of the return are never related to in-arg-flow-ids
   ;; in v-function. We have it here purely to support multiple returns in spec
   ;; functions (when we get around to supporting those)
   (in-arg-flow-ids :initform (error 'flow-ids-mandatory :for :v-function
                                     :primary-type :v-function)
                    :initarg :in-arg-flow-ids :reader in-arg-flow-ids)
   (flow-ids :initform (error 'flow-ids-mandatory :for :v-function
                              :primary-type :v-function)
             :initarg :flow-ids :reader flow-ids)
   (emit-set :initform nil :initarg :emit-set :reader emit-set)
   (pure :initform nil :initarg :pure :reader pure-p)
   (derived-from :initarg :derived-from :initform nil :reader derived-from)))

(defmethod functions ((fn v-function))
  (list fn))

;;------------------------------------------------------------

(defclass v-user-function (v-function)
  ((captured-vars :initform nil :initarg :captured-vars :reader captured-vars)
   (code :initform nil :initarg :code :reader v-code)
   (compiled-result :accessor compiled-result)))

;;------------------------------------------------------------

(defclass return-type-generator () ())

(defclass ret-gen-superior-type (return-type-generator) ())
(defclass ret-gen-nth-arg-type (return-type-generator)
  ((arg-num :initform (error "ret-gen-nth-arg-type - arg-num must be provided")
            :initarg :arg-num
            :reader arg-num)))
(defclass ret-gen-element-of-nth-arg-type (return-type-generator)
  ((arg-num :initform (error "ret-gen-nth-arg-type - arg-num must be provided")
            :initarg :arg-num
            :reader arg-num)))

(defmethod make-load-form ((obj ret-gen-superior-type)
                           &optional environment)
  (declare (ignore environment))
  `(make-instance 'ret-gen-superior-type))

(defmethod make-load-form ((obj ret-gen-nth-arg-type)
                           &optional environment)
  (declare (ignore environment))
  `(make-instance 'ret-gen-nth-arg-type :arg-num ,(arg-num obj)))

(defmethod make-load-form ((obj ret-gen-element-of-nth-arg-type)
                           &optional environment)
  (declare (ignore environment))
  `(make-instance 'ret-gen-element-of-nth-arg-type :arg-num ,(arg-num obj)))
