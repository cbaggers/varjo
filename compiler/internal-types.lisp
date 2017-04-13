(in-package :varjo)

(defclass ast-node ()
  ((starting-env :initarg :starting-env :reader ast-starting-env)
   (ending-env :initarg :ending-env :reader ast-ending-env)
   (kind :initarg :kind :reader ast-kind)
   (return-type :initarg :return-type :reader ast-return-type)
   (val-origin :initarg :val-origin :initform :incomplete
               :reader ast-val-origin)
   (parent :initarg :parent :initform :incomplete :reader ast-parent)
   (args :initarg :args :initform nil :reader ast-args)
   (val-origins :initarg :val-origins :initform :incomplete)
   (flow-id-origin :initarg :flow-id-origin :initform :incomplete
                   :reader ast-flow-id-origin)
   (flow-id-origins :initarg :flow-id-origins :initform :incomplete)))

;;----------------------------------------------------------------------

(defclass code ()
  ((type :initarg :type :initform nil :reader code-type)
   (current-line :initarg :current-line :initform "" :reader current-line)
   (signatures :initarg :signatures :initform nil :reader signatures)
   (to-block :initarg :to-block :initform nil :reader to-block)
   (return-set :initarg :return-set :initform nil :reader return-set)
   (used-types :initarg :used-types :initform nil :reader used-types)
   (multi-vals :initarg :multi-vals :initform nil :reader multi-vals)
   (stem-cells :initarg :stemcells :initform nil :reader stemcells)
   (out-of-scope-args :initarg :out-of-scope-args :initform nil
                      :reader out-of-scope-args)
   (mutations :initarg :mutations :initform nil :reader mutations)
   (place-tree :initarg :place-tree :initform nil :reader place-tree)
   (node-tree :initarg :node-tree :initform nil :reader node-tree)))

;;----------------------------------------------------------------------

(defclass post-compile-process ()
  ((main-func :initarg :main-func :accessor main-func)
   (env :initarg :env :accessor env)
   (stage :initarg :stage :accessor stage)
   (in-args :initarg :in-args :accessor in-args)
   (out-vars :initarg :out-vars :accessor out-vars)
   (uniforms :initarg :uniforms :accessor uniforms)
   (stemcells :initarg :stemcells :accessor stemcells)
   (input-variables :initarg :input-variables :accessor input-variables)
   (used-types :initarg :used-types :accessor used-types)
   (used-external-functions :initarg :used-external-functions
                            :accessor used-external-functions)))

;;----------------------------------------------------------------------

(defclass compiled-function-result ()
  ((function-obj :initarg :function-obj :reader function-obj)
   (glsl-code :initarg :glsl-code :reader glsl-code)
   (signatures :initarg :signatures :reader signatures)
   (ast :initarg :ast :reader ast)
   (used-types :initarg :used-types :reader used-types)
   (stemcells :initarg :stemcells :reader stemcells)
   (return-set :initarg :return-set :reader return-set)))

;;----------------------------------------------------------------------
;; Compiler output

(defclass stage ()
  ((input-variables :initarg :input-variables :accessor input-variables)
   (uniform-variables :initarg :uniform-variables :accessor uniform-variables)
   (context :initarg :context :accessor context)
   (lisp-code :initarg :lisp-code :accessor lisp-code)
   (stemcells-allowed :initarg :stemcells-allowed :accessor stemcells-allowed)
   (previous-stage :initarg :previous-stage :accessor previous-stage
                   :initform nil)
   (primitive :initform nil :initarg :primitive :accessor primitive)))

(defclass vertex-stage (stage) ())
(defclass tesselation-control-stage (stage) ())
(defclass tesselation-evaluation-stage (stage) ())
(defclass geometry-stage (stage) ())
(defclass fragment-stage (stage) ())

(defclass multi-stage
    ;; This is an interesting beast, it is not allowed in rolling-translate
    ;; but it is allowed in translate, I think it won't survive the refactor
    ;; as stage specific variables can have the same names and different types
    ;; (and I dont allow dual bindings) but it currently exists to keep the
    ;; behaviour of having one stage that allows any behaviour from any stage
    (vertex-stage
     tesselation-control-stage
     tesselation-evaluation-stage
     geometry-stage
     fragment-stage)
  ())

(defclass varjo-compile-result (stage)
  ((glsl-code :initarg :glsl-code :accessor glsl-code)
   (out-vars :initarg :out-vars :accessor out-vars)
   (stage-type :initarg :stage-type :accessor stage-type)
   (in-args :initarg :in-args :accessor in-args)
   (implicit-uniforms :initarg :implicit-uniforms :accessor implicit-uniforms)
   (used-external-functions :initarg :used-external-functions
                            :reader used-external-functions)
   (function-asts :initarg :function-asts :reader function-asts)))

(defclass shader-variable ()
  ((name :initarg :name :reader name)
   (qualifiers :initform nil :initarg :qualifiers :reader qualifiers)
   (glsl-name :initarg :glsl-name :reader glsl-name)
   (type :initarg :type :reader v-type-of)
   (glsl-decl :initarg :glsl-decl :reader %glsl-decl)))

(defclass input-variable (shader-variable) ())

(defclass uniform-variable (shader-variable) ())

(defclass implicit-uniform-variable (uniform-variable)
  ((cpu-side-transform :initarg :cpu-side-transform :reader cpu-side-transform)))

(defclass output-variable (shader-variable)
  ((location :initarg :location :reader location)))

;;----------------------------------------------------------------------

(defclass environment ()
  ((stage
    :initform (error "Varjo: stage is mandatory in environment")
    :initarg :stage :reader stage)
   (parent-env
    :initform *global-env* :initarg :parent-env :reader v-parent-env)
   (context
    :initform nil :initarg :context :reader v-context)
   (symbol-bindings
    :initform nil :initarg :symbol-bindings :reader v-symbol-bindings)
   (form-bindings
    :initform nil :initarg :form-bindings :reader v-form-bindings)
   (macros
    :initform nil :initarg :macros :reader v-macros)
   (multi-val-base
    :initform nil :initarg :multi-val-base :reader v-multi-val-base)
   (multi-val-safe
    :initform nil :initarg :multi-val-safe :reader v-multi-val-safe)
   (function-scope
    :initform 0 :initarg :function-scope :reader v-function-scope)
   (allowed-outer-vars
    :initform nil :initarg :allowed-outer-vars :reader v-allowed-outer-vars)))


(defclass base-environment (environment)
  ((in-args :initform nil :initarg :in-args :accessor v-in-args)
   (uniforms :initform nil :initarg :uniforms :accessor v-uniforms)
   (context :initform nil :initarg :context :accessor v-context)
   (stemcell->flow-id :initform nil :initarg :stemcell->flow-id)
   (name-map :initform (make-hash-table :test #'equal))
   (compiled-functions :initform (make-hash-table :test #'eq))
   (value-metadata :initform (make-hash-table :test #'eql))
   (stemcells-allowed :initform t :initarg :stemcells-allowed
                      :reader allows-stemcellsp)))

;;----------------------------------------------------------------------

(defclass external-function ()
  ((name :initarg :name :reader name)
   (in-args :initarg :in-args :reader in-args)
   (uniforms :initarg :uniforms :reader uniforms)
   (code :initarg :code :reader code)
   (glsl-versions :initarg :glsl-versions :reader glsl-versions)))

;;----------------------------------------------------------------------

(defclass multi-return-flow-id ()
  ((m-value-ids :initform nil :initarg :m-value-ids :reader m-value-ids)))

(defclass flow-identifier ()
  ((ids :initform nil :initarg :ids :reader ids)))

(defclass bare-flow-id ()
  ((val :initarg :val)
   (return-pos :initform 0 :initarg :return-pos)))

;;----------------------------------------------------------------------

(defclass rolling-result ()
  ((remaining-stages :initform *stage-types* :initarg :remaining-stages)
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
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
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
   (context :initform nil :initarg :context :accessor v-context)))

;;----------------------------------------------------------------------

(defclass v-compiler-macro ()
  ((name :initform nil :initarg :name :reader name)
   (args :initform nil :initarg :args :accessor arguments)
   (context :initform nil :initarg :context :accessor v-context)
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

(defclass mval ()
  ((value :initarg :value :reader multi-val-value)
   (qualifiers :initarg :qualifiers :reader multi-val-qualifiers)))

(defclass return-val ()
  ((type :initarg :type :reader v-type-of)
   (qualifiers :initarg :qualifiers :reader qualifiers)))

(defclass named-return-val (return-val)
  ((glsl-name :initarg :glsl-name :reader glsl-name)))

(defclass external-return-val (return-val)
  ((out-name :initarg :out-name :reader out-name)))

;;----------------------------------------------------------------------

(defclass standard-value-metadata () ())

;;-------------------------------------------------------------------------

(defclass draw-mode () ())
(defclass geometry-primitive () ())

(defclass points (draw-mode geometry-primitive) ())
(defclass lines (draw-mode geometry-primitive) ())
(defclass line-loop (draw-mode) ())
(defclass line-strip (draw-mode) ())
(defclass lines-adjacency (draw-mode geometry-primitive) ())
(defclass line-strip-adjacency (draw-mode) ())
(defclass triangles (draw-mode geometry-primitive) ())
(defclass triangle-fan (draw-mode) ())
(defclass triangle-strip (draw-mode) ())
(defclass triangles-adjacency (draw-mode geometry-primitive) ())
(defclass triangle-strip-adjacency (draw-mode) ())
(defclass quads (draw-mode) ())
(defclass patches (draw-mode) ())

;;-------------------------------------------------------------------------
