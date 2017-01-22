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
   (out-vars :initarg :out-vars :initform nil :reader out-vars)
   (used-types :initarg :used-types :initform nil :reader used-types)
   (returns :initarg :returns :initform nil :reader returns)
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
   (in-args :initarg :in-args :accessor in-args)
   (out-vars :initarg :out-vars :accessor out-vars)
   (uniforms :initarg :uniforms :accessor uniforms)
   (stemcells :initarg :stemcells :accessor stemcells)
   (used-types :initarg :used-types :accessor used-types)
   (used-external-functions :initarg :used-external-functions
                            :accessor used-external-functions)
   (used-symbol-macros :initarg :used-symbol-macros
                       :accessor used-symbol-macros)
   (used-macros :initarg :used-macros :accessor used-macros)
   (used-compiler-macros :initarg :used-compiler-macros
                         :accessor used-compiler-macros)))

;;----------------------------------------------------------------------

(defclass compiled-function-result ()
  ((function-obj :initarg :function-obj :reader function-obj)
   (glsl-code :initarg :glsl-code :reader glsl-code)
   (signatures :initarg :signatures :reader signatures)
   (ast :initarg :ast :reader ast)
   (used-types :initarg :used-types :reader used-types)
   (stemcells :initarg :stemcells :reader stemcells)
   (out-vars :initarg :out-vars :reader out-vars)))

;;----------------------------------------------------------------------
;; Compiler output

(defclass varjo-compile-result ()
  ((glsl-code :initarg :glsl-code :accessor glsl-code)
   (out-vars :initarg :out-vars :accessor out-vars)
   (stage-type :initarg :stage-type :accessor stage-type)
   (in-args :initarg :in-args :accessor in-args)
   (uniforms :initarg :uniforms :accessor uniforms)
   (implicit-uniforms :initarg :implicit-uniforms :accessor implicit-uniforms)
   (context :initarg :context :accessor context)
   (allowed-stemcells :initarg :allowed-stemcells :accessor allowed-stemcells)
   (used-external-functions :initarg :used-external-functions
                            :reader used-external-functions)
   (used-macros :initarg :used-macros :reader used-macros)
   (used-compiler-macros :initarg :used-compiler-macros
                         :reader used-compiler-macros)
   (function-asts :initarg :function-asts :reader function-asts)
   (used-symbol-macros :initarg :used-symbol-macros
                       :reader used-symbol-macros)
   (third-party-metadata :initarg :third-party-metadata
                         :initform (make-hash-table)
                         :reader third-party-metadata)))

(defclass implicit-uniform ()
  ((name :initarg :name :reader name)
   (glsl-name :initarg :glsl-name :reader glsl-name)
   (type :initarg :type :reader v-type)
   (cpu-side-transform :initarg :cpu-side-transform :reader cpu-side-transform)
   (glsl-decl :initarg :glsl-decl :reader %glsl-decl)))

;;----------------------------------------------------------------------

(defclass environment ()
  ((parent-env :initform *global-env* :initarg :parent-env :reader v-parent-env)
   (context :initform nil :initarg :context :reader v-context)
   (variables :initform nil :initarg :variables :reader v-variables)
   (functions :initform nil :initarg :functions :reader v-functions)
   (macros :initform nil :initarg :macros :reader v-macros)
   (symbol-macros :initform nil :initarg :symbol-macros :reader v-symbol-macros)
   (compiler-macros
    :initform nil :initarg :compiler-macros :reader v-compiler-macros)
   (multi-val-base
    :initform nil :initarg :multi-val-base :reader v-multi-val-base)
   (multi-val-safe
    :initform nil :initarg :multi-val-safe :reader v-multi-val-safe)
   (function-scope
    :initform 0 :initarg :function-scope :reader v-function-scope)
   (allowed-outer-vars
    :initform nil :initarg :allowed-outer-vars :reader v-allowed-outer-vars)))


(defclass base-environment (environment)
  ((raw-in-args :initform nil :initarg :raw-args :accessor v-raw-in-args)
   (raw-uniforms :initform nil :initarg :raw-uniforms :accessor v-raw-uniforms)
   (raw-context :initform nil :initarg :raw-context :accessor v-raw-context)
   (in-args :initform nil :initarg :in-args :accessor v-in-args)
   (uniforms :initform nil :initarg :uniforms :accessor v-uniforms)
   (context :initform nil :initarg :context :accessor v-context)
   (used-symbol-macros :initform nil :initarg :used-symbol-macros)
   (used-macros :initform nil :initarg :used-macros)
   (used-compiler-macros :initform nil :initarg :used-compiler-macros)
   (stemcell->flow-id :initform nil :initarg :stemcell->flow-id)
   (third-party-metadata :initform (make-hash-table) :initarg
                         :third-party-metadata)
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
  ((name :initarg :name)
   (string-name :initarg :string-name)
   (type :initarg :type)
   (flow-id :initarg :flow-id)
   (cpu-side-transform :initarg :cpu-side-transform)))

;;----------------------------------------------------------------------

(defclass v-value ()
  ((type :initarg :type :initform nil :accessor v-type)
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
   (function-scope :initarg :function-scope :initform 0
                   :accessor v-function-scope)
   (read-only :initarg :read-only :initform nil :reader v-read-only)))

;;----------------------------------------------------------------------

(defclass v-symbol-macro ()
  ((expansion :initarg :expansion :initform nil :reader expansion)
   (function-scope :initarg :function-scope :initform 0
                   :accessor v-function-scope)))

;;----------------------------------------------------------------------

(defclass mval ()
  ((value :initarg :value :reader multi-val-value)
   (qualifiers :initarg :qualifiers :reader multi-val-qualifiers)))

;;----------------------------------------------------------------------

(defclass standard-value-metadata () ())

;;-------------------------------------------------------------------------
