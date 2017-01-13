(in-package :varjo)

(defclass ast-node ()
  ((starting-env :initarg :starting-env :reader ast-starting-env)
   (ending-env :initarg :ending-env :reader ast-ending-env)
   (kind :initarg :kind :reader ast-kind)
   (return-type :initarg :return-type :reader ast-return-type)
   (flow-id :initarg :flow-id :reader ast-flow-id)
   (flow-id-origin :initarg :flow-id-origin :initform :incomplete
                   :reader ast-flow-id-origin)
   (val-origin :initarg :val-origin :initform :incomplete
               :reader ast-val-origin)
   (parent :initarg :parent :initform :incomplete :reader ast-parent)
   (args :initarg :args :initform nil :reader ast-args)
   (val-origins :initarg :val-origins :initform :incomplete)
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
   (injected-uniforms :initarg :injected-uniforms :initform nil
                      :reader injected-uniforms)
   (out-of-scope-args :initarg :out-of-scope-args :initform nil
                      :reader out-of-scope-args)
   (flow-ids :initarg :flow-ids :initform (error 'flow-id-must-be-specified-co)
             :reader flow-ids)
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
   (func-defs-glsl :initarg :func-defs-glsl :accessor func-defs-glsl)
   (used-macros :initarg :used-macros :accessor used-macros)
   (used-compiler-macros :initarg :used-compiler-macros
                         :accessor used-compiler-macros)
   (ast :initarg :ast :reader ast)))

;;----------------------------------------------------------------------

(defclass compiled-function-result ()
  ((function-obj :initarg :function-obj :reader function-obj)
   (glsl-code :initarg :glsl-code :reader glsl-code)
   (signatures :initarg :signatures :reader signatures)
   (ast :initarg :ast :reader ast)
   (used-types :initarg :used-types :reader used-types)
   (injected-uniforms :initarg :injected-uniforms :reader injected-uniforms)
   (stemcells :initarg :stemcells :reader stemcells)
   (out-vars :initarg :out-vars :reader out-vars)))

;;----------------------------------------------------------------------

(defclass varjo-compile-result ()
  ((glsl-code :initarg :glsl-code :accessor glsl-code)
   (out-vars :initarg :out-vars :accessor out-vars)
   (stage-type :initarg :stage-type :accessor stage-type)
   (in-args :initarg :in-args :accessor in-args)
   (uniforms :initarg :uniforms :accessor uniforms)
   (implicit-uniforms :initarg :implicit-uniforms :accessor implicit-uniforms)
   (context :initarg :context :accessor context)
   (used-external-functions :initarg :used-external-functions
                            :reader used-external-functions)
   (used-macros :initarg :used-macros :reader used-macros)
   (used-compiler-macros :initarg :used-compiler-macros
                         :reader used-compiler-macros)
   (ast :initarg :ast :reader ast)
   (used-symbol-macros :initarg :used-symbol-macros
                       :reader used-symbol-macros)
   (third-party-metadata :initarg :third-party-metadata
                         :initform (make-hash-table)
                         :reader third-party-metadata)))

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
   (iuniforms :initform nil :initarg :iuniforms :accessor v-iuniforms)
   (in-args :initform nil :initarg :in-args :accessor v-in-args)
   (uniforms :initform nil :initarg :uniforms :accessor v-uniforms)
   (context :initform nil :initarg :context :accessor v-context)
   (used-external-functions :initform nil :initarg :used-external-functions)
   (used-symbol-macros :initform nil :initarg :used-symbol-macros)
   (used-macros :initform nil :initarg :used-macros)
   (used-compiler-macros :initform nil :initarg :used-compiler-macros)
   (stemcell->flow-id :initform nil :initarg :stemcell->flow-id)
   (third-party-metadata :initform (make-hash-table) :initarg
                         :third-party-metadata)
   (name-map :initform (make-hash-table :test #'equal))
   (compiled-functions :initform (make-hash-table :test #'eq))))

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

(defclass func-match ()
  ((score :initarg :score :reader score)
   (func :initarg :func :reader func)
   (arguments :initarg :arguments :reader arguments)))

;;----------------------------------------------------------------------

(defclass stemcell ()
  ((name :initarg :name)
   (string-name :initarg :string-name)
   (type :initarg :type)
   (flow-id :initarg :flow-id)))

;;----------------------------------------------------------------------

(defclass v-value ()
  ((type :initarg :type :initform nil :accessor v-type)
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
   (function-scope :initarg :function-scope :initform 0
                   :accessor v-function-scope)
   (read-only :initarg :read-only :initform nil :reader v-read-only)
   (flow-ids :initarg :flow-ids :initform (error 'flow-id-must-be-specified-vv)
             :reader flow-ids)))

;;----------------------------------------------------------------------

(defclass mval ()
  ((value :initarg :value :reader multi-val-value)
   (qualifiers :initarg :qualifiers :reader multi-val-qualifiers)))

;;----------------------------------------------------------------------

(defmacro def-v-type-class (name direct-superclass direct-slots &rest options)
  (let ((new-names (if (equal (package-name (symbol-package name)) "VARJO")
                       `(append (list ,(kwd (subseq (symbol-name name) 2))
                                      ',name)
                                *registered-types*)
                       `(cons ',name *registered-types*))))
    `(progn (defclass ,name ,direct-superclass ,direct-slots ,@options)
            (setf *registered-types* (remove-duplicates ,new-names))
            ',name)))

(def-v-type-class v-t-type () ())

(def-v-type-class v-type (v-t-type)
  ((core :initform nil :reader core-typep)
   (glsl-string :initform "<invalid>" :reader v-glsl-string)
   (glsl-size :initform 1)
   (casts-to :initform nil)))

;;----------------------------------------------------------------------
