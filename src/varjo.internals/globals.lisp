(in-package :varjo.internals)
(in-readtable fn:fn-reader)

(defvar *global-env* :-genv-)
(defvar *global-env-form-bindings* (make-hash-table))
(defvar *global-env-symbol-bindings* (make-hash-table))
(defvar *global-env-compiler-macros* (make-hash-table))

(defvar *supported-versions* '(:140 :150 :330 :400 :410 :420 :430 :440 :450))

(defvar *stage-names*
  '(:vertex
    :tessellation-control
    :tessellation-evaluation
    :geometry
    :fragment))

(defvar *stage-type-names*
  '(vertex-stage
    tessellation-control-stage
    tessellation-evaluation-stage
    geometry-stage
    fragment-stage))

(defvar *unshadowable-names* '(;; special
                               and
                               flet
                               for
                               function
                               vari.cl:glsl-expr
                               if
                               labels
                               vari.cl:labels-no-implicit
                               let
                               multiple-value-bind
                               or
                               progn
                               setq
                               vari.glsl:switch
                               vari.cl:swizzle
                               the
                               values
                               varjo.internals:values-safe
                               vari.glsl:while
                               ;; macros
                               let*
                               prog1
                               setf
                               symbol-macrolet
                               unless
                               when))

(defvar *default-version* :450)


(defvar *ast-node-kinds*
  '(:function-top-level :get :get-stemcell :get-v-value :literal :error :none
    :code-section :funcall :break))

(defvar *stemcell-infer-hook*
  (lambda (name)
    (declare (ignore name))
    nil))

(defvar *constant-inject-hook*
  (lambda (name)
    (declare (ignore name))
    nil))

(defvar *registered-types* nil)

(defvar +ascii-alpha-num+
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defvar *draw-modes*
  '(:dynamic
    :points
    :lines
    :line-loop
    :line-strip
    :lines-adjacency
    :line-strip-adjacency
    :triangles
    :triangle-fan
    :triangle-strip
    :triangles-adjacency
    :triangle-strip-adjacency
    :quads
    :patches))

(defvar *glsl-variables* nil)

(defvar *fallback-block-name* :in_block)
(defvar *in-block-name* "v_in")
(defvar *out-block-name* "v_out")
(defvar *emit-var-name-base* "emit")
(defvar *return-var-name-base* "return")

(defvar *valid-contents-symbols*
  (append (copy-list *supported-versions*)
          (copy-list *draw-modes*)))
