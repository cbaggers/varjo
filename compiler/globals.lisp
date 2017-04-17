(in-package :varjo)
(in-readtable fn:fn-reader)

(defparameter *global-env* :-genv-)
(defparameter *global-env-form-bindings* (make-hash-table))
(defparameter *global-env-symbol-bindings* (make-hash-table))
(defparameter *global-env-compiler-macros* (make-hash-table))

(defparameter *supported-versions* '(:330 :400 :410 :420 :430 :440 :450))

(defparameter *stage-types* '(:vertex
                              :tesselation-control
                              :tesselation-evaluation
                              :geometry
                              :fragment))

(defparameter *supported-stages* *stage-types*)

(defparameter *supported-draw-modes*
  '(:points
    :lines :line-loop :line-strip :lines-adjacency :line-strip-adjacency
    :triangles :triangle-strip :triangle-fan :triangles-adjacency
    :triangle-strip-adjacency
    :patches))

(defparameter *unshadowable-names* '(;; special
                                     and flet for function glsl-expr if labels
                                     labels-no-implicit let multiple-value-bind
                                     or progn setf-1 setq switch swizzle
                                     symbol-macrolet-1 the values
                                     varjo-lang:values-safe while
                                     ;; macros
                                     let* prog1 setf symbol-macrolet s~ unless
                                     when))

(defparameter *default-version* :450)

(defparameter *valid-contents-symbols*
  (append (copy-list *supported-versions*)
          (copy-list *supported-draw-modes*)))


(defparameter *ast-node-kinds*
  '(:function-top-level :get :get-stemcell :get-v-value :literal :error :none
    :code-section :funcall :break))

(defparameter *stemcell-infer-hook*
  (lambda (name)
    (declare (ignore name))
    nil))

(defparameter *constant-inject-hook*
  (lambda (name)
    (declare (ignore name))
    nil))

(defvar *registered-types* nil)

(defvar +ascii-alpha-num+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defparameter *draw-modes*
  '(:points
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
