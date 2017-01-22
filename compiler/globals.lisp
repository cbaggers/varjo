(in-package :varjo)
(in-readtable fn:fn-reader)

(defparameter *global-env* :-genv-)
(defparameter *global-env-form-bindings* (make-hash-table))
(defparameter *global-env-symbol-bindings* (make-hash-table))
(defparameter *global-env-compiler-macros* (make-hash-table))
(defparameter *supported-versions* '(:330 :400 :410 :420 :430 :440 :450))
(defparameter *stage-types* '(:vertex :geometry :tess-eval :tess-control :fragment))
(defparameter *supported-stages* *stage-types*) ;; not supported well but theoretically these are supported :p
(defparameter *supported-draw-modes* '(:points :line-strip :line-loop :lines
                                       :line-strip-adjacency :lines-adjacency
                                       :triangle-strip :triangle-fan :triangles
                                       :triangle-strip-adjacency
                                       :triangles-adjacency :patches))
(defparameter *unshadowable-names* '(;; special
                                     and flet for function glsl-expr if labels
                                     labels-no-implicit let multiple-value-bind
                                     or progn setf-1 setq switch swizzle
                                     symbol-macrolet-1 the values
                                     varjo-lang:values-safe while
                                     ;; macros
                                     let* prog1 setf symbol-macrolet s~ unless
                                     when))
(defparameter *default-version* :330)
(defparameter *default-context* '(:330 :vertex))
(defparameter *valid-contents-symbols* `(,@(copy-list *supported-versions*)
                                           ,@(copy-list *supported-stages*)
                                           ,@(copy-list *supported-draw-modes*)))


(defparameter *ast-node-kinds*
  '(:get :get-stemcell :get-v-value :literal :error :none :code-section
    :funcall :break))

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
