(in-package :varjo)
(in-readtable fn:fn-reader)

(defparameter *global-env* :-genv-)
(defparameter *global-env-funcs* (make-hash-table))
(defparameter *global-env-vars* (make-hash-table))
(defparameter *global-env-macros* (make-hash-table))
(defparameter *global-env-symbol-macros* (make-hash-table))
(defparameter *global-env-compiler-macros* (make-hash-table))
(defparameter *supported-versions* '(:330 :430 :440))
(defparameter *supported-stages* '(:vertex :geometry :tesselation-control
				   :tesselation-evaluation :fragment))
(defparameter *supported-draw-modes* '(:points :line-strip :line-loop :lines
                                       :line-strip-adjacency :lines-adjacency
                                       :triangle-strip :triangle-fan :triangles
                                       :triangle-strip-adjacency
                                       :triangles-adjacency :patches))
(defparameter *default-version* :330)
(defparameter *default-context* '(:330 :vertex))
(defparameter *valid-contents-symbols* `(,@(copy-list *supported-versions*)
                                           ,@(copy-list *supported-stages*)
                                           ,@(copy-list *supported-draw-modes*)
                                           :iuniforms :no-iuniforms))

(defparameter *stage-types*
  '(:vertex :geometry :tess-eval :tess-control :fragment))

(defparameter *ast-node-kinds*
  '(:get :get-stemcell :get-v-value :literal :error :none))

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
