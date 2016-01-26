;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :varjo)
(in-readtable fn:fn-reader)

(defparameter *global-env* :-genv-)
(defparameter *global-env-funcs* (make-hash-table))
(defparameter *global-env-vars* (make-hash-table))
(defparameter *global-env-macros* (make-hash-table))
(defparameter *global-env-symbol-macros* (make-hash-table))
(defparameter *global-env-compiler-macros* (make-hash-table))
(defparameter *supported-versions* '(:330 :430 :440))
(defparameter *supported-stages* '(:vertex :fragment))
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



;;-------------------------------------------------------------------------

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
    :initform 0 :initarg :function-scope :reader v-function-scope)))

(defclass base-environment (environment)
  ((raw-in-args :initform nil :initarg :raw-args :accessor v-raw-in-args)
   (raw-uniforms :initform nil :initarg :raw-uniforms :accessor v-raw-uniforms)
   (raw-context :initform nil :initarg :raw-context :accessor v-raw-context)
   (in-args :initform nil :initarg :in-args :accessor v-in-args)
   (uniforms :initform nil :initarg :uniforms :accessor v-uniforms)
   (context :initform nil :initarg :context :accessor v-context)
   (function-code-cache :initform (make-hash-table) :reader v-code-cache)
   (used-symbol-macros :initform nil :initarg :used-symbol-macros)
   (used-macros :initform nil :initarg :used-macros)
   (used-compiler-macros :initform nil :initarg :used-compiler-macros)
   (function-dedup :initform nil :initarg :function-dedup)
   (stemcell->flow-id :initform nil :initarg :stemcell->flow-id)
   (third-party-metadata :initform (make-hash-table) :initarg
			 :third-party-metadata)))

(defmethod get-flow-id-for-stem-cell (stem-cell-symbol (e environment))
  (with-slots (stemcell->flow-id) (get-base-env e)
    (or (assocr stem-cell-symbol stemcell->flow-id)
	(let ((flow-id (flow-id!)))
	  (push (cons stem-cell-symbol flow-id) stemcell->flow-id)
	  flow-id))))

(defmethod push-non-implicit-function-for-dedup (code func (e environment))
  (push (cons code func) (slot-value (get-base-env e) 'function-dedup)))

(defmethod dedup-function (code (e environment))
  (cdr (find code (slot-value (get-base-env e) 'function-dedup)
	     :key #'car :test #'equal)))

(defmethod used-symbol-macros ((e environment))
  (slot-value (get-base-env e) 'used-symbol-macros))

(defmethod used-macros ((e environment))
  (slot-value (get-base-env e) 'used-macros))

(defmethod used-compiler-macros ((e environment))
  (slot-value (get-base-env e) 'used-compiler-macros))

;; ugh
(defmethod (setf used-symbol-macros) (val (e environment))
  (setf (slot-value (get-base-env e) 'used-symbol-macros)
	val))

(defmethod (setf used-macros) (val (e environment))
  (setf (slot-value (get-base-env e) 'used-macros)
	val))

(defmethod (setf used-compiler-macros) (val (e environment))
  (setf (slot-value (get-base-env e) 'used-compiler-macros)
	val))

(defun get-base-env (env)
  (let ((parent (v-parent-env env)))
    (if (not (eq parent *global-env*))
	(get-base-env parent)
	env)))

(defmethod v-code-cache ((env environment))
  (v-code-cache (get-base-env env)))

(defmethod v-raw-in-args ((env environment))
  (v-raw-in-args (get-base-env env)))

(defmethod v-raw-uniforms ((env environment))
  (v-raw-uniforms (get-base-env env)))

(defmethod v-raw-context ((env environment))
  (v-raw-context (get-base-env env)))

(defmethod v-in-args ((env environment))
  (v-in-args (get-base-env env)))

(defmethod v-uniforms ((env environment))
  (v-uniforms (get-base-env env)))

(defmethod initialize-instance :after ((env environment) &rest initargs)
  (declare (ignore initargs))
  (unless (every λ(and (symbolp (first _))
		       (every λ(typep _ 'v-value) (rest _)))
		 (v-variables env))
    (error 'invalid-env-vars :vars (v-variables env))))

(defun %make-base-environment (&optional (third-party-metadata
					  (make-hash-table)))
  (make-instance 'base-environment
		 :third-party-metadata third-party-metadata))

;;-------------------------------------------------------------------------
;; global env

(defmethod v-functions ((env (eql :-genv-)))
  (declare (ignore env))
  *global-env-funcs*)

;;-------------------------------------------------------------------------

(defun a-get (name list)
  (assocr name list))

(defun a-get1 (name list)
  (first (assocr name list)))

(defmacro a-add (name value list-place)
  `(acons ,name
	  (cons ,value (assocr ,name ,list-place))
	  ,list-place))


(defmacro a-set (name value list-place)
  (let ((g-list-place (gensym "list-place")))
    `(let ((,g-list-place (remove ,name ,list-place :key #'first)))
       (acons ,name (list ,value) ,g-list-place))))

(defmacro a-remove-all (name list-place)
  `(remove ,name ,list-place :key #'first))

;;-------------------------------------------------------------------------

;; {TODO} this needs a better name, see comment in %make-new-function
(defun process-environment-for-main-labels (env)
  (assert (typep env 'environment))
  (make-instance 'environment
		 :variables (v-variables env)
		 :functions (v-functions env)
                 :macros nil
		 :compiler-macros nil
                 :context (remove :main (copy-list (v-context env)))
                 :function-scope (v-function-scope env)
		 :parent-env (v-parent-env env)))

(defun fresh-environment (env &key context function-scope
				functions macros symbol-macros
				compiler-macros variables
				(multi-val-base nil set-mvb)
				multi-val-safe)
  (assert (typep env 'environment))
  (make-instance 'environment
		 :variables variables
		 :functions functions
                 :macros macros
		 :symbol-macros symbol-macros
		 :compiler-macros compiler-macros
                 :context (or context (copy-list (v-context env)))
		 :multi-val-base (if set-mvb
				     multi-val-base
				     (v-multi-val-base env))
		 :multi-val-safe multi-val-safe
                 :function-scope (or function-scope (v-function-scope env))
		 :parent-env env))

(defmacro with-fresh-env-scope ((name starting-env) &body body)
  (let ((s (gensym "starting-env"))
	(r (gensym "result"))
	(e (gensym "final-env")))
    `(let* ((,s ,starting-env)
	    (,name (fresh-environment ,s)))
       (vbind (,r ,e) (progn ,@body)
	 (values ,r (env-prune (env-depth ,s) ,e))))))

(defun env-replace-parent (env new-parent
			   &key (variables nil variables-set))
  (assert (typep env 'environment))
  (assert (typep new-parent 'environment))
  (make-instance 'environment
		 :variables (if variables-set
				variables
				(v-variables env))
		 :functions (v-functions env)
                 :macros (v-macros env)
		 :symbol-macros (v-symbol-macros env)
		 :compiler-macros (v-compiler-macros env)
                 :context (copy-list (v-context env))
		 :multi-val-base (v-multi-val-base env)
                 :function-scope (v-function-scope env)
		 :parent-env new-parent))

(defun env-depth (env)
  (labels ((dist (e &optional (accum 0))
	     (let ((p (v-parent-env e)))
	       (if (eq p *global-env*) accum (dist p (1+ accum))))))
    (dist env)))

(defun env-prune* (to-depth &rest envs)
  (labels ((%up (e count)
	     (if (> count 0) (%up (v-parent-env e) (1- count)) e))
	   (up (e)
	     (let ((c (- (env-depth e) to-depth)))
	       (assert (>= c 0))
	       (%up e c))))
    (mapcar #'up envs)))

(defun env-prune (to-depth env)
  (first (env-prune* to-depth env)))

(defun env-merge-history (env-a env-b)
  (assert (= (env-depth env-a) (env-depth env-b)))
  (labels ((w (a b)
	     (if (eq a b)
		 a
		 (env-replace-parent
		  a (w (v-parent-env a) (v-parent-env b))
		  :variables (merge-variable-histories a b)))))
    (w env-a env-b)))

(defun merge-variable-histories (env-a env-b)
  ;; we can be sure that both have the var names as assignment
  ;; can only affect flow id, not type
  (let* ((a (v-variables env-a))
	 (v-names (mapcar #'first a)))
    (mapcar
     (lambda (n)
       (let ((va (get-var n env-a))
	     (vb (get-var n env-b)))
	 (if (eq va vb)
	     `(,n ,va)
	     `(,n
	       ,(v-make-value
		 (v-type va)
		 env-a ;; this is ignored as function-scope is provided
		 :read-only (v-read-only va)
		 :function-scope (v-function-scope va)
		 :flow-ids (flow-id! (flow-ids va) (flow-ids vb))
		 :glsl-name (v-glsl-name va))))))
     v-names)))

(defun env-var-names (env &key stop-at-base)
  (labels ((w (e accum)
	     (if (or (eq e *global-env*)
		     (and stop-at-base
			  (typep e 'base-environment)))
		 accum
		 (w (v-parent-env e)
		    (remove-duplicates
		     (append (mapcar #'first (v-variables e))
			     accum)
		     :test #'eq
		     :from-end t)))))
    (w env nil)))

(defun find-env-vars (env-a env-b &key (test #'eq) stop-at-base)
  (let ((n-a (env-var-names env-a :stop-at-base stop-at-base))
	(n-b (env-var-names env-a :stop-at-base stop-at-base)))
    (assert (equal n-a n-b))
    (labels ((v-eq (n) (funcall test (get-var n env-a) (get-var n env-b))))
      (loop for n in n-a if (v-eq n) collect n))))

(defun env-same-vars (env-a env-b)
  (let ((n-a (env-var-names env-a))
	(n-b (env-var-names env-a)))
    (assert (equal n-a n-b))
    (labels ((v-eq (n) (eq (get-var n env-a)
			   (get-var n env-b))))
      (every #'v-eq n-a))))

(defun %same-vars (env-a env-b)
  (let* ((a (v-variables env-a))
	 (b (v-variables env-b))
	 (a-names (mapcar #'first a))
	 (b-names (mapcar #'first b)))
    (and (equal a-names b-names)
	 (every (lambda (n) (eq (get-var n env-a) (get-var n env-b)))
		a-names))))

(defun merge-env (env new-env)
  (unless (= (v-function-scope env) (v-function-scope new-env))
    (error 'merge-env-func-scope-mismatch :env-a env :env-b new-env))
  (with-slots ((a-vars variables) (a-funcs functions) (a-macros macros)
	       (a-cmacros compiler-macros)) env
    (with-slots ((b-vars variables) (b-funcs functions) (b-macros macros)
		 (b-cmacros compiler-macros)) new-env
      (fresh-environment
       env
       :variables (%merge-env-lists a-vars b-vars)
       :functions (%merge-env-lists a-funcs b-funcs)
       :macros (%merge-env-lists a-macros b-macros)
       :compiler-macros (%merge-env-lists a-cmacros b-cmacros)))))

(defun %merge-env-lists (a b)
  (reduce #'%merge-env-lists-item b :initial-value a))

(defun %merge-env-lists-item (a item-to-insert)
  "if item is in A then append its entry to item in A"
  ;; find item in a
  (let* ((pre-exisiting-item (find (first item-to-insert) a :key #'first))
	 (pre-existing-members (rest pre-exisiting-item)))
    (if pre-exisiting-item
        ;; dont insert any item that's already in there
	(let ((to-insert
	       (remove-if (lambda (x) (not (member x pre-existing-members)))
			  (rest item-to-insert))))
	  (cons (cons (first pre-exisiting-item)
		      (append to-insert pre-existing-members))
		(remove (first item-to-insert) a :key #'first))
	  a)
        ;; not found in A so add it
        (cons item-to-insert a))))

;;-------------------------------------------------------------------------

(defun context-ok-given-restriction (context restriction)
  (loop :for item :in restriction :always
     (if (listp item)
         (find-if (lambda (_)
                    (member _ context)) item)
         (find item context))))

(defmethod valid-for-contextp ((func list) (env environment))
  (let ((restriction (second func))
        (context (v-context env)))
    (%valid-for-contextp func restriction context)))

(defmethod valid-for-contextp ((func v-function) (env environment))
  (let ((restriction (v-restriction func))
        (context (v-context env)))
    (%valid-for-contextp func restriction context)))

(defun %valid-for-contextp (func restriction context)
  (if restriction
      (when (context-ok-given-restriction context restriction)
        func)
      func))

(defun shadow-global-check (name &key (specials t) (macros t) (c-macros t))
  (when (or (and macros (get-macro name *global-env*))
            (and c-macros (get-compiler-macro name *global-env*)))
    (error 'cannot-not-shadow-core))
  (when specials
    (loop :for func :in (get-function-by-name name *global-env*)
       :if (and specials (v-special-functionp func))
       :do (error 'cannot-not-shadow-core)))
  t)

(defun get-version-from-context (env)
  (loop :for item :in (v-context env)
     :if (find item *supported-versions*)
     :return item
     :finally (error 'no-version-in-context env)))

(defun get-stage-from-env (env)
  (get-version-from-context (v-context env)))

(defun get-stage-from-context (context)
  (find-if (lambda (x) (member x *supported-stages*)) context))

;;{TODO} move errors to correct place
(let ((prims '(:points :line_strip :line_loop :lines :triangle_strip
               :triangle_fan :triangles)))
  (defun get-primitive-type-from-context (context)
    (or (loop :for i :in context :if (member i prims) :return i)
        :triangles))
  ;; (defun get-primitive-length (prim-type)
  ;;   (let ((pos (position prim-type prims)))
  ;;     (if pos
  ;;         (1+ pos)
  ;;         (error "Varjo: Not a valid primitive type"))))
  )

(defun allows-stemcellsp (env)
  (context-ok-given-restriction (v-context env) '(:iuniforms)))

;;-------------------------------------------------------------------------

(defmethod add-macro (macro-name (macro function) (context list)
                      (env (eql :-genv-)))
  (setf (gethash macro-name *global-env-macros*) `(,macro ,context))
  *global-env*)

(defmethod add-macro (macro-name (macro function) (context list)
                      (env environment))
  (when (shadow-global-check macro-name)
    (let* ((funcs (a-remove-all macro-name (v-functions env)))
	   (macros (a-set macro-name `(,macro ,context) (v-macros env))))
      (fresh-environment env :functions funcs :macros macros))))

(defgeneric get-macro (macro-name env))

(defmethod get-macro (macro-name (env (eql :-genv-)))
  (gethash macro-name *global-env-macros*))

(defmethod %get-macro-spec (macro-name (env (eql :-genv-)))
  (get-macro macro-name env))

(defmethod %get-macro-spec (macro-name (env environment))
  (or (a-get1 macro-name (v-macros env))
      (%get-macro-spec macro-name (v-parent-env env))))

(defmethod get-macro (macro-name (env environment))
  (let ((spec (%get-macro-spec macro-name env)))
    (when (and spec (valid-for-contextp spec env))
      (first spec))))

(defmethod v-mboundp (macro-name (env environment))
  (not (null (get-macro macro-name env))))

;;-------------------------------------------------------------------------



(defmethod add-symbol-macro (macro-name macro (context list)
                      (env (eql :-genv-)))
  (unless (or (listp macro) (symbolp macro) (numberp macro))
    (error 'invalid-symbol-macro-form :name macro-name :form macro))
  (setf (gethash macro-name *global-env-symbol-macros*) `(,macro ,context))
  *global-env*)

(defmethod add-symbol-macro (macro-name macro (context list)
                             (env environment))
  (unless (or (listp macro) (symbolp macro) (numberp macro))
    (error 'invalid-symbol-macro-form :name macro-name :form macro))
  (when (shadow-global-check macro-name)
    (let ((funcs (a-remove-all macro-name (v-functions env)))
	  (sym-macros (a-set macro-name
			     `(,macro ,context)
			     (v-symbol-macros env))))
      (fresh-environment env :functions funcs :symbol-macros sym-macros))))

(defgeneric get-symbol-macro (macro-name env))

(defmethod get-symbol-macro (macro-name (env (eql :-genv-)))
  (gethash macro-name *global-env-symbol-macros*))

(defmethod %get-symbol-macro-spec (macro-name (env (eql :-genv-)))
  (get-symbol-macro macro-name env))

(defmethod %get-symbol-macro-spec (macro-name (env environment))
  (or (a-get1 macro-name (v-symbol-macros env))
      (%get-symbol-macro-spec macro-name (v-parent-env env))))

(defmethod get-symbol-macro (macro-name (env environment))
  (let ((spec (%get-symbol-macro-spec macro-name env)))
    (when (and spec (valid-for-contextp spec env))
      spec)))

(defmethod v-mboundp (macro-name (env environment))
  (not (null (get-symbol-macro macro-name env))))


;;-------------------------------------------------------------------------

(defmethod add-compiler-macro (macro-name (macro function) (context list)
                               (env (eql :-genv-)))
  (setf (gethash macro-name *global-env-compiler-macros*) `(,macro ,context))
  *global-env*)

(defmethod add-compiler-macro (macro-name (macro function) (context list)
                               (env environment))
  (when (shadow-global-check macro-name :specials nil :macros nil :c-macros t)
    (let ((c-macros
	   (a-set macro-name `(,macro ,context) (v-compiler-macros env))))
      (fresh-environment env :compiler-macros c-macros))))

(defgeneric get-compiler-macro (macro-name env))

(defmethod get-compiler-macro (macro-name (env (eql :-genv-)))
  (gethash macro-name *global-env-compiler-macros*))

(defmethod %get-compiler-macro-spec (macro-name (env (eql :-genv-)))
  (get-compiler-macro macro-name env))

(defmethod %get-compiler-macro-spec (macro-name (env environment))
  (or (a-get1 macro-name (v-compiler-macros env))
      (%get-compiler-macro-spec macro-name (v-parent-env env))))

(defmethod get-compiler-macro (macro-name (env environment))
  (let ((spec (%get-compiler-macro-spec macro-name env)))
    (when (and spec (valid-for-contextp spec env))
      (first spec))))

(defmethod v-mboundp (macro-name (env environment))
  (not (null (get-compiler-macro macro-name env))))

;;-------------------------------------------------------------------------

;;[TODO] really no better way of doing this?
(defun vtype-existsp (type-name)
  (and type-name
       (handler-case (progn (typep t type-name) t) (error () nil))
       (handler-case (progn (or (typep (make-instance type-name) 'v-t-type)
				(typep (make-instance type-name) 'v-spec-type))
			    t)
	 (error () nil))))

;;-------------------------------------------------------------------------

(defmethod add-var (var-name (val v-value) (env (eql :-genv-)))
  (setf (gethash var-name *global-env-vars*) val)
  *global-env*)

(defmethod %add-var (var-name (val v-value) (env base-environment))
  (setf (slot-value env 'variables)
	(a-add var-name val (v-variables env))))

(defmethod add-var (var-name (val v-value) (env environment))
  (fresh-environment env :variables (a-add var-name val (v-variables env))))

(defgeneric get-var (var-name env))
(defmethod get-var (var-name (env (eql :-genv-)))
  (let ((s (gethash var-name *global-env-vars*)))
    (cond (s (values s *global-env*))
	  (t nil))))

(defmethod get-var (var-name (env environment))
  (let ((s (first (a-get var-name (v-variables env)))))
    (cond (s (values s env))
	  (t (get-var var-name (v-parent-env env))))))

(defmethod v-boundp (var-name (env environment))
  (not (null (get-var var-name env))))

;;-------------------------------------------------------------------------

(defmethod valid-for-contextp ((func v-function) (env environment))
  (let ((restriction (v-restriction func))
        (context (v-context env)))
    (if restriction
        (when (context-ok-given-restriction context restriction) func)
        func)))

(defmethod valid-for-contextp ((func v-function) (env (eql *global-env*)))
  (let ((restriction (v-restriction func))
        (context (v-context env)))
    (if restriction
        (when (context-ok-given-restriction context restriction) func)
        func)))

(defmethod add-equivalent-name (existing-name new-name)
  (let ((f (get-function-by-name existing-name *global-env*))
	(c (get-compiler-macro existing-name *global-env*))
	(m (get-macro existing-name *global-env*)))
    (cond
      ((or f c)
       (when f
	 (setf (gethash new-name *global-env-funcs*)
	       (gethash existing-name *global-env-funcs*)))
       (when c
	 (setf (gethash new-name *global-env-compiler-macros*)
	       (gethash existing-name *global-env-compiler-macros*))))
      (m (setf (gethash new-name *global-env-macros*)
	       (gethash existing-name *global-env-macros*)))
      (t (error 'could-not-find-any :name existing-name))))
  new-name)

(defmethod add-function (func-name (func-spec list) (env (eql :-genv-)))
  (setf (gethash func-name *global-env-funcs*)
        (cons func-spec (gethash func-name *global-env-funcs*)))
  *global-env*)

(defmethod add-function (func-name (func-spec v-function) (env environment))
  (when (shadow-global-check func-name)
    (fresh-environment env :functions (a-add func-name func-spec (v-functions env)))))

(defmethod %add-function (func-name (func-spec v-function)
			  (env base-environment))
  (when (shadow-global-check func-name)
    (setf (slot-value env 'functions)
	  (a-add func-name func-spec (v-functions env)))))

(defmethod get-function-by-name (func-name (env (eql :-genv-)))
  (sort-function-list
   (loop :for func-spec :in (gethash func-name *global-env-funcs*)
      :collect (func-spec->function func-spec env))))

(defmethod %get-functions-by-name (func-name (env (eql :-genv-)))
  (get-function-by-name func-name env))

(defmethod %get-functions-by-name (func-name (env environment))
  (append (a-get func-name (v-functions env))
	  (%get-functions-by-name func-name (v-parent-env env))))

(defmethod get-function-by-name (func-name (env environment))
  (sort-function-list
   (loop :for func :in (%get-functions-by-name func-name env)
      :if (and func (valid-for-contextp func env)) :collect func)))

(defmethod special-raw-argp ((func v-function))
  (eq (v-argument-spec func) t))

(defmethod special-func-argp ((func v-function))
  (functionp (v-argument-spec func)))

(defmethod special-basic-argp ((func v-function))
  (listp (v-argument-spec func)))

(defmethod func-need-arguments-compiledp ((func v-function))
  (not (and (v-special-functionp func) (special-raw-argp func))))

(defun sort-function-list (func-list)
  (sort (copy-list func-list) #'< :key #'func-priority-score))

(defun func-priority-score (func)
  (if (v-special-functionp func)
      (cond ((special-raw-argp func) 0)
            ((special-func-argp func) 1)
            ((special-basic-argp func) 2))
      (if (v-glsl-spec-matchingp func) 3 4)))

(defmethod v-fboundp (func-name (env environment))
  (not (null (get-function-by-name func-name env))))

;;-------------------------------------------------------------------------

(defun wipe-global-environment ()
  (loop :for f :being :the :hash-key :of *global-env-funcs* :do
     (remhash f *global-env-funcs*))
  (loop :for f :being :the :hash-key :of *global-env-vars* :do
     (remhash f *global-env-vars*)))
