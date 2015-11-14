;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :varjo)

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
  ((raw-in-args :initform nil :initarg :raw-args :accessor v-raw-in-args)
   (raw-uniforms :initform nil :initarg :raw-uniforms :accessor v-raw-uniforms)
   (raw-context :initform nil :initarg :raw-context :accessor v-raw-context)
   (in-args :initform nil :initarg :in-args :accessor v-in-args)
   (uniforms :initform nil :initarg :uniforms :accessor v-uniforms)
   (variables :initform nil :initarg :variables :accessor v-variables)
   (functions :initform nil :initarg :functions :accessor v-functions)
   (macros :initform nil :initarg :macros :accessor v-macros)
   (symbol-macros :initform nil :initarg :symbol-macros
                  :accessor v-symbol-macros)
   (compiler-macros :initform nil :initarg :compiler-macros
                    :accessor v-compiler-macros)
   (types :initform nil :initarg :types :accessor v-types)
   (context :initform nil :initarg :context :accessor v-context)
   (multi-val-base :initform nil :initarg :multi-val-base
                   :accessor v-multi-val-base)
   (function-scope :initform 0 :initarg :function-scope
                   :accessor v-function-scope)
   (parent-env :initform *global-env* :initarg :parent-env
	       :reader v-parent-env)))

(defmethod initialize-instance :after ((env environment) &rest initargs)
  (declare (ignore initargs))
  ;; parent checks
  (let ((parent (v-parent-env env)))
    (when parent
      (unless (or (eq parent *global-env*)
		  (equal (v-context env) (v-context parent)))
	(error 'env-parent-context-mismatch :env-a env :env-b parent)))))

(defun %make-varjo-environment ()
  (make-instance 'environment))

(defun make-varjo-environment (&rest context)
  (let ((context (or context *default-context*)))
    (make-instance 'environment :context context :raw-context context)))


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
  `(setf ,list-place (acons ,name
                            (cons ,value (assocr ,name ,list-place))
                            ,list-place)))


(defmacro a-set (name value list-place)
  (let ((g-list-place (gensym "list-place")))
    `(let ((,g-list-place (remove ,name ,list-place :key #'first)))
       (setf ,list-place (acons ,name (list ,value) ,g-list-place)))))

(defmacro a-remove-all (name list-place)
  `(setf ,list-place (remove ,name ,list-place :key #'first)))

;;-------------------------------------------------------------------------

(defmethod clone-environment ((env (eql :-genv-)))
  (error 'clone-global-env-error))

(defmethod clean-environment ((env (eql :-genv-)))
  (error 'clean-global-env-error))

(defmethod clone-environment ((env environment))
  (make-instance 'environment :variables (copy-list (v-variables env))
                 :functions (copy-list (v-functions env))
                 :macros (copy-list (v-macros env))
                 :compiler-macros (copy-list (v-compiler-macros env))
                 :types (copy-list (v-types env))
                 :context (copy-list (v-context env))
                 :in-args (v-in-args env)
                 :uniforms (v-uniforms env)
                 :raw-context (v-raw-context env)
                 :raw-uniforms (v-raw-uniforms env)
                 :raw-args (v-raw-in-args env)
                 :multi-val-base (v-multi-val-base env)
                 :function-scope (v-function-scope env)
		 :parent-env (v-parent-env env)))

(defmethod process-environment-for-main-labels ((env environment))
  (make-instance 'environment :variables nil :functions nil
                 :macros nil :compiler-macros nil :types nil
                 :context (copy-list (v-context env))
                 :function-scope (v-function-scope env)
		 :parent-env (v-parent-env env)))

;; {TODO} just here until if we work out what to do with clean
;;        and clone
(defmethod fresh-environment ((env environment))
  (make-instance 'environment :variables nil :functions nil
                 :macros nil :compiler-macros nil :types nil
                 :context (copy-list (v-context env))
		 :multi-val-base (v-multi-val-base env)
                 :function-scope (v-function-scope env)
		 :parent-env (v-parent-env env)))

(defun merge-env (env new-env)
  (let ((a (clone-environment env))
        (b (clone-environment new-env)))
    (unless (= (v-function-scope a) (v-function-scope b))
      (error 'merge-env-func-scope-mismatch :env-a a :env-b b))
    (unless (eq (v-parent-env a) (v-parent-env b))
      (error 'merge-env-parent-mismatch :env-a a :env-b b))
    (with-slots ((a-vars variables) (a-funcs functions) (a-macros macros)
                 (a-cmacros compiler-macros) (a-types types)) a
      (with-slots ((b-vars variables) (b-funcs functions) (b-macros macros)
                   (b-cmacros compiler-macros) (b-types types)) b
        (setf a-vars (%merge-env-lists a-vars b-vars)
              a-funcs (%merge-env-lists a-funcs b-funcs)
              a-macros (%merge-env-lists a-macros b-macros)
              a-cmacros (%merge-env-lists a-cmacros b-cmacros)
              a-types (%merge-env-lists a-types b-types))))
    a))

(defun %merge-env-lists (a b)
  (reduce #'varjo::%merge-env-lists-item b :initial-value a))

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
    (a-remove-all macro-name (v-functions env))
    (a-set macro-name `(,macro ,context) (v-macros env)))
  env)

(defgeneric get-macro (macro-name env))

(defmethod get-macro (macro-name (env (eql :-genv-)))
  (or (gethash (kwd macro-name) *global-env-macros*)
      (gethash macro-name *global-env-macros*)))

(defmethod get-macro (macro-name (env environment))
  (let ((spec (or (a-get1 (kwd macro-name) (v-macros env))
                  (a-get1 macro-name (v-macros env))
                  (get-macro macro-name (v-parent-env env)))))
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
    (a-remove-all macro-name (v-functions env))
    (a-set macro-name `(,macro ,context) (v-symbol-macros env)))
  env)

(defgeneric get-symbol-macro (macro-name env))

(defmethod get-symbol-macro (macro-name (env (eql :-genv-)))
  (or (gethash (kwd macro-name) *global-env-symbol-macros*)
      (gethash macro-name *global-env-symbol-macros*)))

(defmethod get-symbol-macro (macro-name (env environment))
  (let ((spec (or (a-get1 (kwd macro-name) (v-symbol-macros env))
                  (a-get1 macro-name (v-symbol-macros env))
                  (get-symbol-macro macro-name (v-parent-env env)))))
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
    (a-set macro-name `(,macro ,context) (v-compiler-macros env)))
  env)

(defgeneric get-compiler-macro (macro-name env))

(defmethod get-compiler-macro (macro-name (env (eql :-genv-)))
  (or (gethash (kwd macro-name) *global-env-compiler-macros*)
      (gethash macro-name *global-env-compiler-macros*)))

(defmethod get-compiler-macro (macro-name (env environment))
  (let ((spec (or (a-get1 (kwd macro-name) (v-compiler-macros env))
                  (a-get1 macro-name (v-compiler-macros env))
                  (get-compiler-macro macro-name (v-parent-env env)))))
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

(defmethod add-var (var-name (val v-value) (env environment))
  (a-add var-name val (v-variables env))
  env)

(defmethod add-vars ((var-names list) (vals list) (env environment))
  (loop :for name in var-names :for val :in vals :do
     (a-add name val (v-variables env)))
  env)

(defgeneric get-var (var-name env))
(defmethod get-var (var-name (env (eql :-genv-)))
  (let ((k (gethash (kwd var-name) *global-env-vars*))
	(s (gethash var-name *global-env-vars*)))
    (cond (k (values k *global-env*))
	  (s (values s *global-env*))
	  (t nil))))

(defmethod get-var (var-name (env environment))
  (let ((k (first (a-get (kwd var-name) (v-variables env))))
	(s (first (a-get var-name (v-variables env)))))
    (cond (k (values k env))
	  (s (values s env))
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

(defmethod add-function (func-name (func-spec list) (env (eql :-genv-)))
  (setf (gethash func-name *global-env-funcs*)
        (cons func-spec (gethash func-name *global-env-funcs*)))
  *global-env*)

(defmethod add-function (func-name (func-spec v-function) (env environment))
  (when (shadow-global-check func-name)
    (a-add func-name func-spec (v-functions env)))
  env)

(defmethod get-function-by-name (func-name (env (eql :-genv-)))
  (sort-function-list
   (loop :for func-spec :in (append (gethash func-name *global-env-funcs*)
                                    (gethash (kwd func-name) *global-env-funcs*))
      :collect (func-spec->function func-spec env))))

(defmethod get-function-by-name (func-name (env environment))
  (sort-function-list
   (loop :for func :in (append (a-get func-name (v-functions env))
                               (a-get (kwd func-name) (v-functions env))
                               (get-function-by-name func-name *global-env*))
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
