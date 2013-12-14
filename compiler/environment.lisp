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
(defparameter *global-env-compiler-macros* (make-hash-table))
(defparameter *supported-versions* '(:330 :430 :440))
(defparameter *supported-stages* '(:vertex :geometry :fragment))
(defparameter *default-version* :330)
(defparameter *default-context* '(:330 :vertex))
(defparameter *valid-contents-symbols* (append (copy-list *supported-versions*)
                                               (copy-list *supported-stages*)))

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
   (compiler-macros :initform nil :initarg :compiler-macros :accessor v-compiler-macros)
   (types :initform nil :initarg :types :accessor v-types)
   (context :initform nil :initarg :context :accessor v-context)))

(defun make-varjo-environment (&rest context)
  (make-instance 'environment :context (or context *default-context*)))

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
                 :raw-args (v-raw-in-args env)))

(defmethod clean-environment ((env environment))
  (make-instance 'environment :variables nil :functions nil
                 :macros nil :compiler-macros nil :types nil
                 :context (copy-list (v-context env))))

(defmethod normalize-environment (env &optional modify-env)
  (let ((env (if modify-env env (clone-environment env))))
    (labels ((norm-list (x) (loop :for i :in x :for seen = nil :do
                              (when (not (find (first i) seen :key #'first))
                                (push i seen))
                              :finally (return seen))))
      (setf (v-variables env) (norm-list (v-variables env)))
      (setf (v-functions env) (norm-list (v-functions env)))
      (setf (v-macros env) (norm-list (v-macros env)))
      (setf (v-compiler-macros env) (norm-list (v-compiler-macros env))))))

;;-------------------------------------------------------------------------

(defun context-ok-given-restriction (context restriction)
  (every #'identity
         (loop :for item :in restriction :collect
            (if (listp item)
                (some #'identity (loop :for sub-item :in item :collect
                                    (find sub-item context)))
                (find item context)))))

(defmethod valid-for-contextp ((func list) (env environment))
  (let ((restriction (second func))
        (context (v-context env)))
    (if restriction
        (when (context-ok-given-restriction context restriction) func)
        func)))

(defmethod valid-for-contextp ((func v-function) (env environment))
  (let ((restriction (v-restriction func))
        (context (v-context env)))
    (if restriction
        (when (context-ok-given-restriction context restriction) func)
        func)))

(defun shadow-global-check (name &key (specials t) (macros t) (c-macros t))
  (when (or (and macros (get-macro name *global-env*))
            (and c-macros (get-compiler-macro name *global-env*)))
    (error 'cannot-not-shadow-core))
  (when specials
    (loop :for func :in (get-function name *global-env*)
       :if (and specials (v-special-functionp func))       
       :do (error 'cannot-not-shadow-core)))
  t)

(defun get-version-from-context (env)
  (loop :for item :in (v-context env)
     :if (find item *supported-versions*)
     :return item
     :finally (error 'no-version-in-context env)))

(defun allows-stemcellsp (env)
  (context-ok-given-restriction (v-context env) '(:stemcells)))

;;-------------------------------------------------------------------------

(defmethod add-macro (macro-name (macro function) (context list) 
                      (env (eql :-genv-)) &optional modify-env)
  (declare (ignore modify-env))
  (setf (gethash macro-name *global-env-macros*) `(,macro ,context))
  *global-env*)

(defmethod add-macro (macro-name (macro function) (context list)
                      (env environment) &optional modify-env)
  (let ((env (if modify-env env (clone-environment env))))
    (when (shadow-global-check macro-name)
      (a-remove-all macro-name (v-functions env))
      (a-set macro-name `(,macro ,context) (v-macros env)))
    env))

(defgeneric get-macro (macro-name env))

(defmethod get-macro (macro-name (env (eql :-genv-)))
  (or (gethash (kwd macro-name) *global-env-macros*)
      (gethash macro-name *global-env-macros*)))

(defmethod get-macro (macro-name (env environment))
  (let ((spec (or (a-get1 (kwd macro-name) (v-macros env))
                  (a-get1 macro-name (v-macros env))
                  (gethash macro-name *global-env-macros*))))
    (when (and spec (valid-for-contextp spec env)) 
      (first spec))))

(defmethod v-mboundp (macro-name (env environment))
  (not (null (get-macro macro-name env))))

;;-------------------------------------------------------------------------

(defmethod add-compiler-macro (macro-name (macro function) (context list) 
                      (env (eql :-genv-)) &optional modify-env)
  (declare (ignore modify-env))
  (setf (gethash macro-name *global-env-compiler-macros*) `(,macro ,context))
  *global-env*)

(defmethod add-compiler-macro (macro-name (macro function) (context list)
                      (env environment) &optional modify-env)  
  (let ((env (if modify-env env (clone-environment env))))
    (when (shadow-global-check macro-name :specials nil :macros nil :c-macros t)
      (a-set macro-name `(,macro ,context) (v-compiler-macros env)))
    env))

(defgeneric get-compiler-macro (macro-name env))

(defmethod get-compiler-macro (macro-name (env (eql :-genv-)))
  (or (gethash (kwd macro-name) *global-env-compiler-macros*)
      (gethash macro-name *global-env-compiler-macros*)))

(defmethod get-compiler-macro (macro-name (env environment))
  (let ((spec (or (a-get1 (kwd macro-name) (v-compiler-macros env))
                  (a-get1 macro-name (v-compiler-macros env))
                  (gethash macro-name *global-env-compiler-macros*))))
    (when (and spec (valid-for-contextp spec env)) 
      (first spec))))

(defmethod v-mboundp (macro-name (env environment))
  (not (null (get-compiler-macro macro-name env))))

;;-------------------------------------------------------------------------

;;[TODO] really no better way of doing this?
(defun vtype-existsp (type-name)
  (and type-name (handler-case (progn (typep t type-name) t) (error () nil))))

;;-------------------------------------------------------------------------

(defmethod add-var (var-name (val v-value) (env (eql :-genv-)) 
                    &optional modify-env)
  (declare (ignore modify-env))
  (setf (gethash var-name *global-env-vars*) val)
  *global-env*)

(defmethod add-var (var-name (val v-value) (env environment)
                    &optional modify-env)
  (let ((env (if modify-env env (clone-environment env))))
    (a-add var-name val (v-variables env))
    env))

(defmethod add-vars ((var-names list) (vals list) (env environment)
                     &optional modify-env)
  (let ((env (if modify-env env (clone-environment env))))
    (loop :for name in var-names :for val :in vals :do
       (a-add name val (v-variables env)))
    env))

(defgeneric get-var (var-name env))
(defmethod get-var (var-name (env (eql :-genv-)))
  (gethash var-name *global-env-vars*))

(defmethod get-var (var-name (env environment))
  (or (first (a-get var-name (v-variables env)))
      (get-var var-name *global-env*)))

(defmethod v-boundp (var-name (env environment))
  (not (null (get-var var-name env))))

;;-------------------------------------------------------------------------

(defmethod add-type (type-name (type-obj v-type) (env environment)
                    &optional modify-env)
  (let ((env (if modify-env env (clone-environment env))))
    (a-add type-name type-obj (v-types env))    
    env))

;;-------------------------------------------------------------------------

(defmethod valid-for-contextp ((func v-function) (env environment))
  (let ((restriction (v-restriction func))
        (context (v-context env)))
    (if restriction
        (when (context-ok-given-restriction context restriction) func)
        func)))

(defmethod add-function (func-name (func-spec list) (env (eql :-genv-))
                         &optional modify-env)
  (declare (ignore modify-env))
  (setf (gethash func-name *global-env-funcs*)
        (cons func-spec (gethash func-name *global-env-funcs*)))
  *global-env*)

(defmethod add-function (func-name (func-spec v-function) (env environment)
                         &optional modify-env)
  (let ((env (if modify-env env (clone-environment env))))
    (when (shadow-global-check func-name)
      (a-add func-name func-spec (v-functions env)))
    env))

(defmethod get-function (func-name (env (eql :-genv-)))
  (sort-function-list
   (loop :for func-spec :in (append (gethash func-name *global-env-funcs*)
                                    (gethash (kwd func-name) *global-env-funcs*))
      :collect (func-spec->function func-spec env))))

(defmethod get-function (func-name (env environment))
  (sort-function-list
   (loop :for func :in (append (a-get func-name (v-functions env))
                               (a-get (kwd func-name) (v-functions env))
                               (get-function func-name *global-env*))
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
  (not (null (get-function func-name env))))

(defun func-spec->function (spec env)
  (destructuring-bind (transform arg-spec return-spec context place 
                                 glsl-spec-matching glsl-name required-glsl)
      spec
    (make-instance 'v-function :glsl-string transform 
                   :arg-spec (if (listp arg-spec)
                                 (loop :for spec :in arg-spec :collect
                                    (type-spec->type spec :env env))
                                 arg-spec)
                   :return-spec (if (type-specp return-spec)
                                    (type-spec->type return-spec :env env)
                                    return-spec)
                   :restriction context :place place
                   :required-glsl required-glsl
                   :glsl-spec-matching glsl-spec-matching 
                   :glsl-name glsl-name)))

(defun function->func-spec (func &key required-glsl)
  (let ((arg-spec (v-argument-spec func)))
    (v-make-f-spec (v-glsl-string func)
                   (when (listp arg-spec) 
                     (loop :for a :in arg-spec :collect '?))
                   (when (listp arg-spec) 
                     (loop :for a :in arg-spec :collect (type->type-spec a)))
                   (if (type-specp (v-return-spec func))
                       (type->type-spec (v-return-spec func))
                       (v-return-spec func))
                   :place (v-placep func) 
                   :glsl-spec-matching (v-glsl-spec-matchingp func)
                   :glsl-name (v-glsl-name func)
                   :required-glsl (or required-glsl (v-required-glsl func)))))

(defmethod v-functions ((env (eql :-genv-)))
  (declare (ignore env))
  *global-env-funcs*)

;;-------------------------------------------------------------------------

(defun wipe-global-environment ()
  (loop :for f :being :the :hash-key :of *global-env-funcs* :do
     (remhash f *global-env-funcs*))
  (loop :for f :being :the :hash-key :of *global-env-vars* :do
     (remhash f *global-env-vars*)))
