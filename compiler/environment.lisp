;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :varjo)

(defparameter *global-env* :-genv-)
(defparameter *global-env-external-funcs* (make-hash-table))
(defparameter *global-env-funcs* (make-hash-table))
(defparameter *global-env-vars* (make-hash-table))
(defparameter *global-env-macros* (make-hash-table))

;;-------------------------------------------------------------------------

(defmethod clone-environment ((env (eql :-genv-)))
  (error 'clone-global-env-error))

(defmethod clone-environment ((env environment))
  (make-instance 'environment :variables (copy-list (v-variables env))
                 :functions (copy-list (v-functions env))
                 :macros (copy-list (v-macros env))
                 :types (copy-list (v-types env))
                 :context (copy-list (v-context env))))

;;-------------------------------------------------------------------------

(defmethod add-macro (macro-name (macro function) (env (eql :-genv-)))
  (setf (gethash macro-name *global-env-macros*) macro))

(defmethod add-macro (macro-name (macro function) (env environment))
  (let ((env (clone-environment env)))
    (setf (gethash macro-name (v-macros env)) macro)))

(defmethod add-macros (macro-name (macros list) (env environment))
  (let ((env (clone-environment env)))
    (loop :for macro :in macros :do (setf (gethash macro-name (v-macros env)) macro))))

(defgeneric get-macro (macro-name env))
(defmethod get-macro (macro-name (env (eql :-genv-)))
  (gethash macro-name *global-env-macros*))

(defmethod get-macro (macro-name (env environment))
  (or (gethash macro-name (v-macros env))
      (get-macro macro-name *global-env*)))

(defmethod v-mboundp (macro-name (env environment))
  (not (null (get-macro macro-name env))))

;;-------------------------------------------------------------------------

(defmethod add-var (var-name (val v-value) (env (eql :-genv-)))
  (setf (gethash var-name *global-env-vars*) val))

(defmethod add-var (var-name (val v-value) (env environment))
  (let ((env (clone-environment env)))
    (setf (gethash var-name (v-variables env)) val)))

(defmethod add-vars (var-name (vals list) (env environment))
  (let ((env (clone-environment env)))
    (loop :for val :in vals :do (setf (gethash var-name (v-variables env)) val))))

(defgeneric get-var (var-name env))
(defmethod get-var (var-name (env (eql :-genv-)))
  (gethash var-name *global-env-vars*))

(defmethod get-var (var-name (env environment))
  (or (gethash var-name (v-variables env))
      (get-var var-name *global-env*)))

(defmethod v-boundp (var-name (env environment))
  (not (null (get-var var-name env))))

;;-------------------------------------------------------------------------

(defmethod add-function (func-name (func-spec list) (env (eql :-genv-)))
  (setf (gethash func-name *global-env-funcs*)
        (cons func-spec (gethash func-name *global-env-funcs*))))

(defmethod add-function (func-name (func-spec v-function) (env environment))
  (let ((env (clone-environment env)))
    (setf (gethash func-name (v-functions env))
          (cons func-spec (gethash func-name (v-functions env))))))

(defmethod add-functions (func-name (func-specs list) (env environment))
  (let ((env (clone-environment env)))
    (loop :for func-spec :in func-specs :do
       (setf (gethash func-name (v-functions env))
             (cons func-spec (gethash func-name (v-functions env)))))))

;; loop and instanstiate
(defmethod get-function (func-name (env (eql :-genv-)))
  (mapcar #'func-spec->function (gethash func-name *global-env-funcs*)))

(defmethod get-function (func-name (env environment))  
  (append (loop :for func :in (gethash func-name (v-functions env)) 
             :if (valid-for-contextp func env) :collect func)
          (get-function func-name *global-env*)))

(defmethod v-fboundp (func-name (env environment))
  (not (null (get-function func-name env))))

(defun func-spec->function (spec)
  (destructuring-bind (transform arg-spec return-spec context place) spec
    (make-instance 'v-function :glsl-string transform :arg-spec arg-spec
                   :return-spec return-spec :restriction context :place place)))

(defmethod v-functions ((env (eql :-genv-)))
  (declare (ignore env))
  *global-env-funcs*)

;;-------------------------------------------------------------------------

(defun wipe-global-environment ()
  (loop :for f :being :the :hash-key :of *global-env-funcs* :do
     (remhash f *global-env-funcs*))
  (loop :for f :being :the :hash-key :of *global-env-external-funcs* :do
     (remhash f *global-env-external-funcs*))
  (loop :for f :being :the :hash-key :of *global-env-vars* :do
     (remhash f *global-env-vars*)))
