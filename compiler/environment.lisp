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


(defmethod add-function (func-name (func-spec list) (env (eql :-genv-)))
  (setf (gethash func-name *global-env-funcs*)
        (cons func-spec (gethash func-name *global-env-funcs*))))

(defmethod add-function (func-name (func-spec v-function) (env environment))
  (setf (gethash func-name (v-functions env))
        (cons func-spec (gethash func-name (v-functions env)))))

;; loop and instanstiate
(defmethod get-function-definitions (func-name (env (eql :-genv-)))
  (append (gethash func-name *global-env-funcs*)
          (gethash (kwd func-name) *global-env-funcs*)))

(defmethod get-function-definitions (func-name (env environment))
  (append (gethash func-name *global-env-funcs*)
          (gethash (kwd func-name) *global-env-funcs*)
          (get-function-definitions func-name *global-env*)))

(defmethod v-functions ((env (eql :-genv-)))
  (declare (ignore env))
  *global-env-funcs*)


(defun wipe-global-environment ()
  (loop :for f :being :the :hash-key :of *global-env-funcs* :do
     (remhash f *global-env-funcs*))
  (loop :for f :being :the :hash-key :of *global-env-external-funcs* :do
     (remhash f *global-env-external-funcs*))
  (loop :for f :being :the :hash-key :of *global-env-vars* :do
     (remhash f *global-env-vars*)))
