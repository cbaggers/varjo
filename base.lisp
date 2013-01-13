 ;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

;;------------------------------------------------------------
;; Handy Functions
;;-----------------

(defun kwd (name) 
  (intern (string name) 'keyword))

(defun print-hash (hash-table)
  (loop for x being the hash-keys of hash-table
     :do (print (format nil "~s -> ~s" x (gethash x hash-table))))
  hash-table)

(defun printf (control-string &rest format-arguments)
  (apply #'format (append (list t control-string) format-arguments)))

;;------------------------------------------------------------
;; Code Class
;;------------

(defclass code ()
  ((type-spec
    :initarg :type
    :initform nil
    :reader code-type
    :writer (setf code-type))
   (current-line
    :initarg :current-line
    :initform nil
    :reader current-line
    :writer (setf current-line))
   (to-block
    :initarg :to-block
    :initform nil
    :reader to-block
    :writer (setf to-block))
   (to-top
    :initarg :to-top
    :initform nil
    :reader to-top
    :writer (setf to-top))))

(defmethod initialize-instance :after ((code-ob code) &key type code)
  (if (not (and type code))
      (error "Type and Code Content must be specified when creating an instance of varjo:code"))
  (setf (slot-value code-ob 'type-spec) (flesh-out-type type)
        (slot-value code-ob 'current-line) code))

;;------------------------------------------------------------
;; GLSL Types
;;------------

(defun flesh-out-type (type)
  (if (> (length type) 3)
      (error "Invalid GLSL Type Definition: ~s has more than 3 components." type)
      (append type (make-list (- 3 (length type))))))

;; [TODO] Checking length should be a '<' not an eq 
(defun glsl-valid-type (candidate spec)
  (labels ((component-valid-p (c s) 
	     (or (and (null c) (null s))
		 (if (listp s) (find c s) (eq c s))
		 (eq t s))))
    (every #'component-valid-p candidate spec)))

(defun glsl-typep (object type)
  (glsl-valid-type (code-type object) type))

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun func-in-spec (x)
  (first x))

(defun func-out-spec (x)
  (second x))

(defun func-body (x)
  (third x))

(defun glsl-function (symbol)
  (gethash symbol *glsl-functions*))

(defun glsl-valid-function-args (func args)
  (let ((in-spec (func-in-spec func)))
    (and (eq (length args) (length in-spec))
	 (every #'(lambda (c s) (glsl-typep c s)) 
		args in-spec))))

(defun glsl-resolve-func-type (func args)
  (let ((in-types (mapcar #'code-type args)))
    (loop :for i in (func-out-spec func)
	  :for part from 0
	  :collect (if (numberp i)
		       (nth part (nth i in-types))
		       i))))


(defun glsl-resolve-oper-type (oper args)
  (declare (ignore oper))
  (let ((in-types (mapcar #'code-type args)))
    (declare (ignore in-types))
    `(:implement :resolve-oper-type :now)))
