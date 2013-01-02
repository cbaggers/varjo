;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :umbra)

(defparameter *glsl-functions* (make-hash-table))
(defparameter *glsl-substitutions* (make-hash-table))
(defparameter *glsl-operators* '((+ . t) (- . t) (~ . t) (! . t) 
				 (*) (/) (>>) (<<) (<>) (>=) (<=)
				 (==) (!=) (&) (^) (\|) (&&) (^^)
				 (||)))

(defun glsl-defun (&key name in-args output-type transform
		     (blockp nil))
  (let* ((func-spec (list (mapcar #'flesh-out-type
				  (mapcar #'second in-args))
			  (flesh-out-type output-type)
			  transform
			  blockp)))
    (setf (gethash name *glsl-functions*) 
          (cons func-spec (gethash name *glsl-functions*)))))

(defun glsl-operatorp (symbol) 
  (not (null (assoc symbol *glsl-operators*))))

(defun glsl-unary-operator (symbol)
  (cdr (assoc symbol *glsl-operators*)))

;;------------------------------------------------------------
;; Core Language Definitions
;;---------------------------

(glsl-defun :name 'degrees
            :in-args '((radians ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "degrees(~a)")

(glsl-defun :name 'radians
            :in-args '((degrees ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "radians(~a)")

(glsl-defun :name 'sin
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "sin(~a)")

(glsl-defun :name 'cos
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "cos(~a)")

(glsl-defun :name 'tan
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "tan(~a)")

(glsl-defun :name 'asin
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "asin(~a)")

(glsl-defun :name 'acos
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "acos(~a)")

(glsl-defun :name 'atan
            :in-args '((y ((:float :vec2 :vec3 :vec4)))
		       (x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "atan(~a, ~a)")

(glsl-defun :name 'atan
            :in-args '((y-over-x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "atan(~a)")

(glsl-defun :name 'sinh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "sinh(~a)")

(glsl-defun :name 'cosh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "cosh(~a)")

(glsl-defun :name 'tanh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "tanh(~a)")

(glsl-defun :name 'asinh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "asinh(~a)")

(glsl-defun :name 'acosh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "acosh(~a)")

(glsl-defun :name 'atanh
            :in-args '((angle ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "atanh(~a)")

(glsl-defun :name 'pow
            :in-args '((x ((:float :vec2 :vec3 :vec4)))
		       (y ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "pow(~a, ~a)")

(glsl-defun :name 'exp
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "exp(~a)")

(glsl-defun :name 'log
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "log(~a)")

(glsl-defun :name 'exp2
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "exp2(~a)")

(glsl-defun :name 'log2
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "log2(~a)")

(glsl-defun :name 'sqrt
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "exp(~a)")

(glsl-defun :name 'inversesqrt
            :in-args '((x ((:float :vec2 :vec3 :vec4))))
            :output-type '(0 nil nil)
            :transform "inversesqrt(~a)")

;;------------------------------------------------------------
;; Operator Definitions
;;----------------------

;;------------------------------------------------------------
;; Lisp Function Substitutions
;;-----------------------------

