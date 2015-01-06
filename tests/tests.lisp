(in-package :varjo)

(defparameter *env* (make-varjo-environment))

(defun test-translate (in-args uniforms context body)
  (let ((env (make-instance 'environment)))
    (pipe-> (in-args uniforms context body env)
      #'split-input-into-env
      #'process-context
      #'process-in-args
      #'process-uniforms
      #'add-context-glsl-vars
      (equal #'macroexpand-pass
             #'compiler-macroexpand-pass)
      #'compile-pass)))

(defmacro deftestshader (args &body body)
  (destructuring-bind (in-args uniforms context)
      (split-arguments args '(&uniform &context))
    `(test-translate ',in-args ',uniforms ',context '(progn ,@body))))


(defun test1 () (to-block (deftestshader () (let ((a 1))))))
(defun test2 () (to-block (deftestshader () (let ((a nil))))))

(defun test3 () (to-block (deftestshader ((arr (:int 5))) (let ((a arr))))))

(defun test4 () (to-block (deftestshader () (let ((a 1))))))

(defun test5 () (to-block (deftestshader () (%glsl-let (a 1) t))))
(defun test6 () (to-block (deftestshader () (%glsl-let ((a :int)) t))))
(defun test7 () (to-block (deftestshader () (%glsl-let ((a :int) 1) t))))

(defun test8 () (to-block (deftestshader () (%glsl-let ((a (:int 5))) t))))


(defun tests ()
  (format t "~{~a~%~}" (mapcar #'first (list (test5) (test6) (test7) (test8)))))

;; float a[5] = float[](3.4, 4.2, 5.0, 5.2, 1.1);

;; why is b a v-number?
(deftestshader () 
  (wip-let (((a (:int 5)))
            (b 1))
           (+ b 2)))
