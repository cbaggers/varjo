(in-package :varjo)

(defun proc-res (x)
  (let ((r (first x)))
    (values r (glsl-code r))))

(defun test-0 ()
  (proc-res
   (v-compile
    () :450
    :vertex '(()
              (funcall #'sin 10)
              (v! 1 2 3 4)))))


(defun test-1 ()
  (proc-res
   (v-compile
    () :450
    :vertex '(()
              (let ((x #'(sin :float)))
                (funcall x 10))))))
