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
                (funcall x 10)
                (v! 1 2 3 4))))))

(defun test-2 ()
  (proc-res
   (v-compile
    () :450
    :vertex '(()
              (let ((x #'(sin :float)))
                (let ((y x))
                  (funcall y 10))
                (v! 1 2 3 4))))))

(defun test-3 ()
  (proc-res
   (v-compile
    () :450
    :vertex '(()
              (labels ((foo ((x (function (:float) :float)))
                         x))
                (let ((x #'(sin :float)))
                  (let ((y x))
                    (let ((z (foo y)))
                      (funcall z 10)))
                  (v! 1 2 3 4)))))))
