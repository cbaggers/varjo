(in-package :varjo)

;;
(defun test-it ()
  (let ((foo
         (v-compile
          () :450
          :vertex '(((vert :vec4))
                    (values
                     (v! 1 2 3 4)
                     (v! 1 2)))
          :geometry '(((hm (:vec2 3)))
                      (values
                       (v! 0 0 0 0)
                       (v! 1 1)))
          :fragment '(((a :vec4) (b :vec2))
                      (v! 0 0 0 0)))))
    (values foo (mapcar #'glsl-code foo))))
