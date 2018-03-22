(in-package :varjo.tests)
(5am:in-suite symbol-macro-tests)

;;------------------------------------------------------------
;; Tests

(5am:def-test symbol-macros-0 (:suite symbol-macro-tests)
  (glsl-contains-p "vec4\\(10.0f,10.0f,0.0f,0.0f\\)"
    (compile-vert () :410 nil
      (symbol-macrolet ((x 10f0))
        (v! x x 0f0 0f0)))))

(5am:def-test symbol-macros-1 (:suite symbol-macro-tests)
  (glsl-contains-all-p ("vec4\\(10.0f,10.0f,0.0f,0.0f\\)"
                        "vec2\\(X,X\\)")
    (compile-vert () :410 nil
      (symbol-macrolet ((x 10f0))
        (let ((x 20f0))
          (v! x x))
        (v! x x 0f0 0f0)))))

(5am:def-test symbol-macros-2 (:suite symbol-macro-tests)
  (glsl-contains-all-p
      ("vec3\\(float\\(X\\),vec2\\(2.0f,2.0f\\)\\)"
       "vec4\\(float\\(10\\),float\\(10\\),float\\(0\\),float\\(0\\)\\)")
    (compile-vert () :410 nil
      (symbol-macrolet ((x 10))
        (let ((x 20))
          (v! x (symbol-macrolet ((x 2s0))
                  (v! x x))))
        (v! x x 0 0)))))

(5am:def-test symbol-macros-3 (:suite symbol-macro-tests)
  (glsl-contains-all-p ("vec4\\(V.x,V.x,V.x,V.x\\)")
    (compile-vert () :410 nil
      (let ((v (v! 0 0 0 0)))
        (symbol-macrolet ((x (x v)))
          (v! x x x x))))))

(5am:def-test symbol-macros-4 (:suite symbol-macro-tests)
  (glsl-contains-all-p ("vec4\\(V.x,V.x,V.x,V.x\\)"
                        "V.x = 10.0f"
                        "Y = 10.0f")
    (compile-vert () :410 nil
      (let ((v (v! 0 0 0 0)))
        (symbol-macrolet ((x (x v)))
          (let ((y 0s0))
            (setf x 10s0)
            (setf y 10s0))
          (v! x x x x))))))
