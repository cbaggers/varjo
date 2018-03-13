(in-package :varjo.tests)

(5am:def-test emit-0 (:suite emit-tests)
  (finishes-p
   (compile-geom () :410 nil
     (declare (output-primitive :kind :triangle-strip :max-vertices 3))
     (emit () (vec4 1 0 0 1) (vec4 1 0 0 1))
     (emit () (vec4 1 0 0 1) (vec4 1 0 0 1))
     (emit () (vec4 1 0 0 1) (vec4 1 0 0 1))
     (end-primitive)
     (values))))

(5am:def-test emit-1 (:suite emit-tests)
  (finishes-p
   (compile-geom () :410 nil
     (declare (output-primitive :kind :triangle-strip :max-vertices 3))
     (labels ((foo ()
                (emit () (vec4 1 0 0 1) (vec4 1 0 0 1))
                (emit () (vec4 1 0 0 1) (vec4 1 0 0 1))
                (emit () (vec4 1 0 0 1) (vec4 1 0 0 1))
                (values)))
       (foo))
     (end-primitive)
     (values))))
