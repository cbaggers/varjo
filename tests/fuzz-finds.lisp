
;; <invalid> in glsl
(glsl-code
 (compile-vert nil :450 nil
   (LET ((FUZZ
          (IF (= 1 2)
              (D-FDY 1f0)
              (TANH GL-SAMPLE-POSITION))))
     (v! 1 2 3 4))))
