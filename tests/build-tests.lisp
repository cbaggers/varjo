(in-package :varjo.tests)
(5am:in-suite build-tests)

(5am:def-test build-0 (:suite build-tests)
  (finishes-p
   (compile-vert () :450 nil
     (v! 0 0 0 0))))

(5am:def-test build-1 (:suite build-tests)
  (finishes-p
   (compile-vert () :450 nil
     (v! 0 0 0 0)
     (v! 0 0 0 0))))

(5am:def-test build-2 (:suite build-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 1))
       (v! 0 x 1 2))
     (v! 0 0 0 0))))

(5am:def-test build-3 (:suite build-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 1)
           (y 2))
       (v! x y 1 2))
     (v! 0 0 0 0))))

(5am:def-test build-4 (:suite build-tests)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test () 1))
       (test))
     (v! 0 0 0 0))))

(5am:def-test build-5 (:suite build-tests)
  (signals varjo-conditions:could-not-find-function
    (compile-vert () :450 nil
      (labels ((test () 1))
        (test))
      (v! 0 (test) 0 0))))

(5am:def-test build-6 (:suite build-tests)
  (signals varjo-conditions:symbol-unidentified
    (compile-vert () :450 nil
      (let ((x 1)
            (y 2))
        (v! 0 x 1 2))
      (v! 0 0 y 0))))

(5am:def-test build-7 (:suite build-tests)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test () 1))
       (v! 0 (test) 0 0)))))

(5am:def-test build-8 (:suite build-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 2))
       (labels ((test () x))
         (v! 0 (test) 0 0))))))

(5am:def-test build-9 (:suite build-tests)
  (signals varjo-conditions:vertex-stage-primary-type-mismatch
    (compile-vert () :450 nil
      ())))

(5am:def-test build-10 (:suite build-tests)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 1)
           (y 2)
           (z 3))
       (v! x y 0 0)))))

(5am:def-test build-11 (:suite build-tests)
  ;; trying to use gl-frag-coord in vertex shader
  (signals varjo-conditions:symbol-unidentified
    (compile-vert (&uniform (iresolution :vec2) (iglobaltime :float)) :450 nil
      (let* (((z :vec2) (/ (* 1.15 (- (* (s~ gl-frag-coord :xy) 2.0)
                                      (s~ iResolution :xy)))
                           (y iResolution)))
             (vtemp (v2! 0.0 1.5708))
             (vtime (v2! (* .05 iGlobalTime)))
             (an  (- (* 0.51 (cos (+ vtemp vtime)))
                     (* 0.25 (cos (+ vtemp vtime vtime)))))
             (f 1e20))
        (for (i 0) (< i 120) (++ i)
             (let ((xz (x z)) (yz (y z)))
               (setf z (+ an (v! (- (* xz xz) (* yz yz))
                                 (* 2.0 xz yz)))
                     f (min f (dot z z)))))
        (setf f (+ 1.0 (/ (log f) 16)))
        (v! f (* f f) (* f f f ) 1.0)))))

(5am:def-test build-12 (:suite build-tests)
  ;; same as build-22 but in fragment shader
  (finishes-p
   (compile-frag (&uniform (iresolution :vec2) (iglobaltime :float)) :450 nil
     (let* (((z :vec2) (/ (* 1.15 (- (* (s~ gl-frag-coord :xy) 2.0)
                                     (s~ iResolution :xy)))
                          (y iResolution)))
            (vtemp (v2! 0.0 1.5708))
            (vtime (v2! (* .05 iGlobalTime)))
            (an  (- (* 0.51 (cos (+ vtemp vtime)))
                    (* 0.25 (cos (+ vtemp vtime vtime)))))
            (f 1e20))
       (for (i 0) (< i 120) (++ i)
            (let ((xz (x z)) (yz (y z)))
              (setf z (+ an (v! (- (* xz xz) (* yz yz))
                                (* 2.0 xz yz)))
                    f (min f (dot z z)))))
       (setf f (+ 1.0 (/ (log f) 16)))
       (v! f (* f f) (* f f f ) 1.0)))))


(5am:def-test build-13 (:suite build-tests)
  ;; same as build-22 but in fragment shader
  (is (finishes-p
       (compile-frag (&uniform (iresolution :vec2) (iglobaltime :float)) :450 nil
         (let* (((z :vec2) (/ (* 1.15 (- (* (s~ gl-frag-coord :xy) 2.0)
                                         (s~ iResolution :xy)))
                              (y iResolution)))
                (vtemp (v2! 0.0 1.5708))
                (vtime (v2! (* .05 iGlobalTime)))
                (an  (- (* 0.51 (cos (+ vtemp vtime)))
                        (* 0.25 (cos (+ vtemp vtime vtime)))))
                (f 1e20))
           (for (i 0) (< i 120) (++ i)
                (let ((xz (x z)) (yz (y z)))
                  (setf z (+ an (v! (- (* xz xz) (* yz yz))
                                    (* 2.0 xz yz)))
                        f (min f (dot z z)))))
           (setf f (+ 1.0 (/ (log f) 16)))
           (v! f (* f f) (* f f f ) 1.0))))))

(5am:def-test build-14 (:suite build-tests)
  (glsl-contains-n-p 1
      "float FOO\\(int X\\);"
    (varjo.tests::compile-vert () :450 nil
      (labels ((foo ((x :int)) (float 10)))
        (v! 0 (foo 0) 0 0)))))

(5am:def-test build-15 (:suite build-tests)
  (finishes-p
   (compile-vert () :450 nil
     (test-ext2 10s0)
     (multiple-value-bind (a b c) (test-ext2 10s0)
       (v! (s~ a :xy) b c)))))

(5am:def-test build-16 (:suite build-tests)
  (finishes-p
   (compile-vert-frag () :450 nil
     (()
      (v! 0 0 0 0))
     (()
      (v! 0 0 0 0)))))

(5am:def-test build-17 (:suite build-tests)
  (finishes-p
   (compile-vert-frag () :450 nil
     (()
      (values (v! 0 0 0 0)
              (v! 1 1)))
     (((tc :vec2))
      (v! tc 0 0)))))

(5am:def-test build-18 (:suite build-tests)
  (signals varjo-conditions::dup-names-in-let
   (compile-vert () :450 nil
     (let ((a 1)
           (a 2))
       (v! 0 0 0 0)))))

(5am:def-test build-19 (:suite build-tests)
  (signals varjo-conditions::dup-names-in-let
   (compile-vert () :450 nil
     (let ((a 1)
           ((a :int) 2))
       (v! 0 0 0 0)))))
