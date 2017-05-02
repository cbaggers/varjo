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

(5am:def-test build-20 (:suite build-tests)
  (finishes-p
   (v-compile
    '((tess-level-inner :float) (tess-level-outer :float)
      (projection :mat4) (model->clip :mat4) (normal-mat :mat3)
      (light-position :vec3) (diffuse-material :vec3) (ambient-material :vec3))
    :450
    :draw-mode '(:patch 3)

    :vertex
    '(((position :vec4))
      (values position
       (s~ position :xyz)))

    :tessellation-control
    '(((position (:vec3 3)))
      (declare (varjo::output-patch :vertices 3))
      (let ((tess-level-inner 5f0)
            (tess-level-outer 5f0))
        (when (= gl-invocation-id 0)
          (setf (aref gl-tess-level-inner 0) tess-level-inner
                (aref gl-tess-level-outer 0) tess-level-outer
                (aref gl-tess-level-outer 1) tess-level-outer
                (aref gl-tess-level-outer 2) tess-level-outer)))
      (aref position gl-invocation-id))

    :tessellation-evaluation
    '(((position (:vec3 3)))
      (declare (tessellate-to
                :primitive :triangles
                :spacing :equal
                :order :ccw))
      (let* ((p0 (* (x gl-tess-coord) (aref position 0)))
             (p1 (* (y gl-tess-coord) (aref position 1)))
             (p2 (* (z gl-tess-coord) (aref position 2)))
             (pos (normalize (+ p0 p1 p2))))
        (values
         (* model->clip (v! pos 1))
         pos
         gl-tess-coord)))

    :geometry
    '(((position (:vec3 3)) (patch-distance (:vec3 3)))
      (declare (output-primitive :kind :triangle-strip
                :max-vertices 3))
      (let* ((a (- (aref position 2) (aref position 0)))
             (b (- (aref position 2) (aref position 0)))
             (facet-normal (* normal-mat (normalize (cross a b)))))
        (emit ()
              (gl-position (aref gl-in 0))
              (aref patch-distance 0)
              facet-normal
              (v! 1 0 0))
        (emit ()
              (gl-position (aref gl-in 1))
              (aref patch-distance 1)
              facet-normal
              (v! 0 1 0))
        (emit ()
              (gl-position (aref gl-in 2))
              (aref patch-distance 2)
              facet-normal
              (v! 0 0 1))
        (varjo-lang::end-primitive)
        (values)))

    :fragment
    '(((patch-distance :vec3) (facet-normal :vec3) (tri-distance :vec3))
      (labels ((amplify ((d :float) (scale :float) (offset :float))
                 (let* ((d (+ (* scale d) offset))
                        (d (clamp d 0 1)))
                   (- 1 (expt (* d d -2) 2)))))
        (let* ((light-position (v! 0 1 0))
               (diffuse-material (v! 1 0 0))
               (ambient-material (v! 0.2 0.2 0.2))
               ;;
               (n (normalize facet-normal))
               (l light-position)
               (df (abs (dot n l)))
               (color (+ ambient-material (* df diffuse-material)))
               (d1 (min (min (x tri-distance) (y tri-distance))
                        (z tri-distance)))
               (d2 (min (min (x patch-distance) (y patch-distance))
                        (z patch-distance)))
               (color (* (+ ambient-material (* df diffuse-material))
                         (amplify d1 40 -0.5)
                         (amplify d2 50 -0.5))))
          (v! color 1)))))))

(5am:def-test build-21 (:suite build-tests)
  (finishes-p
   (varjo.tests::compile-vert () :450 nil
     (uint 32)
     (v! 0 0 0 0))))
