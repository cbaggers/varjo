(in-package :varjo.tests)

;;------------------------------------------------------------
;; Helper macros

(defmacro compile-vert (args version allow-stemcells &body body)
  (destructuring-bind (in-args uniforms) (split-arguments args '(&uniform))
    `(first
      (v-compile ',uniforms ,version
                 :vertex '(,in-args ,@body)
                 :allow-stemcells ,allow-stemcells))))

(defmacro compile-frag (args version allow-stemcells &body body)
  (destructuring-bind (in-args uniforms) (split-arguments args '(&uniform))
    `(first
      (v-compile ',uniforms ,version
                 :fragment '(,in-args ,@body)
                 :allow-stemcells ,allow-stemcells))))

(defmacro compile-vert-frag (uniforms version allow-stemcells &body body)
  `(v-compile ',uniforms ,version
              :vertex ',(first body)
              :fragment ',(second body)
              :allow-stemcells ,allow-stemcells))

(defun ast-stabalizes-p (compile-result &optional (depth 0) (max-depth 20))
  "Returns t if compile the ast->code of compile-result gives the same ast
   It is allowed to recompile up to 'max-depth' times in order to find
   convergence"
  (let* ((code (ast->code compile-result))
         (version (varjo::get-version-from-context-list
                   (context compile-result)))
         (stemcells (allowed-stemcells compile-result))
         (recomp (first (v-compile (uniforms compile-result) version
                                   (stage-type compile-result)
                                   (list (in-args compile-result)
                                         code)
                                   :allow-stemcells stemcells)))
         (recomp-code (ast->code recomp)))
    (or (values (equal code recomp-code) depth)
        (when (< depth max-depth)
          (ast-stabalizes-p recomp (incf depth))))))

(defmacro finishes-p (form)
  (alexandria:with-gensyms (res)
    `(let ((,res (varjo::listify ,form)))
       (is (every (lambda (x)
                    (and (typep x 'varjo-compile-result)
                         (ast-stabalizes-p x)))
                  ,res)))))

(defmacro glsl-contains-p (regex &body form)
  (assert (= 1 (length form)))
  `(is (cl-ppcre:all-matches ,regex (glsl-code ,(first form)))))

(defmacro glsl-doesnt-contain-p (regex &body form)
  (assert (= 1 (length form)))
  `(is (null (cl-ppcre:all-matches ,regex (glsl-code ,(first form))))))

(defmacro glsl-contains-n-p (n regex &body form)
  (assert (= 1 (length form)))
  (alexandria:with-gensyms (count matches)
    `(let ((,count ,n)
           (,matches (cl-ppcre:all-matches-as-strings
                     ,regex
                     (glsl-code ,(first form)))))
       (is (= ,count (length ,matches))))))

;;------------------------------------------------------------
;; {TODO} turn this into a test (for testing definition of things)
;;        use compile.

(defvar *initd* nil)

(defun init (&optional force)
  (when (or (not *initd*) force)
    (varjo:add-external-function
     'test-ext '((ham :float)) nil
     `((progn (v! ham ham ham ham))))
    (varjo:add-external-function
     'test-ext2 '((ham :float)) nil
     `((progn (values (v! ham ham ham ham)
                      2
                      3))))
    (varjo:add-external-function
     'test-ext3 '((ham :float)) nil
     `((let ((jam (test-ext ham)))
         jam)))
    (setf *initd* t)))

(init)

#+nil
(init t)

;;------------------------------------------------------------
;; Helper structs

(v-defstruct pos-rot ()
  (pos :vec3)
  (rot :vec4))

(v-defstruct pos-col ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

;;------------------------------------------------------------

(5am:def-suite test-all)

(5am:in-suite test-all)

(5am:def-test build-0 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (v! 0 0 0 0))))

(5am:def-test build-1 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (v! 0 0 0 0)
     (v! 0 0 0 0))))

(5am:def-test build-2 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 1))
       (v! 0 x 1 2))
     (v! 0 0 0 0))))

(5am:def-test build-3 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 1)
           (y 2))
       (v! x y 1 2))
     (v! 0 0 0 0))))

(5am:def-test build-4 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test () 1))
       (test))
     (v! 0 0 0 0))))

(5am:def-test build-5 (:suite test-all)
  (signals varjo-conditions:could-not-find-function
    (compile-vert () :450 nil
      (labels ((test () 1))
        (test))
      (v! 0 (test) 0 0))))

(5am:def-test build-6 (:suite test-all)
  (signals varjo-conditions:symbol-unidentified
    (compile-vert () :450 nil
      (let ((x 1)
            (y 2))
        (v! 0 x 1 2))
      (v! 0 0 y 0))))

(5am:def-test build-7 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test () 1))
       (v! 0 (test) 0 0)))))

(5am:def-test build-8 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 2))
       (labels ((test () x))
         (v! 0 (test) 0 0))))))

(5am:def-test build-9 (:suite test-all)
  (signals varjo-conditions:setq-type-match
    (compile-vert () :450 nil
      ())))

(5am:def-test build-10 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values x 2)))
       (v! 0 (test 1))
       (v! 0 0 0 0)))))

(5am:def-test build-11 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (values (v! 1 2 3 4)
             (v! 1 2)))))

(5am:def-test build-12 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values (v! 0 0 0 0) 2)))
       (test 1)))))

(5am:def-test build-13 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values x 2)))
       (v! 0 (int (test 1)) 0 0)))))


(5am:def-test build-14 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (values (v! 1 2 3 4)
             (v! 1 2))
     (v! 10 20 30 40))))

(5am:def-test build-15 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values (v! 0 0 0 0) 2)))
       (test 1)
       (v! 10 20 30 40)))))

(5am:def-test build-16 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (labels ((test ((x :int)) (values x 2)))
       (v! 0 (int (test 1)) 0 0)
       (v! 10 20 30 40)))))

(5am:def-test build-17 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 1))
       (let ((y 2)
             (z 3))
         (v! x y z)
         (%if (> x 2)
              (setq x y)
              (setq x z))
         (v! x 2 3 4))))))

(5am:def-test build-18 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 0)
           (z 1))
       (v! x z)
       (switch x
         (0 (setq z 1))
         (1 (setq z x))
         (2 z))
       (v! x z 3 4)))))

(5am:def-test build-19 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 0)
           (z 1))
       (v! x z)
       (while (< x 10)
         (setq x z)
         (setq z (+ 1 1)))
       (v! x z 3 4)))))

(5am:def-test build-20 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (let ((x 1)
           (y 2)
           (z 3))
       (v! x y 0 0)))))

(5am:def-test build-21 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (multiple-value-bind (x y) (values 1 2)
       (v! 0 0 0 0)))))

(5am:def-test build-22 (:suite test-all)
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

(5am:def-test build-23 (:suite test-all)
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


(5am:def-test build-24 (:suite test-all)
  ;; same as build-22 but in fragment shader
  (is (ast-stabalizes-p
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

;;------------------------------------------------------------

(5am:def-test build-25 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (let ((fn (labels ((test ((x :int)) x))
                 #'test)))
       (v! 0 0 0 0)))))

(5am:def-test build-26 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (let ((fn (labels ((test ((x :int)) x))
                 #'test)))
       (funcall fn 10)
       (v! 0 0 0 0)))))

(5am:def-test build-27 (:suite test-all)
  (glsl-doesnt-contain-p "FN;"
    (compile-vert () :450 nil
      (let ((fn (labels ((test ((x :int)) x))
                  #'test)))
        fn
        (v! 0 0 0 0)))))

(5am:def-test build-28 (:suite test-all)
  (glsl-doesnt-contain-p "FN;"
    (compile-vert () :450 nil
      (let ((fn (labels ((test ((x :int)) x))
                  #'test)))
        (labels ((foo ((ffn (function (:int) :int)))
                   (funcall ffn 10)))
          (foo fn))
        (v! 0 0 0 0)))))

(5am:def-test build-29 (:suite test-all)
  (signals varjo-conditions:closures-not-supported
    (compile-vert () :450 nil
      (let* ((y 10)
             (fn (labels ((test ((x :int)) (* y x)))
                   #'test)))
        (v! 0 0 0 0)))))

(5am:def-test build-30 (:suite test-all)
  (signals varjo-conditions:cross-scope-mutate
    (compile-vert () :450 nil
      (let* ((y 10)
             (fn (labels ((test ((x :int))
                            (setf y 2)
                            x))
                   #'test)))
        (funcall fn 10)
        (v! 0 0 0 0)))))

(5am:def-test build-31 (:suite test-all)
  (signals varjo-conditions:cross-scope-mutate
    (compile-vert () :450 nil
      (let* ((y 10)
             (fn (labels ((test ((x :int)) x))
                   #'test)))
        (labels ((foo ((ffn (function (:int) :int)))
                   (setf y 2)
                   (funcall ffn 10)))
          (foo fn))
        (v! 0 0 0 0)))))

(5am:def-test build-32 (:suite test-all)
  (signals varjo-conditions:symbol-unidentified
    (compile-vert () :450 nil
      (labels ((foo ((ffn (function (:int) :int)))
                 (funcall ffn y)))
        (let ((y 10))
          (foo (lambda ((a :int)) a))))
      (v! 0 0 0 0))))

(5am:def-test build-33 (:suite test-all)
  (is (ast-stabalizes-p
       (compile-vert () :450 nil
         (let ((fn (labels ((test ((x :int)) x))
                     #'test)))
           (labels ((foo ((ffn (function (:int) :int)))
                      (funcall ffn 10)))
             (foo fn))
           (v! 0 0 0 0))))))

(5am:def-test build-34 (:suite test-all)
  (glsl-contains-n-p 1 "vec4 TEST_EXT.*\\(float HAM\\);"
    (compile-vert () :450 nil
      (test-ext 10s0)
      (test-ext 10s0)
      (v! 0 0 0 0))))

(5am:def-test build-35 (:suite test-all)
  (glsl-contains-n-p 1
      "vec4 TEST_EXT2.*\\(float HAM, out int return1, out int return2\\);"
    (compile-vert () :450 nil
      (test-ext2 10s0)
      (test-ext2 10s0)
      (v! 0 0 0 0))))

(5am:def-test build-36 (:suite test-all)
  (glsl-contains-n-p 1
      "vec4 TEST_EXT3.*\\(float HAM.*\\);"
    (compile-vert () :450 nil
      (test-ext3 10s0)
      (test-ext3 10s0)
      (v! 0 0 0 0))))

(5am:def-test build-37 (:suite test-all)
  (glsl-contains-n-p 1
      "float FOO\\(int X\\);"
    (varjo.tests::compile-vert () :450 nil
      (labels ((foo ((x :int)) (float 10)))
        (v! 0 (foo 0) 0 0)))))

(5am:def-test build-38 (:suite test-all)
  (finishes-p
   (compile-vert () :450 nil
     (test-ext2 10s0)
     (multiple-value-bind (a b c) (test-ext2 10s0)
       (v! (s~ a :xy) b c)))))

(5am:def-test build-39 (:suite test-all)
  (finishes-p
   (compile-vert-frag () :450 nil
     (()
      (v! 0 0 0 0))
     (()
      (v! 0 0 0 0)))))

(5am:def-test build-40 (:suite test-all)
  (finishes-p
   (compile-vert-frag () :450 nil
     (()
      (values (v! 0 0 0 0)
              (v! 1 1)))
     (((tc :vec2))
      (v! tc 0 0)))))

(5am:def-test build-41 (:suite test-all)
  (finishes-p
   (compile-vert-frag () :450 nil
     (((vert pos-rot))
      (values (v! 0 0 0 0)
              (v! 1 1)
              (pos-rot-pos vert)))
     (((tc :vec2) (veec :vec3))
      (v! tc 0 0)))))

(5am:def-test build-42 (:suite test-all)
  (finishes-p
   (compile-vert-frag ((thing pos-rot)) :450 nil
     (()
      (values (v! 0 0 0 0)
              (v! 1 1)
              (pos-rot-pos thing)))
     (((tc :vec2) (veec :vec3))
      (pos-rot-pos thing)
      (v! tc 0 0)))))

(5am:def-test build-43 (:suite test-all)
  (compile-vert ((vert pos-col)) :450 nil
    (values (v! (pos vert) 1.0)
            (col vert))))

;;------------------------------------------------------------

(defvar *some-test-var* 20)

(defun try-guessing-a-type-for-symbol (name)
  (when (boundp name)
    (typecase (symbol-value name)
      (single-float :float)
      (double-float :double)
      ((signed-byte 32) :int)
      ((unsigned-byte 32) :uint)
      (t (error "Cant guess a suitable type for ~s" name)))))

(5am:def-test stemcells-0 (:suite test-all)
  (varjo:with-stemcell-infer-hook #'try-guessing-a-type-for-symbol
    (finishes-p
     (compile-vert () :450 t
       (v! *some-test-var* 0 0 1)))))
