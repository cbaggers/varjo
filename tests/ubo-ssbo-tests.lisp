(in-package :varjo.tests)
(5am:in-suite ubo-ssbo-tests)

;;------------------------------------------------------------
;; Helper data

(v-defstruct some-data ()
  (ints (:int 1000)))

;;------------------------------------------------------------
;; Tests

(5am:def-test ubo-0 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo)) :410 nil
     (v! 1 2 3 4))))

(5am:def-test ubo-1 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo)) :410 nil
     (with-slots (ints) the-data
       (v! (aref ints 1) 2 3 4)))))

(5am:def-test ubo-2 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo :std-140))
       :410 nil
     (with-slots (ints) the-data
       (v! (aref ints 1) 2 3 4)))))

(5am:def-test ubo-3 (:suite ubo-ssbo-tests)
  (signals error
    (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo :std-430))
        :410 nil
      (with-slots (ints) the-data
        (v! (aref ints 1) 2 3 4)))))

(5am:def-test ubo-4 (:suite ubo-ssbo-tests)
  (signals varjo-conditions::assigning-to-readonly
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ubo :std-140))
       :410 nil
     (with-slots (ints) the-data
       (setf (aref ints 1) 10)
       (v! 1 2 3 4)))))

(5am:def-test ssbo-0 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (with-slots (ints) the-data
       (v! (aref ints 1) 2 3 4)))))

(5am:def-test ssbo-1 (:suite ubo-ssbo-tests)
  (finishes-p
    (compile-vert ((vert pos-col) &uniform (the-data some-data :ssbo :std-430))
        :450 nil
      (with-slots (ints) the-data
        (v! (aref ints 1) 2 3 4)))))

(5am:def-test ssbo-2 (:suite ubo-ssbo-tests)
  (finishes-p
   (compile-vert ((vert pos-col) &uniform (the-data some-data :ssbo :std-140))
        :450 nil
      (with-slots (ints) the-data
        (setf (aref ints 1) 10)
        (v! 1 2 3 4)))))

;; this is ok
(defun foo0 ()
  (glsl-code
   (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10))))
       (v! 1 2 3 4)))))

;; this is ok
(defun foo1 ()
  (glsl-code
   (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10))))
       (blah the-data)
       (v! 1 2 3 4)))))

;; this is not ok
(defun foo2 ()
  (glsl-code
   (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10)))
              (ham ((x some-data))
                (blah x)))
       ;; note that we dont even have to call ham for this to happen!
       (blah the-data)
       (v! 1 2 3 4)))))

;; this is not ok
(defun foo3 ()
  (glsl-code
   (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10)))
              (ham ((x some-data))
                (blah x)))
       (ham the-data)
       (v! 1 2 3 4)))))

;; this is not ok
(defun foo4 ()
  (glsl-code
   (compile-vert ((vert pos-col)
                  &uniform (the-data some-data :ssbo :std-140))
       :450 nil
     (labels ((blah ((x some-data))
                (with-slots (ints) x
                  (setf (aref ints 1) 10)))
              (ham ((x some-data))
                (blah x)))
       (ham the-data)
       (blah the-data)
       (v! 1 2 3 4)))))

;;------------------------------------------------------------

#||

Ok, so the issue is that when the call to blah is made inside another function
that function is first compiled in issolation. This means that, at this point,
there is nothing to tell varjo that this will be called with an unrepresentable
value.

This only happens later.

What we need to do is somehow mark the function so that it is not included in
the source unless the function that calls it is included.

'all-type-from-post-proc' in translate.lisp does the actual function stripping
it does it based on #'call-count

hmm gen-shader-string in string-generation.lisp also checks call-count.. I
wonder under what cases that that would be neccessary

We'd kinda like to store the functiosn that called 'us' instead of call-count,
then we could check the who called then recurively to find out if we find
main in there somewhere. If not then dont include the func. However it's the
compile result that tracks the call-count.

What we need to do instead is collect the compiled-results in the 'compiled'
result object that is returned through the stack. Then, when we compile
functions we can push the containing function into those collected
'compiled-results' objects.

This could work

||#
