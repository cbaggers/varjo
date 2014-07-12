

(defun test (x y z)
  (values (* x 2) (* y 2) (* z 2)))

(defun test (x y z &out _1 _2 _3)
  (setf _1 (* x 2)
        _2 (* y 2)
        _3 (* z 2)))



(multiple-value-bind (a b c) (test 7 8 9)
  (+ a b c))

(let (a b c)
  (test 7 8 9 a b c)
  (+ a b c))

int _1;
int _2;
int _3;
test (7 8 9 _1 _2 _3);
a=_1;
b=_2;
c=_3;

;; problem
(test 7 8 (test 2 3 4))

would have clashing names, needs to be unique for each function call

;; ok so

(multiple-value-bind (a b c) (test 7 8 9)
  (+ a b c))

;; becomes

(progn
  (test 7 8 9)
  (let ((a _1)
        (b _2)
        (c _3))
    (+ a b c)))

;; the fuction call expand to the following (because of the values info 
;;  in the function)

(progn
  (let ((_1 _2 _3))
    (test 7 8 9))
  (let ((a _1)
        (b _2)
        (c _3))
    (+ a b c)))

;; which is roughly

int _1;
int _2;
int _3;
test (7 8 9 _1 _2 _3);
a=_1;
b=_2;
c=_3;
a + b + c;

;; Damn, now this will work but is conceptually ugly.

;; ok so what should happen is the lambda-list and the body should be captured
;; as raw code in the environment which #'test can then inject into it's scope.

(let ((_1 _2 _3))
  (test 7 8 9)
  (let ((a _1)
        (b _2)
        (c _3))
    (+ a b c)))

;; this results in sensible code that doesnt make use of any hacks and so is
;; more future-proof
