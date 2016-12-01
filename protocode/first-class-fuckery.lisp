(in-package :varjo)


(let ((f (let ((x 10))
           (labels ((foo () x))
             #'foo))))
  (funcall f)) ;; x is out of scope

;; but if all vars have own name then it's fine, we just keep the glsl-name of x


(defun-g baz ()
  (let ((f (let ((x 10))
             (labels ((foo () x))
               #'foo))))
    (funcall bar f)))

(defun-g bar ((f (function () :int)))
  (funcall f)) ;; from here there is no way to set x

#||

unless bar gains the out vars :D
hard to do that without generating new functions.. which we are kinda
doing.. ah for fucks sake that is cool but dumb :p

So there's only 2 real places that capture is an issue:
- a function being passed into a function where the var was defined.
- a function being returned from a function

Hoooowever. The only functions that can take/returns functiosn are local or
external..and all external functions become local...so it's always fine?

hehe ... heheheh. this is crazy town. So every function that takes a function
also inherits the implicit arguments.


Hold on their, this is fine for passing functions with captured vars into
functions. However what about returning functions with captured vars?

THAT doesnt work, as the var definition is in that scope and the caller would
then have to provide the var..one sec let's draw this:

||#

(defun-g bar ()
  (let ((x 10))
    (labels ((foo ((a :int)) (+ a x)))
      #'foo)))

(defun-g baz ()
  (let ((f (bar)))
    (funcall f 3)))

#||

Wait...we don't allow modifying captured vars anyway so whats the problem

This is the problem:

||#

(defun-g bar ((i :int))
  (let ((x 10))
    (setf x i)
    (labels ((foo ((a :int)) (+ a x)))
      #'foo))) ;; we would be tempted to bake x but we can't know the value
;;                until runtime

(defun-g baz ()
  (let ((f (bar some-val)))
    (funcall f 3)))

#||

Ok so we cannot return functions with captured vars from external functions.

Arg but now Im back in the mental case that all external functions become local.

Yeah but the declaration of x (in the above case) is going to be in a glsl-func:


int foo(int a, out int x) {
  return a + x;
}


void bar(int i) {
  int x = 10;
  x = i;
}

void baz() {
  bar();
  foo(3, ???);
}

So maybe it's ok. We just say that if you return a function with an implicit
arg, then you have to take that arg as an implicit arg yourself. But then
the declaration of that var must be a set instead otherwise it will shadow it.

Furthermore in this case:

(labels ((foo ((x :int))
           (labels ((bar ((a :int))
                      (+ x a)))
             #'bar)))
  (foo))

we would need to make 'a' an outvar

What we really want is this: In case of returning a function that captures a var
add that var to the argument list (if not an arg already) as an :inout and
if not an arg change the first 'let' to a 'setf'

Let's noodle on this:

||#

(glsl-code
 (varjo.tests::compile-vert () :450
   (labels ((baz ()
              (let ((x 10))
                (labels ((foo ((a :int))
                           (+ x a)))
                  (foo 5)
                  (foo 7)
                  (foo 9)
                  1))))
     (baz)
     (v! 0 0 0 0))))

#|| becomes this:

int FOO(int A, int X) {
    return (X + A);
}

int BAZ() {
    int X = 10;
    FOO(5,X);
    FOO(7,X);
    FOO(9,X);
    return 1;
}

void main() {
    BAZ();
    gl_Position = vec4(float(0),float(0),float(0),float(0));
}


But if BAZ were to return #'FOO then we would need to change BAZ & MAIN to:


int BAZ(inout int X) {
    X = 10;
    FOO(5,X);
    FOO(7,X);
    FOO(9,X);
    return 1;
}

void main() {
    int X;
    BAZ(X);
    gl_Position = vec4(float(0),float(0),float(0),float(0));
}

So the question is, is there a way to know that the function a function returns
has captured vars WITHOUT compiling the body of the surrounding function.

Clearly the answer is no. We can't change the order of compilation either.
We need to compile once to see, and then if it returns it then compile again
taking that into account. Unless.. nah we can't mutate the ast as we generate
the glsl text as we go (we need to fix this).

We can use checkpointing of flow-ids, but we have things that mutate the
base-environment (I knew that was gonna bite me in the arse eventually).





Oh shit wait! Capture won't work!

||#

(defun bar ((b :int))
  (let ((x b))
    (lambda ((a :int))
      (+ x a))))

(defun-g foo ()
  (let ((f0 (bar 1))
        (f2 (bar 2)))
    (funcall f0 10)   ;; did you see the problem?
    (funcall f1 10))) ;; both will return '12'

#||

The declaration for 'x' will get moved to foo, but will get mutated by EACH call
to 'bar'. This break the issusion of capture.

That's ok though! we just use a new var for each call to 'bar'. We end up with
something like:

void foo() {
  int x0;
  int x1;
  bar(1, x0);
  bar(1, x1);
  lambda0(10, x0);
  lambda0(10, x1);
}

Phew, it'll be fine. And lambda0 will still dedup! :D compilers are too much
fun.

||#
