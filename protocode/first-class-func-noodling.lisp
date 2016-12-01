#||

So there may be a subtle difference between the spaces a functions

Before the type was space, but we needed to know which space.

Now the type is the function...isnt it? Nah

Becuse if have a function X that takes a function of type (function (int) int)
we dont know which function of that type it will be when we compile X.

What if we always inline the function though?

So every call to a external function is a 'labels' form? Isnt that already the
case?

Yeah. Ok so only local & external functions can possibly take functions. And
external functions always get turned into local functions at the call site.

We cant have data structures of functions & we dont have closures.

||#


(let ((func #'some-func))
  (funcall func 10) ;; no worries here
  (labels ((foo ((f (function (:int) :int)))
             (funcall foo 10)))
    (foo func) ;; this is an issue if called with multiple
    ;;            different funcs. hmm. In cases where it's
    ;;            like this we can just create new local funcs at call
    ;;            site. It's only if the same call site gets multiple different
    ;;            funcs passed.
    ;;            When making local funcs We need to make sure we read only
    ;;            the vars for the original lexical scope though.
    ))


#||

functions that return functions are arse :D

Screw it, just use flow-ids! Trust past you to have thought about this.


No! See rewrite-spaces.lisp

||#
