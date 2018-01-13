(in-package :varjo.tests)

#||

Fucking hell, here we are again :|

There are so many ways to get this wrong and at the moment it's trivially easy
to trip this system up.

Let's walk through and see if we can work this out.

So we have explicit-returns and implicit-returns

the implicit-return special form was required as the form it surrounded might
have a different type from the return-set (e.g. void) and this naturally would
result in a failure.

(defun-g foo ((x :bool))
  (if x
      (return 1)
      (return 2))

But this ↑↑ 'if' doesnt not return void. It, never returns. So we should say
'v-returned or maybe '(v-returned :int)

(defun-g foo ((x :bool))
  (if x
      2
      (return 1))

this would techincally be (:or :int (v-returned :int)) but we could have the
same rules as v-discarded where that reduces to :int

I think that we can ditch the implicit-return special form and just say that
'return' is allowed to take a 'v-returned in which case it is just a no-op

Maybe we can ditch return-set in favor of the v-returned type.. nah lets have
the unparametized v-returned & return-set. It should require less jumping
through hoops.

Let's have a look at this code:
||#

(glsl-code
 (varjo.tests::compile-frag () :450 t
   (labels ((gen-line ((x :int))
              (if (= x 1)
                  (if (= x 1)
                      (values 1 1 1)
                      1)
                  (values 2 2 2))))
     (gen-line 2)
     (values))))

#||
We have an issue that the values forms know they need to generate 'return' glsl
code but the '1' expression does not. The problem is that we shouldnt be
emitting these return statements away from the return special-form. It's going
to be uglier but we need to put the values in vars and pass them up.

I think I want a to-values-block, which migrates code up to the right place for
the temp vars. So .. no wait.. it will always propegate to the place which set
the multi-val-base; yes. that's it.

'if' fakes being an expression so it makes for an interesting case.

||#
