#||

Alrighty, so we need to have first class function support.

We will not support functions being passed in by uniform. This is a choice
of presentation that cepl wants to make but it isnt technically whjat happens.

The first thing we need is to be able to support the 'function special form.

This will be very simple, just get the function by name and then use that as the
type of the variable. We then define funcall to..do something :p

We need to have the same logic as regular function calls. So we need to use the
logic we already have. This should be fine as the only thing that is different
is that we don't have to look up the function object as it is the type of the
var. OH I got it. The call site goes from:

    (funcall x 1 2)

to
    (labels ((x (a b) (+ a b)))
      (x 1 2)

We also need to support functions that take functions. This also means making
sure the glsl-string DOESNT have this arg.

Lastly we recompile until no funcall ast-nodes remain.

This seems too easy.. what am I missing? The flow stuff I guess. Yeah.

Ok so at the funcall site we need to trace the flow and find the type at the
declartion of the var (as that is the actual function. If this fails we should
emit dummy code to the GLSL, flag for another pass (add this) and emit a funcall
into the AST. The idea is that any place we were unable to trace should become
available next time.. How does it work inside other functions though? with
spaces we just mapped where the value could flow as we were trying to work out
where uniforms could have travelled. Now we want to have functions calling
functiosn which means 'if's (ugh) or generating new functions (potential
ballooning).. neither are great, worse if we don't have a way to communicate
the issue.

If we disallow function arguments we avoid the issue, but also make it a bit
weird as then what is the point of them being first class?

We can sya that it has to be statically resolvable. So it's fine in if's and
such where we can work out the finite set of functions that can be used. But
in cases like loops we lose ability to say what is happening and can report
this to the user.

This could work!

||#

#||

bake is a simple feature. It lets you pick a uniform and fix the value. This
removes the uniform and adds a let to the body. it would also be cool to be
able to make this constant.

||#
