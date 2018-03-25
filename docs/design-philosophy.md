# Design Philosophy

In Varjo we have tried to be pragmatic in our choices. We want to be as close as possible to Common Lisp, whilst being realistic about performance and not adding constructs which would deceive our users.

For example, let us assume we wanted provide support for adjustable arrays in Varjo. Well, we could certainly provide the experience & syntax of using adjustable arrays.  However, as GLSL doesn't have an equivalent, we would only be providing an illusion of the feature, and behind the scenes we would still be creating a new array on every `vector-push-extend`. Now imagine the user is trying to debug a performance issue in their shader code. They are forced to look into the implementation, as their tool has lied to them about what it provides.

In a similar vein we provide first class functions, but only in cases where we can track the calls sites as compile time.

With this is mind we hope we can bring as much convenience as possible to the shader writing experience without making it to hard to reason about the result.
