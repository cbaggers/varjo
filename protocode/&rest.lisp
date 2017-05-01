#||

&rest for v-def-glsl-template-fun

This is a tricky feature as Varjo will not support linked lists (they make no sense on the gpu).

Here is a v-def-glsl-template-fun right now:
```(v-def-glsl-template-fun v! (x y) "vec3(~a,~a)" (v-float v-vec2) v-vec3)```
That's cool, the glsl-strings of the compiled params are spliced into the string. Simple enough.

But now let's allow `&rest` and also `~{~}`
```(v-def-glsl-template-fun v! (&rest x) "vec3(~{~a~})" (&rest :float) ??)```
ugh, of course. now the return type is unknown, an also `(v!)` is invalid but this isn't expressed here.

||#
