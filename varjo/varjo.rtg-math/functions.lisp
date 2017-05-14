(in-package :varjo.internals)

(v-def-glsl-template-fun v! (v) "~a" (v-vec2) v-vec2)
(v-def-glsl-template-fun v! (v) "~a" (v-vec2) v-vec3)
(v-def-glsl-template-fun v! (v) "~a" (v-vec4) v-vec4)
(v-def-glsl-template-fun v! (x y) "vec3(~a,~a)" (v-float v-vec2) v-vec3)
(v-def-glsl-template-fun v! (x y) "vec3(~a,~a)" (v-vec2 v-float) v-vec3)

(v-def-glsl-template-fun v! (x y) "vec4(~a,~a)" (v-vec2 v-vec2) v-vec4)
(v-def-glsl-template-fun v! (x y) "vec4(~a,~a)" (v-float v-vec3) v-vec4)
(v-def-glsl-template-fun v! (x y) "vec4(~a,~a)" (v-vec3 v-float) v-vec4)

(v-def-glsl-template-fun v! (x y z) "vec4(~a,~a,~a)" (v-vec2 v-float v-float) v-vec4)
(v-def-glsl-template-fun v! (x y z) "vec4(~a,~a,~a)" (v-float v-vec2 v-float) v-vec4)
(v-def-glsl-template-fun v! (x y z) "vec4(~a,~a,~a)" (v-float v-float v-vec2) v-vec4)

(v-def-glsl-template-fun v! (x y) "vec2(~a,~a)" (v-float v-float) v-vec2)
(v-def-glsl-template-fun v! (x y z) "vec3(~a,~a,~a)" (v-float v-float v-float) v-vec3)
(v-def-glsl-template-fun v! (x y z w) "vec4(~a,~a,~a,~a)" (v-float v-float v-float v-float)
         v-vec4)


(v-def-glsl-template-fun v!bool (x y) "bvec3(~a,~a)" (v-bool v-bvec2) v-bvec3)
(v-def-glsl-template-fun v!bool (x y) "bvec3(~a,~a)" (v-bvec2 v-bool) v-bvec3)

(v-def-glsl-template-fun v!bool (x y) "bvec4(~a,~a)" (v-bvec2 v-bvec2) v-bvec4)
(v-def-glsl-template-fun v!bool (x y) "bvec4(~a,~a)" (v-bool v-bvec3) v-bvec4)
(v-def-glsl-template-fun v!bool (x y) "bvec4(~a,~a)" (v-bvec3 v-bool) v-bvec4)

(v-def-glsl-template-fun v!bool (x y z) "bvec4(~a,~a,~a)" (v-bvec2 v-bool v-bool) v-bvec4)
(v-def-glsl-template-fun v!bool (x y z) "bvec4(~a,~a,~a)" (v-bool v-bvec2 v-bool) v-bvec4)
(v-def-glsl-template-fun v!bool (x y z) "bvec4(~a,~a,~a)" (v-bool v-bool v-bvec2) v-bvec4)

(v-def-glsl-template-fun v!bool (x y) "bvec2(~a,~a)" (v-bool v-bool) v-bvec2)
(v-def-glsl-template-fun v!bool (x y z) "bvec3(~a,~a,~a)" (v-bool v-bool v-bool) v-bvec3)
(v-def-glsl-template-fun v!bool (x y z w) "bvec4(~a,~a,~a,~a)" (v-bool v-bool v-bool v-bool)
         v-bvec4)


(v-def-glsl-template-fun v!int (x) "~a" (v-ivec2) v-ivec2)
(v-def-glsl-template-fun v!int (x) "~a" (v-ivec3) v-ivec3)
(v-def-glsl-template-fun v!int (x) "~a" (v-ivec4) v-ivec4)

(v-def-glsl-template-fun v!int (x) "ivec2(~a)" (v-vec2) v-ivec2)
(v-def-glsl-template-fun v!int (x) "ivec3(~a)" (v-vec3) v-ivec3)
(v-def-glsl-template-fun v!int (x) "ivec4(~a)" (v-vec4) v-ivec4)

(v-def-glsl-template-fun v!int (x y) "ivec3(~a,~a)" (v-int v-ivec2) v-ivec3)
(v-def-glsl-template-fun v!int (x y) "ivec3(~a,~a)" (v-ivec2 v-int) v-ivec3)

(v-def-glsl-template-fun v!int (x y) "ivec4(~a,~a)" (v-ivec2 v-ivec2) v-ivec4)
(v-def-glsl-template-fun v!int (x y) "ivec4(~a,~a)" (v-int v-ivec3) v-ivec4)
(v-def-glsl-template-fun v!int (x y) "ivec4(~a,~a)" (v-ivec3 v-int) v-ivec4)

(v-def-glsl-template-fun v!int (x y z) "ivec4(~a,~a,~a)" (v-ivec2 v-int v-int) v-ivec4)
(v-def-glsl-template-fun v!int (x y z) "ivec4(~a,~a,~a)" (v-int v-ivec2 v-int) v-ivec4)
(v-def-glsl-template-fun v!int (x y z) "ivec4(~a,~a,~a)" (v-int v-int v-ivec2) v-ivec4)

(v-def-glsl-template-fun v!int (x y) "ivec2(~a,~a)" (v-int v-int) v-ivec2)
(v-def-glsl-template-fun v!int (x y z) "ivec3(~a,~a,~a)" (v-int v-int v-int) v-ivec3)
(v-def-glsl-template-fun v!int (x y z w) "ivec4(~a,~a,~a,~a)" (v-int v-int v-int v-int)
         v-ivec4)


(v-def-glsl-template-fun v!uint (x y) "uvec3(~a,~a)" (v-uint v-uvec2) v-uvec3)
(v-def-glsl-template-fun v!uint (x y) "uvec3(~a,~a)" (v-uvec2 v-uint) v-uvec3)

(v-def-glsl-template-fun v!uint (x y) "uvec4(~a,~a)" (v-uvec2 v-uvec2) v-uvec4)
(v-def-glsl-template-fun v!uint (x y) "uvec4(~a,~a)" (v-uint v-uvec3) v-uvec4)
(v-def-glsl-template-fun v!uint (x y) "uvec4(~a,~a)" (v-uvec3 v-uint) v-uvec4)

(v-def-glsl-template-fun v!uint (x y z) "uvec4(~a,~a,~a)" (v-uvec2 v-uint v-uint) v-uvec4)
(v-def-glsl-template-fun v!uint (x y z) "uvec4(~a,~a,~a)" (v-uint v-uvec2 v-uint) v-uvec4)
(v-def-glsl-template-fun v!uint (x y z) "uvec4(~a,~a,~a)" (v-uint v-uint v-uvec2) v-uvec4)

(v-def-glsl-template-fun v!uint (x y) "uvec2(~a,~a)" (v-uint v-uint) v-uvec2)
(v-def-glsl-template-fun v!uint (x y z) "uvec3(~a,~a,~a)" (v-uint v-uint v-uint) v-uvec3)
(v-def-glsl-template-fun v!uint (x y z w) "uvec4(~a,~a,~a,~a)" (v-uint v-uint v-uint v-uint)
         v-uvec4)


(v-def-glsl-template-fun v!double (x y) "dvec3(~a,~a)" (v-double v-dvec2) v-dvec3)
(v-def-glsl-template-fun v!double (x y) "dvec3(~a,~a)" (v-dvec2 v-double) v-dvec3)

(v-def-glsl-template-fun v!double (x y) "dvec4(~a,~a)" (v-dvec2 v-dvec2) v-dvec4)
(v-def-glsl-template-fun v!double (x y) "dvec4(~a,~a)" (v-double v-dvec3) v-dvec4)
(v-def-glsl-template-fun v!double (x y) "dvec4(~a,~a)" (v-dvec3 v-double) v-dvec4)

(v-def-glsl-template-fun v!double (x y z) "dvec4(~a,~a,~a)" (v-dvec2 v-double v-double) v-dvec4)
(v-def-glsl-template-fun v!double (x y z) "dvec4(~a,~a,~a)" (v-double v-dvec2 v-double) v-dvec4)
(v-def-glsl-template-fun v!double (x y z) "dvec4(~a,~a,~a)" (v-double v-double v-dvec2) v-dvec4)

(v-def-glsl-template-fun v!double (x y) "dvec2(~a,~a)" (v-double v-double) v-dvec2)
(v-def-glsl-template-fun v!double (x y z) "dvec3(~a,~a,~a)" (v-double v-double v-double) v-dvec3)
(v-def-glsl-template-fun v!double (x y z w) "dvec4(~a,~a,~a,~a)" (v-double v-double v-double v-double)
         v-dvec4)




(v-def-glsl-template-fun m! (a b c d) "mat2(~a,~a,~a,~a)" (v-float v-float v-float v-float)
         v-mat2)
(v-def-glsl-template-fun m! (a b) "mat2(~a,~a)" (v-vec2 v-vec2) v-mat2)

(v-def-glsl-template-fun m! (a b c d e f g h i) "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
         (v-float v-float v-float v-float v-float
                  v-float v-float v-float v-float) v-mat3)
(v-def-glsl-template-fun m! (a b c) "mat3(~a,~a,~a)" (v-vec3 v-vec3 v-vec3) v-mat3
         )

(v-def-glsl-template-fun m! (a b c d e f g h i j k l m n o p)
  "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
  (v-float v-float v-float v-float v-float v-float v-float v-float v-float
           v-float v-float v-float v-float v-float v-float v-float)
  v-mat4)
(v-def-glsl-template-fun m! (a b c d)
  "mat4(~a,~a,~a,~a)" (v-vec4 v-vec4 v-vec4 v-vec4) v-mat4)
