(in-package :vari.glsl)

;;------------------------------------------------------------

(v-def-glsl-template-fun mat2 (a b c d) "mat2(~a,~a,~a,~a)" (v-float v-float v-float v-float)
         v-mat2 :pure t)
(v-def-glsl-template-fun mat2 (a b) "mat2(~a,~a)" (v-vec2 v-vec2) v-mat2 :pure t)

(v-def-glsl-template-fun mat3 (a b c d e f g h i) "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
         (v-float v-float v-float v-float v-float
                  v-float v-float v-float v-float) v-mat3 :pure t)

(v-def-glsl-template-fun mat3 (a b c) "mat3(~a,~a,~a)" (v-vec3 v-vec3 v-vec3) v-mat3
                         :pure t)

(v-def-glsl-template-fun mat4 (a b c d e f g h i j k l m n o p)
  "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
  (v-float v-float v-float v-float v-float v-float v-float v-float v-float
           v-float v-float v-float v-float v-float v-float v-float)
  v-mat4 :pure t)

(v-def-glsl-template-fun mat4 (a b c d)
  "mat4(~a,~a,~a,~a)" (v-vec4 v-vec4 v-vec4 v-vec4) v-mat4 :pure t)

;;------------------------------------------------------------
