(in-package :varjo)

(add-equivalent-name 'cl:length 'rtg-math.vectors:length)

(v-defun rtg-math.vectors:x (a) "~a.x" (v-bvec2) v-bool)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-bvec3) v-bool)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-bvec4) v-bool)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-dvec2) v-double)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-dvec3) v-double)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-dvec4) v-double)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-ivec2) v-int)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-ivec3) v-int)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-ivec4) v-int)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-uvec2) v-uint)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-uvec3) v-uint)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-uvec4) v-uint)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-vec2) v-float)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-vec3) v-float)
(v-defun rtg-math.vectors:x (a) "~a.x" (v-vec4) v-float)

(v-defun rtg-math.vectors:y (a) "~a.y" (v-bvec2) v-bool)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-bvec3) v-bool)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-bvec4) v-bool)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-dvec2) v-double)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-dvec3) v-double)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-dvec4) v-double)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-ivec2) v-int)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-ivec3) v-int)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-ivec4) v-int)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-uvec2) v-uint)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-uvec3) v-uint)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-uvec4) v-uint)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-vec2) v-float)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-vec3) v-float)
(v-defun rtg-math.vectors:y (a) "~a.y" (v-vec4) v-float)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-vec3)  v-float)
(v-defun rtg-math.vectors:z (a) "~a.z" (v-bvec3) v-bool)
(v-defun rtg-math.vectors:z (a) "~a.z" (v-ivec3) v-int)
(v-defun rtg-math.vectors:z (a) "~a.z" (v-uvec3) v-uint)
(v-defun rtg-math.vectors:z (a) "~a.z" (v-dvec3) v-double)
(v-defun rtg-math.vectors:z (a) "~a.z" (v-vec4)  v-float)
(v-defun rtg-math.vectors:z (a) "~a.z" (v-bvec4) v-bool)
(v-defun rtg-math.vectors:z (a) "~a.z" (v-ivec4) v-int)
(v-defun rtg-math.vectors:z (a) "~a.z" (v-uvec4) v-uint)
(v-defun rtg-math.vectors:z (a) "~a.z" (v-dvec4) v-double)

(v-defun rtg-math.vectors:w (a) "~a.w" (v-vec4) v-float)
(v-defun rtg-math.vectors:w (a) "~a.w" (v-bvec4) v-bool)
(v-defun rtg-math.vectors:w (a) "~a.w" (v-ivec4) v-int)
(v-defun rtg-math.vectors:w (a) "~a.w" (v-uvec4) v-uint)
(v-defun rtg-math.vectors:w (a) "~a.w" (v-dvec4) v-double)


;; m3

(v-defun rtg-math.matrix3:make (a b c d e f g h i)
  "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
  (v-float v-float v-float v-float v-float v-float v-float v-float v-float)
  v-mat3)

(v-defun m3:melm (m r c) "~a[~a, ~a]" (v-mat3 v-int v-int)
	 v-float)

(v-defun m3:melm (m r c) "~a[~a, ~a]" (v-dmat3 v-int v-int)
	 v-double)


(v-defun m3:identity () "mat3(1.0)" () v-mat3)
(v-defun m3:0! () "mat3(0.0)" () v-mat3)

(v-defun m3:from-columns (a b c) "mat3(~a, ~a, ~a)" (v-vec3 v-vec3 v-vec3)
	 v-mat3)

;; m4

(v-defun rtg-math.matrix4:make (a b c d e f g h i j k l m n o p)
  "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
  (v-float v-float v-float v-float v-float v-float v-float v-float v-float
           v-float v-float v-float v-float v-float v-float v-float v-float)
  v-mat4)

(v-defun m4:melm (m r c) "~a[~a, ~a]" (v-mat4 v-int v-int)
	 v-float)

(v-defun m4:melm (m r c) "~a[~a, ~a]" (v-dmat4 v-int v-int)
	 v-double)

(v-defun m4:identity () "mat4(1.0)" () v-mat4)
(v-defun m4:0! () "mat4(0.0)" () v-mat4)

(v-defun m4:from-columns (a b c d) "mat4(~a, ~a, ~a, ~a)"
	 (v-vec4 v-vec4 v-vec4 v-vec4) v-mat3)

(v-defun rtg-math.matrix4:to-mat3 (m) "mat3(~a)" (v-mat4) v-mat3)
