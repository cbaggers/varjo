(in-package :varjo)

;; (add-equivalent-name 'varjo-lang:v! 'rtg-math.base-vectors:v!)
;; (add-equivalent-name 'varjo-lang:m! 'rtg-math.base-matrices:m!)
;; (add-equivalent-name 'varjo-lang:swizzle 'rtg-math.vectors:swizzle)
;; (add-equivalent-name 'varjo-lang:s~ 'rtg-math.vectors:s~)
;; (add-equivalent-name 'varjo-lang:dot 'rtg-math.vectors:dot)
;; (add-equivalent-name 'varjo-lang:normalize 'rtg-math.vectors:normalize)
;; (add-equivalent-name 'varjo-lang:normalize 'rtg-math.vectors:cross)

(add-equivalent-name 'cl:length 'rtg-math.vectors:length)

;;

(v-defun rtg-math.vectors:x (a) "~a.x" (v-vector) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:y (a) "~a.y" (v-vector) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-vec3)  (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-bvec3) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-ivec3) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-uvec3) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-dvec3) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-vec4)  (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-bvec4) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-ivec4) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-uvec4) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:z (a) "~a.z" (v-dvec4) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:w (a) "~a.w" (v-vec4) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:w (a) "~a.w" (v-bvec4) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:w (a) "~a.w" (v-ivec4) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:w (a) "~a.w" (v-uvec4) (:element 0)
	       :glsl-spec-matching t)

(v-defun rtg-math.vectors:w (a) "~a.w" (v-dvec4) (:element 0)
	       :glsl-spec-matching t)
