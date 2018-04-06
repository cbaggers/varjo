(in-package :vari.cl)
(in-readtable :fn.reader)

(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-dvec4 v-int) :double :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-dvec3 v-int) :double :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-dvec2 v-int) :double :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-ivec4 v-int) :int :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-ivec3 v-int) :int :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-ivec2 v-int) :int :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-uvec4 v-int) :uint :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-uvec3 v-int) :uint :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-uvec2 v-int) :uint :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-bvec4 v-int) :bool :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-bvec3 v-int) :bool :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-bvec2 v-int) :bool :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-vec4 v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-vec3 v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-vec2 v-int) :float :v-place-index 0)

(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat2 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat3 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat4 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-dmat2 v-int v-int) :double :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-dmat3 v-int v-int) :double :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-dmat4 v-int v-int) :double :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat2x2 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat2x3 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat2x4 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat3x2 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat3x3 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat3x4 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat4x2 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat4x3 v-int v-int) :float :v-place-index 0)
(v-def-glsl-template-fun aref (x i j) "~a[~a][~a]" (v-mat4x4 v-int v-int) :float :v-place-index 0)

(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat2 v-int) :vec2 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat3 v-int) :vec3 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat4 v-int) :vec4 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-dmat2 v-int) :dvec2 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-dmat3 v-int) :dvec3 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-dmat4 v-int) :dvec4 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat2x2 v-int) :vec2 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat2x3 v-int) :vec2 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat2x4 v-int) :vec2 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat3x2 v-int) :vec3 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat3x3 v-int) :vec3 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat3x4 v-int) :vec3 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat4x2 v-int) :vec4 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat4x3 v-int) :vec4 :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-mat4x4 v-int) :vec4 :v-place-index 0)

;;------------------------------------------------------------

(defun aref-common (func arr index env)
  (let* ((type (primary-type arr))
         (elem-type (set-flow-id (v-element-type type) (flow-id!)))
         (type-set (make-type-set elem-type)))
    (values
     (merge-compiled (list arr index)
                     :type-set type-set
                     :current-line (format nil "~a[~a]"
                                           (current-line arr t)
                                           (current-line index))
                     :place-tree (varjo.internals::place-tree-cons func arr))
     env)))

(v-defspecial aref ((arr v-array) (index v-int))
  :return (aref-common this arr index env))

(v-defspecial row-major-aref ((arr v-array) (index v-int))
  :return (aref-common this arr index env))

(v-defspecial svref ((arr v-array) (index v-int))
  :return (aref-common this arr index env))

(v-defspecial aref ((arr (v-block-array "n/a" t *)) (index v-int))
  :return
  (let* ((type (primary-type arr))
         (elem-type (set-flow-id (v-element-type type) (flow-id!)))
         (type-set (make-type-set elem-type)))
    (values
     (merge-compiled (list arr index)
                     :type-set type-set
                     :current-line (format nil "~a[~a].~a"
                                           (block-name type)
                                           (current-line index)
                                           (current-line arr t)))
     env)))

;;------------------------------------------------------------
