(in-package :vari.cl)
(in-readtable :fn.reader)

(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-array v-int) (:element 0) :v-place-index 0)
(v-def-glsl-template-fun aref (x i) "~a[~a]" (v-vector v-int) (:element 0) :v-place-index 0)

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

(v-def-glsl-template-fun row-major-aref (x i) "~a[~a]" (v-array v-int) (:element 0) :v-place-index 0)
(v-def-glsl-template-fun svref (x i) "~a[~a]" (v-array v-int) (:element 0) :v-place-index 0)

;;------------------------------------------------------------
