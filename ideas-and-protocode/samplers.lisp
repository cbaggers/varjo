
(glsl-defun :name 'texture-size
            :in-args '((sampler ((:sampler-1d 
				  :isampler-1d
				  :usampler-1d
				  :sampler-1d-shadow)))
                       (lod :int))
            :output-type :int
            :transform "textureSize(~a, ~a)")

(glsl-defun :name 'texture-size
            :in-args '((sampler ((:sampler-2d
				  :isampler-2d
				  :usampler-2d
				  :sampler-cube
				  :usampler-cube
				  :isampler-cube
				  :sampler-2d-shadow
				  :sampler-cube-shadow
				  :sampler-1d-array
				  :isampler-1d-array
				  :usampler-1d-array
				  :sampler-1d-array-shadow)))
                       (lod :int))
            :output-type :ivec2
            :transform "textureSize(~a, ~a)")

(glsl-defun :name 'texture-size
            :in-args '((sampler ((:sampler-3d
				  :isampler-3d
				  :usampler-3d
				  :sampler-2d-array
				  :usampler-2d-array
				  :isampler-2d-array
				  :sampler-2d-array-shadow)))
                       (lod :int))
            :output-type :ivec3
            :transform "textureSize(~a, ~a)")

(glsl-defun :name 'texture-size
            :in-args '((sampler ((:sampler-buffer
				  :isampler-buffer
				  :usampler-buffer))))
            :output-type :int
            :transform "textureSize(~a)")

(glsl-defun :name 'texture-size
            :in-args '((sampler ((:sampler-2d-rect
				  :isampler-2d-rect
				  :usampler-2d-rect
				  :sampler-2d-rect-shadow
				  :sampler-2d-ms
				  :isampler-2d-ms
				  :usampler-2d-ms
				  :sampler-2d-ms-array
				  :isampler-2d-ms-array
				  :usampler-2d-ms-array))))
            :output-type :ivec2
            :transform "textureSize(~a)")

;;-----------------------

(glsl-defun :name 'texture
            :in-args '((sampler :sampler-1d)
		       (p :float))
            :output-type :vec4
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler :sampler-1d)
		       (p :float)
		       (bias :float))
            :output-type :vec4
            :transform "texture(~a, ~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler :isampler-1d)
		       (p :float))
            :output-type :ivec4
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler :isampler-1d)
		       (p :float)
		       (bias :float))
            :output-type :ivec4
            :transform "texture(~a, ~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler :usampler-1d)
		       (p :float))
            :output-type :uvec4
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler :usampler-1d)
		       (p :float)
		       (bias :float))
            :output-type :uvec4
            :transform "texture(~a, ~a, ~a)")


(glsl-defun :name 'texture
            :in-args '((sampler ((:sampler-2d :sampler-1d-array)))
		       (p :vec2))
            :output-type :vec4
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:sampler-2d :sampler-1d-array)))
		       (p :vec2)
		       (bias :float))
            :output-type :vec4
            :transform "texture(~a, ~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:isampler-2d :isampler-1d-array)))
		       (p :vec2))
            :output-type :ivec4
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:isampler-2d :isampler-1d-array)))
		       (p :vec2)
		       (bias :float))
            :output-type :ivec4
            :transform "texture(~a, ~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:usampler-2d :usampler-1d-array)))
		       (p :vec2))
            :output-type :uvec4
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:usampler-2d :usampler-1d-array)))
		       (p :vec2)
		       (bias :float))
            :output-type :uvec4
            :transform "texture(~a, ~a, ~a)")


(glsl-defun :name 'texture
            :in-args '((sampler ((:sampler-3d :sampler-cube :sampler-2d-array)))
		       (p :vec3))
            :output-type :vec4
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:sampler-3d :sampler-cube :sampler-2d-array)))
		       (p :vec3)
		       (bias :float))
            :output-type :vec4
            :transform "texture(~a, ~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:isampler-3d :isampler-cube :isampler-2d-array)))
		       (p :vec3))
            :output-type :ivec4
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:isampler-3d :isampler-cube :isampler-2d-array)))
		       (p :vec3)
		       (bias :float))
            :output-type :ivec4
            :transform "texture(~a, ~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:usampler-3d :usampler-cube :usampler-2d-array)))
		       (p :vec3))
            :output-type :uvec4
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:usampler-3d :usampler-cube :usampler-2d-array)))
		       (p :vec3)
		       (bias :float))
            :output-type :uvec4
            :transform "texture(~a, ~a, ~a)") 

(glsl-defun :name 'texture
            :in-args '((sampler ((:sampler-1d-shadow :sampler-2d-shadow :samdpler-2d-rect-shader)))
		       (p :vec3))
            :output-type :float
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:sampler-1d-shadow :sampler-2d-shadow)))
		       (p :vec3)
		       (bias :float))
            :output-type :float
            :transform "texture(~a, ~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:sampler-cube-shadow)))
		       (p :vec4))
            :output-type :float
            :transform "texture(~a, ~a)")

(glsl-defun :name 'texture
            :in-args '((sampler ((:sampler-cube-shadow)))
		       (p :vec4)
		       (bias :float))
            :output-type :float
            :transform "texture(~a, ~a, ~a)")
