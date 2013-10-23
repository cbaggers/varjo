;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

;;------------------------------------------------------------
;; Texture Lookup Functions
;;-------------------------
;; :in-args '((vec ((:vec2 :vec3 :vec4))))

;;---texture size---
(glsl-defun 
 :name 'texture-size 
 :in-args '((sampler ((:sampler-1d :isampler-1d :usampler-1d
                                   :sampler-1d-shadow))) (lod :int))
 :output-type :int 
 :transform "textureSize(~a, ~a)"
 :context-restriction '((:330)))

(glsl-defun 
 :name 'texture-size 
 :in-args '((sampler ((:sampler-2d :isampler-2d :usampler-2d :sampler-cube
                                   :isampler-cube :usampler-cube :sampler-2d-shadow
                                   :sampler-cube-shadow :sampler-2d-rect
                                   :isampler-2d-rect :usampler-2d-rect
                                   :sampler-2d-rect-shadow :isampler-2d-rect-shadow
                                   :usampler-2d-rect-shadow :sampler-1d-array
                                   :isampler-1d-array :usampler-1d-array
                                   :sampler-1d-array-shadow))) (lod :int))
 :output-type :ivec2 
 :transform "textureSize(~a, ~a)"
 :context-restriction '((:330)))

(glsl-defun 
 :name 'texture-size 
 :in-args '((sampler ((:sampler-3d :isampler-3d :usampler-3d :sampler-cube-array
                                   :sampler-cube-array-shadow :sampler-2d-array
                                   :isampler-2d-array :usampler-2d-array
                                   :sampler-2d-array-shadow ))) (lod :int))
 :output-type :ivec3 
 :transform "textureSize(~a, ~a)"
 :context-restriction '((:330)))

(glsl-defun 
 :name 'texture-size 
 :in-args '((sampler ((:sampler-buffer :isampler-buffer :usampler-buffer))))
 :output-type :int 
 :transform "textureSize(~a, ~a)"
 :context-restriction '((:330)))

(glsl-defun 
 :name 'texture-size 
 :in-args '((sampler ((:sampler-2d-ms :isampler-2d-ms :usampler-2d-ms))))
 :output-type :ivec2 
 :transform "textureSize(~a, ~a)"
 :context-restriction '((:330)))

(glsl-defun 
 :name 'texture-size 
 :in-args '((sampler ((:sampler-2d-ms-array :isampler-2d-ms-array :usampler-2d-ms-array))))
 :output-type :ivec3 
 :transform "textureSize(~a, ~a)"
 :context-restriction '((:330)))

;;-----texture-----
;; [TODO] This whole section needs reworking... it is wrong

(glsl-multi-defun 
 :name 'texture 
 :specs '((:in ((sampler :isampler-1d) (P :float) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :vec2) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec2) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :vec3) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec3) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-cube) (P :vec3) (bias :float)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :float) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :vec2) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-1d-array-shadow) (P :vec3) (bias :float)) :out :float)
          (:in ((sampler :isampler-2d-array-shadow) (P :vec3) (bias :float)) :out :float)
          (:in ((sampler :sampler-2d-array-shadow) (P :vec3) (bias :float)) :out :float)
          (:in ((sampler :sampler-1d-shadow) (P :vec3) (bias :float)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec2) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :vec3) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d-shadow) (P :vec3) (bias :float)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec3) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-cube) (P :vec3) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-cube-shadow) (P :vec4) (bias :float)) :out :float)          
          (:in ((sampler :usampler-1d) (P :float) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :vec2) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec2) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :vec3) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec3) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-cube) (P :vec3) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d-array-shadow) (P :vec3) (bias :float)) :out :float)
          (:in ((sampler :sampler-cube-array-shadow) (P :vec4) (compare :float)) :out :float)
          (:in ((sampler :isampler-cube-array-shadow) (P :vec4) (compare :float)) :out :float)
          (:in ((sampler :usampler-cube-array-shadow) (P :vec4) (compare :float)) :out :float))
 :transform "texture(~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture 
 :specs '((:in ((sampler :isampler-1d) (P :float)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :vec2)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :vec3)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec2)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec3)) :out :ivec4)
          (:in ((sampler :isampler-cube) (P :vec3)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :float)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :vec2)) :out :vec4)
          (:in ((sampler :sampler-1d-array-shadow) (P :vec3)) :out :float)
          (:in ((sampler :sampler-1d-shadow) (P :vec3)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :vec3)) :out :vec4)
          (:in ((sampler :sampler-2d-array-shadow) (P :vec4)) :out :float)
          (:in ((sampler :isampler-2d-array-shadow) (P :vec4)) :out :float)
          (:in ((sampler :usampler-2d-array-shadow) (P :vec4)) :out :float)
          (:in ((sampler :sampler-2d-rect) (P :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect-shadow) (P :vec3)) :out :float)
          (:in ((sampler :sampler-2d-shadow) (P :vec3)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec3)) :out :vec4)
          (:in ((sampler :sampler-cube) (P :vec3)) :out :vec4)
          (:in ((sampler :sampler-cube-shadow) (P :vec4)) :out :float)
          (:in ((sampler :usampler-1d) (P :float)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :vec2)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :vec3)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec2)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec3)) :out :uvec4)
          (:in ((sampler :usampler-cube) (P :vec3)) :out :uvec4))
 :transform "texture(~a, ~a)"
 :context-restriction '((:330)))
(glsl-multi-defun
 :name 'texture-proj
 :specs '((:in ((sampler :isampler-1d) (P :vec2)) :out :ivec4)
          (:in ((sampler :isampler-1d) (P :vec4)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec3)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec4)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec3)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec4)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec4)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect-shadow) (P :vec4)) :out :float) 
          (:in ((sampler :sampler-1d) (P :vec2)) :out :vec4)
          (:in ((sampler :sampler-1d) (P :vec4)) :out :vec4)
          (:in ((sampler :sampler-1d-shadow) (P :vec4)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec3)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :vec4)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec3)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec4)) :out :vec4)
          (:in ((sampler :sampler-2d-rect-shadow) (P :vec4)) :out :float)
          (:in ((sampler :sampler-2d-shadow) (P :vec4)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec4)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :vec2)) :out :uvec4)
          (:in ((sampler :usampler-1d) (P :vec4)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec3)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec4)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec3)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec4)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec4)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect-shadow) (P :vec4)) :out :float))
 :transform "textureProj(~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun
 :name 'texture-proj
 :specs '((:in ((sampler :isampler-1d) (P :vec2) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-1d) (P :vec4) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec3) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec4) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec4) (bias :float)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :vec2) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-1d) (P :vec4) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-1d-shadow) (P :vec4) (bias :float)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec3) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :vec4) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec3) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec4) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d-shadow) (P :vec4) (bias :float)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec4) (bias :float)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :vec2) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-1d) (P :vec4) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec3) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec4) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec3) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec4) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec4) (bias :float)) :out :uvec4))
 :transform "textureProj(~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-lod 
 :specs '((:in ((sampler :isampler-1d) (P :float) (lod :float)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :vec2) (lod :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec2) (lod :float)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :vec3) (lod :float)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec3) (lod :float)) :out :ivec4)
          (:in ((sampler :isampler-cube) (P :vec3) (lod :float)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :float) (lod :float)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :vec2) (lod :float)) :out :vec4)
          (:in ((sampler :sampler-1d-array-shadow) (P :vec3) (lod :float)) :out :float)
          (:in ((sampler :sampler-1d-shadow) (P :vec3) (lod :float)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec2) (lod :float)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :vec3) (lod :float)) :out :ivec4)
          (:in ((sampler :sampler-2d-shadow) (P :vec3) (lod :float)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec3) (lod :float)) :out :vec4)
          (:in ((sampler :sampler-cube) (P :vec3) (lod :float)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :float) (lod :float)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :vec2) (lod :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec2) (lod :float)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :vec3) (lod :float)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec3) (lod :float)) :out :uvec4)
          (:in ((sampler :usampler-cube) (P :vec3) (lod :float)) :out :uvec4))
 :transform "textureLod(~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-offset 
 :specs '((:in ((sampler :isampler-1d) (P :float) (offset :int) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :vec2) (offset :int) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec2) (offset :ivec2) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :vec3) (offset :ivec2) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec3) (offset :ivec3) (bias :float)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :float) (offset :int) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :vec2) (offset :int) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-1d-array-shadow) (P :vec3) (offset :int) (bias :float)) :out :float)
          (:in ((sampler :sampler-1d-shadow) (P :vec3) (offset :int) (bias :float)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec2) (offset :ivec2) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :vec3) (offset :ivec2) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d-shadow) (P :vec3) (offset :ivec2) (bias :float)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec3) (offset :ivec3) (bias :float)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :float) (offset :int) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :vec2) (offset :int) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec2) (offset :ivec2) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :vec3) (offset :ivec2) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec3) (offset :ivec3) (bias :float)) :out :uvec4))
 :transform "textureOffset(~a, ~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-offset 
 :specs '((:in ((sampler :isampler-1d) (P :float) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :vec2) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec2) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :vec3) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec2) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec3) (offset :ivec3)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :float) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :vec2) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-array-shadow) (P :vec3) (offset :int)) :out :float)
          (:in ((sampler :sampler-1d-shadow) (P :vec3) (offset :int)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec2) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :vec3) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec2) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect-shadow) (P :vec3) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-2d-shadow) (P :vec3) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec3) (offset :ivec3)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :float) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :vec2) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec2) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :vec3) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec2) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec3) (offset :ivec3)) :out :uvec4))
 :transform "textureOffset(~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texel-fetch 
 :specs '((:in ((sampler :isampler-1d) (P :int) (lod :int)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :ivec2) (lod :int)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :ivec2) (lod :int)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :ivec3) (lod :int)) :out :ivec4)
          (:in ((sampler :isampler-2d-ms) (P :ivec2) (sample :int)) :out :ivec4)
          (:in ((sampler :isampler-2d-ms-array) (P :ivec3) (sample :int)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :ivec3) (lod :int)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :int) (lod :int)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :ivec2) (lod :int)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :ivec2) (lod :int)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :ivec3) (lod :int)) :out :vec4)
          (:in ((sampler :sampler-2d-ms) (P :ivec2) (sample :int)) :out :vec4)
          (:in ((sampler :sampler-2d-ms-array) (P :ivec3) (sample :int)) :out :vec4)
          (:in ((sampler :sampler-3d) (P :ivec3) (lod :int)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :int) (lod :int)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :ivec2) (lod :int)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :ivec2) (lod :int)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :ivec3) (lod :int)) :out :uvec4)
          (:in ((sampler :usampler-2d-ms) (P :ivec2) (sample :int)) :out :uvec4)
          (:in ((sampler :usampler-2d-ms-array) (P :ivec3) (sample :int)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :ivec3) (lod :int)) :out :uvec4))
 :transform "texelFetch(~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texel-fetch 
 :specs '((:in ((sampler :isampler-2d-rect) (P :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-buffer) (P :int)) :out :ivec4)
          (:in ((sampler :sampler-2d-rect) (P :ivec2)) :out :vec4)
          (:in ((sampler :sampler-buffer) (P :int)) :out :vec4)
          (:in ((sampler :usampler-2d-rect) (P :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-buffer) (P :int)) :out :uvec4))
 :transform "texelFetch(~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texel-fetch-offset 
 :specs '((:in ((sampler :isampler-1d) (P :int) (lod :int) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :ivec2) (lod :int) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :ivec2) (lod :int) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :ivec3) (lod :int) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :ivec3) (lod :int) (offset :ivec3)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :int) (lod :int) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :ivec2) (lod :int) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :ivec2) (lod :int) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :ivec3) (lod :int) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :ivec2) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-3d) (P :ivec3) (lod :int) (offset :ivec3)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :int) (lod :int) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :ivec2) (lod :int) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :ivec2) (lod :int) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :ivec3) (lod :int) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :ivec3) (lod :int) (offset :ivec3)) :out :uvec4))
 :transform "texelFetchOffset(~a, ~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texel-fetch-offset 
 :specs '((:in ((sampler :isampler-2d-rect) (P :ivec2) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :usampler-2d-rect) (P :ivec2) (offset :ivec2)) :out :uvec4))
 :transform "texelFetchOffset(~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-proj-offset 
 :specs '((:in ((sampler :isampler-1d) (P :vec2) (offset :int) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-1d) (P :vec4) (offset :int) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec3) (offset :ivec2) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec4) (offset :ivec2) (bias :float)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec4) (offset :ivec3) (bias :float)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :vec2) (offset :int) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-1d) (P :vec4) (offset :int) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-1d-shadow) (P :vec4) (offset :int) (bias :float)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec3) (offset :ivec2) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :vec4) (offset :ivec2) (bias :float)) :out :vec4)
          (:in ((sampler :sampler-2d-shadow) (P :vec4) (offset :ivec2) (bias :float)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec4) (offset :ivec3) (bias :float)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :vec2) (offset :int) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-1d) (P :vec4) (offset :int) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec3) (offset :ivec2) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec4) (offset :ivec2) (bias :float)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec4) (offset :ivec3) (bias :float)) :out :uvec4))
 :transform "textureProjOffset(~a, ~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-proj-offset 
 :specs '((:in ((sampler :isampler-1d) (P :vec2) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-1d) (P :vec4) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec3) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec4) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec3) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec4) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec4) (offset :ivec3)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :vec2) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d) (P :vec4) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-shadow) (P :vec4) (offset :int)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec3) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :vec4) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec3) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec4) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect-shadow) (P :vec4) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-2d-shadow) (P :vec4) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec4) (offset :ivec3)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :vec2) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-1d) (P :vec4) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec3) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec4) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec3) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec4) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec4) (offset :ivec3)) :out :uvec4))
 :transform "textureProjOffset(~a, ~a, ~a)"
 :context-restriction '((:330)))


(glsl-multi-defun 
 :name 'texture-lod-offset
 :specs '((:in ((sampler :isampler-1d) (P :float) (lod :float) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :vec2) (lod :float) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec2) (lod :float) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :vec3) (lod :float) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec3) (lod :float) (offset :ivec3)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :float) (lod :float) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :vec2) (lod :float) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-array-shadow) (P :vec3) (lod :float) (offset :int)) :out :float)
          (:in ((sampler :sampler-1d-shadow) (P :vec3) (lod :float) (offset :int)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec2) (lod :float) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :vec3) (lod :float) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-shadow) (P :vec3) (lod :float) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec3) (lod :float) (offset :ivec3)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :float) (lod :float) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :vec2) (lod :float) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec2) (lod :float) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :vec3) (lod :float) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec3) (lod :float) (offset :ivec3)) :out :uvec4))
 :transform "textureLodOffset(~a, ~a, ~a, ~a)"
 :context-restriction '((:330)))


(glsl-multi-defun 
 :name 'texture-proj-lod 
 :specs '((:in ((sampler :isampler-1d) (P :vec2) (lod :float)) :out :ivec4)
          (:in ((sampler :isampler-1d) (P :vec4) (lod :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec3) (lod :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec4) (lod :float)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec4) (lod :float)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :vec4) (lod :float)) :out :vec4)
          (:in ((sampler :sampler-1d-shadow) (P :vec4) (lod :float)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec3) (lod :float)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :vec4) (lod :float)) :out :vec4)
          (:in ((sampler :sampler-2d-shadow) (P :vec4) (lod :float)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec4) (lod :float)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :vec2) (lod :float)) :out :uvec4)
          (:in ((sampler :usampler-1d) (P :vec4) (lod :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec3) (lod :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec4) (lod :float)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec4) (lod :float)) :out :uvec4))
 :transform "textureProjLod(~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-proj-lod-offset 
 :specs '((:in ((sampler :isampler-1d) (P :vec2) (lod :float) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-1d) (P :vec4) (lod :float) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec3) (lod :float) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec4) (lod :float) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec4) (lod :float) (offset :ivec3)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :vec2) (lod :float) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d) (P :vec4) (lod :float) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-shadow) (P :vec4) (lod :float) (offset :int)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec3) (lod :float) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :vec4) (lod :float) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-shadow) (P :vec4) (lod :float) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec4) (lod :float) (offset :ivec3)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :vec2) (lod :float) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-1d) (P :vec4) (lod :float) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec3) (lod :float) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec4) (lod :float) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec4) (lod :float) (offset :ivec3)) :out :uvec4))
 :transform "textureProjLodOffset(~a, ~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-grad 
 :specs '((:in ((sampler :isampler-1d) (P :float) (dPdx :float) (dPdy :float)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :vec2) (dPdx :float) (dPdy :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec2) (dPdx :vec2) (dPdy :vec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec2) (dPdx :vec2) (dPdy :vec2)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec3) (dPdx :vec3) (dPdy :vec3)) :out :ivec4)
          (:in ((sampler :isampler-cube) (P :vec3) (dPdx :vec3) (dPdy :vec3)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :float) (dPdx :float) (dPdy :float)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :vec2) (dPdx :float) (dPdy :float)) :out :vec4)
          (:in ((sampler :sampler-1d-array-shadow) (P :vec3) (dPdx :float) (dPdy :float)) :out :float)
          (:in ((sampler :sampler-1d-shadow) (P :vec3) (dPdx :float) (dPdy :float)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec2) (dPdx :vec2) (dPdy :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d-array-shadow) (P :vec4) (dPdx :vec2) (dPdy :vec2)) :out :float)
          (:in ((sampler :sampler-2d-rect) (P :vec2) (dPdx :vec2) (dPdy :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect-shadow) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :float)
          (:in ((sampler :sampler-2d-shadow) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec3) (dPdx :vec3) (dPdy :vec3)) :out :vec4)
          (:in ((sampler :sampler-cube) (P :vec3) (dPdx :vec3) (dPdy :vec3)) :out :vec4)
          (:in ((sampler :sampler-cube-shadow) (P :vec4) (dPdx :vec3) (dPdy :vec3)) :out :float)
          (:in ((sampler :usampler-1d) (P :float) (dPdx :float) (dPdy :float)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :vec2) (dPdx :float) (dPdy :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec2) (dPdx :vec2) (dPdy :vec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec2) (dPdx :vec2) (dPdy :vec2)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec3) (dPdx :vec3) (dPdy :vec3)) :out :uvec4)
          (:in ((sampler :usampler-cube) (P :vec3) (dPdx :vec3) (dPdy :vec3)) :out :uvec4))
 :transform "textureGrad(~a, ~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-grad-offset 
 :specs '((:in ((sampler :isampler-1d) (P :float) (dPdx :float) (dPdy :float) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-1d-array) (P :vec2) (dPdx :float) (dPdy :float) (offset :int)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec2) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-array) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec2) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec3) (dPdx :vec3) (dPdy :vec3) (offset :ivec3)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :float) (dPdx :float) (dPdy :float) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-array) (P :vec2) (dPdx :float) (dPdy :float) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-array-shadow) (P :vec3) (dPdx :float) (dPdy :float) (offset :int)) :out :float)
          (:in ((sampler :sampler-1d-shadow) (P :vec3) (dPdx :float) (dPdy :float) (offset :int)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec2) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-array) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-array-shadow) (P :vec4) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-2d-rect) (P :vec2) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect-shadow) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-2d-shadow) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec3) (dPdx :vec3) (dPdy :vec3) (offset :ivec3)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :float) (dPdx :float) (dPdy :float) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-1d-array) (P :vec2) (dPdx :float) (dPdy :float) (offset :int)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec2) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-array) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec2) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec3) (dPdx :vec3) (dPdy :vec3) (offset :ivec3)) :out :uvec4))
 :transform "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-proj-grad 
 :specs '((:in ((sampler :isampler-1d) (P :vec2) (dPdx :float) (dPdy :float)) :out :ivec4)
          (:in ((sampler :isampler-1d) (P :vec4) (dPdx :float) (dPdy :float)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :ivec4)
          (:in ((sampler :isampler-2d) (P :vec4) (dPdx :vec2) (dPdy :vec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :ivec4)
          (:in ((sampler :isampler-2d-rect) (P :vec4) (dPdx :vec2) (dPdy :vec2)) :out :ivec4)
          (:in ((sampler :isampler-3d) (P :vec4) (dPdx :vec3) (dPdy :vec3)) :out :ivec4)
          (:in ((sampler :sampler-1d) (P :vec2) (dPdx :float) (dPdy :float)) :out :vec4)
          (:in ((sampler :sampler-1d) (P :vec4) (dPdx :float) (dPdy :float)) :out :vec4)
          (:in ((sampler :sampler-1d-shadow) (P :vec4) (dPdx :float) (dPdy :float)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :vec4) (dPdx :vec2) (dPdy :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec4) (dPdx :vec2) (dPdy :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect-shadow) (P :vec4) (dPdx :vec2) (dPdy :vec2)) :out :float)
          (:in ((sampler :sampler-2d-shadow) (P :vec4) (dPdx :vec2) (dPdy :vec2)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec4) (dPdx :vec3) (dPdy :vec3)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :vec2) (dPdx :float) (dPdy :float)) :out :uvec4)
          (:in ((sampler :usampler-1d) (P :vec4) (dPdx :float) (dPdy :float)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :uvec4)
          (:in ((sampler :usampler-2d) (P :vec4) (dPdx :vec2) (dPdy :vec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec3) (dPdx :vec2) (dPdy :vec2)) :out :uvec4)
          (:in ((sampler :usampler-2d-rect) (P :vec4) (dPdx :vec2) (dPdy :vec2)) :out :uvec4)
          (:in ((sampler :usampler-3d) (P :vec4) (dPdx :vec3) (dPdy :vec3)) :out :uvec4))
 :transform "textureProjGrad(~a, ~a, ~a, ~a)"
 :context-restriction '((:330)))

(glsl-multi-defun 
 :name 'texture-proj-grad-offset 
 :specs '((:in ((sampler :isampler-1d) (P :vec2) (dPdx :float) (dPdy :float) (offset :int)) :out :uvec4)
          (:in ((sampler :isampler-1d) (P :vec4) (dPdx :float) (dPdy :float) (offset :int)) :out :uvec4)
          (:in ((sampler :isampler-2d) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :vec2)) :out :uvec4)
          (:in ((sampler :isampler-2d) (P :vec4) (dPdx :vec2) (dPdy :vec2) (offset :vec2)) :out :uvec4)
          (:in ((sampler :isampler-2d-rect) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :isampler-2d-rect) (P :vec4) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :uvec4)
          (:in ((sampler :isampler-3d) (P :vec4) (dPdx :vec3) (dPdy :vec3) (offset :vec3)) :out :uvec4)
          (:in ((sampler :sampler-1d) (P :vec2) (dPdx :float) (dPdy :float) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d) (P :vec4) (dPdx :float) (dPdy :float) (offset :int)) :out :vec4)
          (:in ((sampler :sampler-1d-shadow) (P :vec4) (dPdx :float) (dPdy :float) (offset :int)) :out :float)
          (:in ((sampler :sampler-2d) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d) (P :vec4) (dPdx :vec2) (dPdy :vec2) (offset :vec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect) (P :vec4) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :vec4)
          (:in ((sampler :sampler-2d-rect-shadow) (P :vec4) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :float)
          (:in ((sampler :sampler-2d-shadow) (P :vec4) (dPdx :vec2) (dPdy :vec2) (offset :vec2)) :out :float)
          (:in ((sampler :sampler-3d) (P :vec4) (dPdx :vec3) (dPdy :vec3) (offset :vec3)) :out :vec4)
          (:in ((sampler :usampler-1d) (P :vec2) (dPdx :float) (dPdy :float) (offset :int)) :out :ivec4)
          (:in ((sampler :usampler-1d) (P :vec4) (dPdx :float) (dPdy :float) (offset :int)) :out :ivec4)
          (:in ((sampler :usampler-2d) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :vec2)) :out :ivec4)
          (:in ((sampler :usampler-2d) (P :vec4) (dPdx :vec2) (dPdy :vec2) (offset :vec2)) :out :ivec4)
          (:in ((sampler :usampler-2d-rect) (P :vec3) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :usampler-2d-rect) (P :vec4) (dPdx :vec2) (dPdy :vec2) (offset :ivec2)) :out :ivec4)
          (:in ((sampler :usampler-3d) (P :vec4) (dPdx :vec3) (dPdy :vec3) (offset :vec3)) :out :ivec4))
 :transform "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
 :context-restriction '((:330)))

;; I think these are all deprecated, YAY!

;; (glsl-multi-defun 
;;  :name 'texture-1D 
;;  '(:in ((sampler :sampler-1d) (coord :float) (bias :float)) :out :int )
;;  :transform "texture1D(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-1D 
;;  '(:in ((sampler :sampler-1d) (coord :float)) :out :int )
;;  :transform "texture1D(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-1d-proj 
;;  '(:in ((sampler :sampler-1d) (coord :vec2) (bias :float)) :out :int )
;;  :transform "texture1DProj(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-1d-proj 
;;  '(:in ((sampler :sampler-1d) (coord :vec2)) :out :int )
;;  :transform "texture1DProj(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-1d-proj 
;;  '(:in ((sampler :sampler-1d) (coord :vec4) (bias :float)) :out :int )
;;  :transform "texture1DProj(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-1d-proj 
;;  '(:in ((sampler :sampler-1d) (coord :vec4)) :out :int )
;;  :transform "texture1DProj(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-1d-lod 
;;  '(:in ((sampler :sampler-1d) (coord :float) (lod :float)) :out :int )
;;  :transform "texture1DLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-1d-proj-lod 
;;  '(:in ((sampler :sampler-1d) (coord :vec2) (lod :float)) :out :int )
;;  :transform "texture1DProjLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-1d-proj-lod 
;;  '(:in ((sampler :sampler-1d) (coord :vec4) (lod :float)) :out :int )
;;  :transform "texture1DProjLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-2D 
;;  '(:in ((sampler :sampler-2d) (coord :vec2) (bias :float)) :out :int )
;;  :transform "texture2D(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-2D 
;;  '(:in ((sampler :sampler-2d) (coord :vec2)) :out :int )
;;  :transform "texture2D(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-2d-proj 
;;  '(:in ((sampler :sampler-2d) (coord :vec3) (bias :float)) :out :int )
;;  :transform "texture2DProj(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-2d-proj 
;;  '(:in ((sampler :sampler-2d) (coord :vec3)) :out :int )
;;  :transform "texture2DProj(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-2d-proj 
;;  '(:in ((sampler :sampler-2d) (coord :vec4) (bias :float)) :out :int )
;;  :transform "texture2DProj(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-2d-proj 
;;  '(:in ((sampler :sampler-2d) (coord :vec4)) :out :int )
;;  :transform "texture2DProj(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-2d-lod 
;;  '(:in ((sampler :sampler-2d) (coord :vec2) (lod :float)) :out :int )
;;  :transform "texture2DLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-2d-proj-lod 
;;  '(:in ((sampler :sampler-2d) (coord :vec3) (lod :float)) :out :int )
;;  :transform "texture2DProjLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-2d-proj-lod 
;;  '(:in ((sampler :sampler-2d) (coord :vec4) (lod :float)) :out :int )
;;  :transform "texture2DProjLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-3D 
;;  '(:in ((sampler :sampler-3d) (coord :vec3) (bias :float)) :out :int )
;;  :transform "texture3D(~a, ~a, ~a)"
;;  :context-restriction '((:330)))p

;; (glsl-multi-defun 
;;  :name 'texture-3D 
;;  '(:in ((sampler :sampler-3d) (coord :vec3)) :out :int )
;;  :transform "texture3D(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-3d-proj 
;;  '(:in ((sampler :sampler-3d) (coord :vec4) (bias :float)) :out :int )
;;  :transform "texture3DProj(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-3d-proj 
;;  '(:in ((sampler :sampler-3d) (coord :vec4)) :out :int )
;;  :transform "texture3DProj(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-3d-lod 
;;  '(:in ((sampler :sampler-3d) (coord :vec3) (lod :float)) :out :int )
;;  :transform "texture3DLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-3d-proj-lod 
;;  '(:in ((sampler :sampler-3d) (coord :vec4) (lod :float)) :out :int )
;;  :transform "texture3DProjLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-cube 
;;  '(:in ((sampler :sampler-cube) (coord :vec3) (bias :float)) :out :int )
;;  :transform "textureCube(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-cube 
;;  '(:in ((sampler :sampler-cube) (coord :vec3)) :out :int )
;;  :transform "textureCube(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'texture-cube-lod 
;;  '(:in ((sampler :sampler-cube) (coord :vec3) (lod :float)) :out :int )
;;  :transform "textureCubeLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-1D 
;;  '(:in ((sampler :sampler-1d-shadow) (coord :vec3) (bias :float)) :out :int )
;;  :transform "shadow1D(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-1D 
;;  '(:in ((sampler :sampler-1d-shadow) (coord :vec3)) :out :int )
;;  :transform "shadow1D(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-2D 
;;  '(:in ((sampler :sampler-2d-shadow) (coord :vec3) (bias :float)) :out :int )
;;  :transform "shadow2D(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-2D 
;;  '(:in ((sampler :sampler-2d-shadow) (coord :vec3)) :out :int )
;;  :transform "shadow2D(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-1d-proj 
;;  '(:in ((sampler :sampler-1d-shadow) (coord :vec4) (bias :float)) :out :int )
;;  :transform "shadow1DProj(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-1d-proj 
;;  '(:in ((sampler :sampler-1d-shadow) (coord :vec4)) :out :int )
;;  :transform "shadow1DProj(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-2d-proj 
;;  '(:in ((sampler :sampler-2d-shadow) (coord :vec4) (bias :float)) :out :int )
;;  :transform "shadow2DProj(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun p
;;  :name 'shadow-2d-proj 
;;  '(:in ((sampler :sampler-2d-shadow) (coord :vec4)) :out :int )
;;  :transform "shadow2DProj(~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-1d-lod 
;;  '(:in ((sampler :sampler-1d-shadow) (coord :vec3) (lod :float)) :out :int )
;;  :transform "shadow1DLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-2d-lod 
;;  '(:in ((sampler :sampler-2d-shadow) (coord :vec3) (lod :float)) :out :int )
;;  :transform "shadow2DLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-1d-proj-lod 
;;  '(:in ((sampler :sampler-1d-shadow) (coord :vec4) (lod :float)) :out :int )
;;  :transform "shadow1DProjLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))

;; (glsl-multi-defun 
;;  :name 'shadow-2d-proj-lod 
;;  '(:in ((sampler :sampler-2d-shadow) (coord :vec4) (lod :float)) :out :int )
;;  :transform "shadow2DProjLod(~a, ~a, ~a)"
;;  :context-restriction '((:330)))
