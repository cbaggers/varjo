;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-1d :int)
  :int)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-1d :int)
  :int)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-1d :int)
  :int)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-1d-shadow :int)
  :int)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-2d :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-2d :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-2d :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-cube :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-cube :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-cube :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-2d-shadow :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-cube-shadow :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-2d-rect :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-2d-rect :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-2d-rect :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-2d-rect-shadow :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-1d-array :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-1d-array :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-1d-array :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-1d-array-shadow :int)
  :ivec2)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-3d :int)
  :ivec3)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-3d :int)
  :ivec3)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-3d :int)
  :ivec3)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-cube-array :int)
  :ivec3)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-cube-array-shadow :int)
  :ivec3)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-2d-array :int)
  :ivec3)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-2d-array :int)
  :ivec3)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-2d-array :int)
  :ivec3)

(v-defun :texture-size (sampler lod &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-2d-array-shadow :int)
  :ivec3)

(v-defun :texture-size (sampler &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-buffer)
  :int)

(v-defun :texture-size (sampler &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-buffer)
  :int)

(v-defun :texture-size (sampler &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-buffer)
  :int)

(v-defun :texture-size (sampler &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-2d-ms)
  :ivec2)

(v-defun :texture-size (sampler &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-2d-ms)
  :ivec2)

(v-defun :texture-size (sampler &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-2d-ms)
  :ivec2)

(v-defun :texture-size (sampler &context (:330 :440))
  "textureSize(~a, ~a)"
  (:sampler-2d-ms-array)
  :ivec3)

(v-defun :texture-size (sampler &context (:330 :440))
  "textureSize(~a, ~a)"
  (:isampler-2d-ms-array)
  :ivec3)

(v-defun :texture-size (sampler &context (:330 :440))
  "textureSize(~a, ~a)"
  (:usampler-2d-ms-array)
  :ivec3)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:isampler-1d :float :float)
  :ivec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:isampler-1d-array :vec2 :float)
  :ivec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:isampler-2d :vec2 :float)
  :ivec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:isampler-2d-array :vec3 :float)
  :ivec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:isampler-3d :vec3 :float)
  :ivec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:isampler-cube :vec3 :float)
  :ivec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-1d :float :float)
  :vec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-1d-array :vec2 :float)
  :vec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-1d-array-shadow :vec3 :float)
  :float)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-2d-array-shadow :vec3 :float)
  :float)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-1d-shadow :vec3 :float)
  :float)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-2d :vec2 :float)
  :vec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-2d-array :vec3 :float)
  :vec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-2d-shadow :vec3 :float)
  :float)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-3d :vec3 :float)
  :vec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-cube :vec3 :float)
  :vec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-cube-shadow :vec4 :float)
  :float)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:usampler-1d :float :float)
  :uvec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:usampler-1d-array :vec2 :float)
  :uvec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:usampler-2d :vec2 :float)
  :uvec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:usampler-2d-array :vec3 :float)
  :uvec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:usampler-3d :vec3 :float)
  :uvec4)

(v-defun :texture (sampler p bias &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:usampler-cube :vec3 :float)
  :uvec4)

(v-defun :texture (sampler p compare &context (:330 :440))
  "texture(~a, ~a, ~a)"
  (:sampler-cube-array-shadow :vec4 :float)
  :float)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:isampler-1d :float)
  :ivec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:isampler-1d-array :vec2)
  :ivec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:isampler-2d :vec2)
  :ivec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:isampler-2d-array :vec3)
  :ivec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:isampler-2d-rect :vec2)
  :ivec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:isampler-3d :vec3)
  :ivec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:isampler-cube :vec3)
  :ivec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-1d :float)
  :vec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-1d-array :vec2)
  :vec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-1d-array-shadow :vec3)
  :float)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-1d-shadow :vec3)
  :float)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-2d :vec2)
  :vec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-2d-array :vec3)
  :vec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-2d-array-shadow :vec4)
  :float)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-2d-rect :vec2)
  :vec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-2d-rect-shadow :vec3)
  :float)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-2d-shadow :vec3)
  :float)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-3d :vec3)
  :vec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-cube :vec3)
  :vec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:sampler-cube-shadow :vec4)
  :float)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:usampler-1d :float)
  :uvec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:usampler-1d-array :vec2)
  :uvec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:usampler-2d :vec2)
  :uvec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:usampler-2d-array :vec3)
  :uvec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:usampler-2d-rect :vec2)
  :uvec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:usampler-3d :vec3)
  :uvec4)

(v-defun :texture (sampler p &context (:330 :440))
  "texture(~a, ~a)"
  (:usampler-cube :vec3)
  :uvec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:isampler-1d :vec2)
  :ivec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:isampler-1d :vec4)
  :ivec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:isampler-2d :vec3)
  :ivec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:isampler-2d :vec4)
  :ivec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:isampler-2d-rect :vec3)
  :ivec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:isampler-2d-rect :vec4)
  :ivec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:isampler-3d :vec4)
  :ivec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-1d :vec2)
  :vec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-1d :vec4)
  :vec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-1d-shadow :vec4)
  :float)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-2d :vec3)
  :vec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-2d :vec4)
  :vec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-2d-rect :vec3)
  :vec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-2d-rect :vec4)
  :vec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-2d-rect-shadow :vec4)
  :float)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-2d-shadow :vec4)
  :float)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:sampler-3d :vec4)
  :vec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:usampler-1d :vec2)
  :uvec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:usampler-1d :vec4)
  :uvec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:usampler-2d :vec3)
  :uvec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:usampler-2d :vec4)
  :uvec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:usampler-2d-rect :vec3)
  :uvec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:usampler-2d-rect :vec4)
  :uvec4)

(v-defun :texture-proj (sampler p &context (:330 :440))
  "textureProj(~a, ~a)"
  (:usampler-3d :vec4)
  :uvec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:isampler-1d :vec2 :float)
  :ivec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:isampler-1d :vec4 :float)
  :ivec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:isampler-2d :vec3 :float)
  :ivec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:isampler-2d :vec4 :float)
  :ivec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:isampler-3d :vec4 :float)
  :ivec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:sampler-1d :vec2 :float)
  :vec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:sampler-1d :vec4 :float)
  :vec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:sampler-1d-shadow :vec4 :float)
  :float)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:sampler-2d :vec3 :float)
  :vec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:sampler-2d :vec4 :float)
  :vec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:sampler-2d-rect :vec3 :float)
  :vec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:sampler-2d-rect :vec4 :float)
  :vec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:sampler-2d-shadow :vec4 :float)
  :float)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:sampler-3d :vec4 :float)
  :vec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:usampler-1d :vec2 :float)
  :uvec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:usampler-1d :vec4 :float)
  :uvec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:usampler-2d :vec3 :float)
  :uvec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:usampler-2d :vec4 :float)
  :uvec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:usampler-2d-rect :vec3 :float)
  :uvec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:usampler-2d-rect :vec4 :float)
  :uvec4)

(v-defun :texture-proj (sampler p bias &context (:330 :440))
  "textureProj(~a, ~a, ~a)"
  (:usampler-3d :vec4 :float)
  :uvec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:isampler-1d :float :float)
  :ivec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:isampler-1d-array :vec2 :float)
  :ivec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:isampler-2d :vec2 :float)
  :ivec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:isampler-2d-array :vec3 :float)
  :ivec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:isampler-3d :vec3 :float)
  :ivec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:isampler-cube :vec3 :float)
  :ivec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:sampler-1d :float :float)
  :vec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:sampler-1d-array :vec2 :float)
  :vec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:sampler-1d-array-shadow :vec3 :float)
  :float)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:sampler-1d-shadow :vec3 :float)
  :float)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:sampler-2d :vec2 :float)
  :vec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:sampler-2d-array :vec3 :float)
  :ivec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:sampler-2d-shadow :vec3 :float)
  :float)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:sampler-3d :vec3 :float)
  :vec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:sampler-cube :vec3 :float)
  :vec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:usampler-1d :float :float)
  :uvec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:usampler-1d-array :vec2 :float)
  :uvec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:usampler-2d :vec2 :float)
  :uvec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:usampler-2d-array :vec3 :float)
  :uvec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:usampler-3d :vec3 :float)
  :uvec4)

(v-defun :texture-lod (sampler p lod &context (:330 :440))
  "textureLod(~a, ~a, ~a)"
  (:usampler-cube :vec3 :float)
  :uvec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d :float :int :float)
  :ivec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d-array :vec2 :int :float)
  :ivec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d :vec2 :ivec2 :float)
  :ivec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d-array :vec3 :ivec2 :float)
  :ivec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:isampler-3d :vec3 :ivec3 :float)
  :ivec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d :float :int :float)
  :vec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d-array :vec2 :int :float)
  :vec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d-array-shadow :vec3 :int :float)
  :float)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d-shadow :vec3 :int :float)
  :float)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d :vec2 :ivec2 :float)
  :vec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d-array :vec3 :ivec2 :float)
  :vec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d-shadow :vec3 :ivec2 :float)
  :float)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:sampler-3d :vec3 :ivec3 :float)
  :vec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d :float :int :float)
  :uvec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d-array :vec2 :int :float)
  :uvec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d :vec2 :ivec2 :float)
  :uvec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d-array :vec3 :ivec2 :float)
  :uvec4)

(v-defun :texture-offset (sampler p offset bias &context (:330 :440))
  "textureOffset(~a, ~a, ~a, ~a)"
  (:usampler-3d :vec3 :ivec3 :float)
  :uvec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:isampler-1d :float :int)
  :ivec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:isampler-1d-array :vec2 :int)
  :ivec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:isampler-2d :vec2 :ivec2)
  :ivec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:isampler-2d-array :vec3 :ivec2)
  :ivec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:isampler-2d-rect :vec2 :ivec2)
  :ivec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:isampler-3d :vec3 :ivec3)
  :ivec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-1d :float :int)
  :vec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-1d-array :vec2 :int)
  :vec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-1d-array-shadow :vec3 :int)
  :float)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-1d-shadow :vec3 :int)
  :float)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-2d :vec2 :ivec2)
  :vec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-2d-array :vec3 :ivec2)
  :vec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-2d-rect :vec2 :ivec2)
  :vec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-2d-rect-shadow :vec3 :ivec2)
  :float)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-2d-shadow :vec3 :ivec2)
  :float)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:sampler-3d :vec3 :ivec3)
  :vec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:usampler-1d :float :int)
  :uvec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:usampler-1d-array :vec2 :int)
  :uvec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:usampler-2d :vec2 :ivec2)
  :uvec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:usampler-2d-array :vec3 :ivec2)
  :uvec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:usampler-2d-rect :vec2 :ivec2)
  :uvec4)

(v-defun :texture-offset (sampler p offset &context (:330 :440))
  "textureOffset(~a, ~a, ~a)"
  (:usampler-3d :vec3 :ivec3)
  :uvec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:isampler-1d :int :int)
  :ivec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:isampler-1d-array :ivec2 :int)
  :ivec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:isampler-2d :ivec2 :int)
  :ivec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:isampler-2d-array :ivec3 :int)
  :ivec4)

(v-defun :texel-fetch (sampler p sample &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:isampler-2d-ms :ivec2 :int)
  :ivec4)

(v-defun :texel-fetch (sampler p sample &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:isampler-2d-ms-array :ivec3 :int)
  :ivec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:isampler-3d :ivec3 :int)
  :ivec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:sampler-1d :int :int)
  :vec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:sampler-1d-array :ivec2 :int)
  :vec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:sampler-2d :ivec2 :int)
  :vec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:sampler-2d-array :ivec3 :int)
  :vec4)

(v-defun :texel-fetch (sampler p sample &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:sampler-2d-ms :ivec2 :int)
  :vec4)

(v-defun :texel-fetch (sampler p sample &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:sampler-2d-ms-array :ivec3 :int)
  :vec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:sampler-3d :ivec3 :int)
  :vec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:usampler-1d :int :int)
  :uvec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:usampler-1d-array :ivec2 :int)
  :uvec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:usampler-2d :ivec2 :int)
  :uvec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:usampler-2d-array :ivec3 :int)
  :uvec4)

(v-defun :texel-fetch (sampler p sample &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:usampler-2d-ms :ivec2 :int)
  :uvec4)

(v-defun :texel-fetch (sampler p sample &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:usampler-2d-ms-array :ivec3 :int)
  :uvec4)

(v-defun :texel-fetch (sampler p lod &context (:330 :440))
  "texelFetch(~a, ~a, ~a)"
  (:usampler-3d :ivec3 :int)
  :uvec4)

(v-defun :texel-fetch (sampler p &context (:330 :440))
  "texelFetch(~a, ~a)"
  (:isampler-2d-rect :ivec2)
  :ivec4)

(v-defun :texel-fetch (sampler p &context (:330 :440))
  "texelFetch(~a, ~a)"
  (:isampler-buffer :int)
  :ivec4)

(v-defun :texel-fetch (sampler p &context (:330 :440))
  "texelFetch(~a, ~a)"
  (:sampler-2d-rect :ivec2)
  :vec4)

(v-defun :texel-fetch (sampler p &context (:330 :440))
  "texelFetch(~a, ~a)"
  (:sampler-buffer :int)
  :vec4)

(v-defun :texel-fetch (sampler p &context (:330 :440))
  "texelFetch(~a, ~a)"
  (:usampler-2d-rect :ivec2)
  :uvec4)

(v-defun :texel-fetch (sampler p &context (:330 :440))
  "texelFetch(~a, ~a)"
  (:usampler-buffer :int)
  :uvec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d :int :int :int)
  :ivec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d-array :ivec2 :int :int)
  :ivec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d :ivec2 :int :ivec2)
  :ivec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d-array :ivec3 :int :ivec2)
  :ivec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:isampler-3d :ivec3 :int :ivec3)
  :ivec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d :int :int :int)
  :vec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d-array :ivec2 :int :int)
  :vec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d :ivec2 :int :ivec2)
  :vec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d-array :ivec3 :int :ivec2)
  :vec4)

(v-defun :texel-fetch-offset (sampler p offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d-rect :ivec2 :ivec2)
  :vec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:sampler-3d :ivec3 :int :ivec3)
  :vec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d :int :int :int)
  :uvec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d-array :ivec2 :int :int)
  :uvec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d :ivec2 :int :ivec2)
  :uvec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d-array :ivec3 :int :ivec2)
  :uvec4)

(v-defun :texel-fetch-offset (sampler p lod offset &context (:330 :440))
  "texelFetchOffset(~a, ~a, ~a, ~a)"
  (:usampler-3d :ivec3 :int :ivec3)
  :uvec4)

(v-defun :texel-fetch-offset (sampler p offset &context (:|330|))
  "texelFetchOffset(~a, ~a, ~a)"
  (:isampler-2d-rect :ivec2 :ivec2)
  :ivec4)

(v-defun :texel-fetch-offset (sampler p offset &context (:|330|))
  "texelFetchOffset(~a, ~a, ~a)"
  (:usampler-2d-rect :ivec2 :ivec2)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d :vec2 :int :float)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d :vec4 :int :float)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d :vec3 :ivec2 :float)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d :vec4 :ivec2 :float)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:isampler-3d :vec4 :ivec3 :float)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d :vec2 :int :float)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d :vec4 :int :float)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d-shadow :vec4 :int :float)
  :float)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d :vec3 :ivec2 :float)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d :vec4 :ivec2 :float)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d-shadow :vec4 :ivec2 :float)
  :float)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:sampler-3d :vec4 :ivec3 :float)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d :vec2 :int :float)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d :vec4 :int :float)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d :vec3 :ivec2 :float)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d :vec4 :ivec2 :float)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset bias &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a, ~a)"
  (:usampler-3d :vec4 :ivec3 :float)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:isampler-1d :vec2 :int)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:isampler-1d :vec4 :int)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:isampler-2d :vec3 :ivec2)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:isampler-2d :vec4 :ivec2)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:isampler-2d-rect :vec3 :ivec2)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:isampler-2d-rect :vec4 :ivec2)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:isampler-3d :vec4 :ivec3)
  :ivec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-1d :vec2 :int)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-1d :vec4 :int)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-1d-shadow :vec4 :int)
  :float)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-2d :vec3 :ivec2)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-2d :vec4 :ivec2)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-2d-rect :vec3 :ivec2)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-2d-rect :vec4 :ivec2)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-2d-rect-shadow :vec4 :ivec2)
  :float)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-2d-shadow :vec4 :ivec2)
  :float)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:sampler-3d :vec4 :ivec3)
  :vec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:usampler-1d :vec2 :int)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:usampler-1d :vec4 :int)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:usampler-2d :vec3 :ivec2)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:usampler-2d :vec4 :ivec2)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:usampler-2d-rect :vec3 :ivec2)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:usampler-2d-rect :vec4 :ivec2)
  :uvec4)

(v-defun :texture-proj-offset (sampler p offset &context (:330 :440))
  "textureProjOffset(~a, ~a, ~a)"
  (:usampler-3d :vec4 :ivec3)
  :uvec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d :float :float :int)
  :ivec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d-array :vec2 :float :int)
  :ivec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d :vec2 :float :ivec2)
  :ivec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d-array :vec3 :float :ivec2)
  :ivec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-3d :vec3 :float :ivec3)
  :ivec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d :float :float :int)
  :vec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d-array :vec2 :float :int)
  :vec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d-array-shadow :vec3 :float :int)
  :float)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d-shadow :vec3 :float :int)
  :float)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d :vec2 :float :ivec2)
  :vec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d-array :vec3 :float :ivec2)
  :vec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d-shadow :vec3 :float :ivec2)
  :float)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-3d :vec3 :float :ivec3)
  :vec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d :float :float :int)
  :uvec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d-array :vec2 :float :int)
  :uvec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d :vec2 :float :ivec2)
  :uvec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d-array :vec3 :float :ivec2)
  :uvec4)

(v-defun :texture-lod-offset (sampler p lod offset &context (:330 :440))
  "textureLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-3d :vec3 :float :ivec3)
  :uvec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:isampler-1d :vec2 :float)
  :ivec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:isampler-1d :vec4 :float)
  :ivec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:isampler-2d :vec3 :float)
  :ivec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:isampler-2d :vec4 :float)
  :ivec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:isampler-3d :vec4 :float)
  :ivec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:sampler-1d :vec4 :float)
  :vec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:sampler-1d-shadow :vec4 :float)
  :float)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:sampler-2d :vec3 :float)
  :vec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:sampler-2d :vec4 :float)
  :vec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:sampler-2d-shadow :vec4 :float)
  :float)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:sampler-3d :vec4 :float)
  :vec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:usampler-1d :vec2 :float)
  :uvec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:usampler-1d :vec4 :float)
  :uvec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:usampler-2d :vec3 :float)
  :uvec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:usampler-2d :vec4 :float)
  :uvec4)

(v-defun :texture-proj-lod (sampler p lod &context (:330 :440))
  "textureProjLod(~a, ~a, ~a)"
  (:usampler-3d :vec4 :float)
  :uvec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d :vec2 :float :int)
  :ivec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-1d :vec4 :float :int)
  :ivec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d :vec3 :float :ivec2)
  :ivec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-2d :vec4 :float :ivec2)
  :ivec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:isampler-3d :vec4 :float :ivec3)
  :ivec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d :vec2 :float :int)
  :vec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d :vec4 :float :int)
  :vec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-1d-shadow :vec4 :float :int)
  :float)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d :vec3 :float :ivec2)
  :vec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d :vec4 :float :ivec2)
  :vec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-2d-shadow :vec4 :float :ivec2)
  :float)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:sampler-3d :vec4 :float :ivec3)
  :vec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d :vec2 :float :int)
  :uvec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-1d :vec4 :float :int)
  :uvec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d :vec3 :float :ivec2)
  :uvec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-2d :vec4 :float :ivec2)
  :uvec4)

(v-defun :texture-proj-lod-offset (sampler p lod offset &context (:330 :440))
  "textureProjLodOffset(~a, ~a, ~a, ~a)"
  (:usampler-3d :vec4 :float :ivec3)
  :uvec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:isampler-1d :float :float :float)
  :ivec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:isampler-1d-array :vec2 :float :float)
  :ivec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:isampler-2d :vec2 :vec2 :vec2)
  :ivec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:isampler-2d-array :vec3 :vec2 :vec2)
  :ivec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:isampler-2d-rect :vec2 :vec2 :vec2)
  :ivec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:isampler-3d :vec3 :vec3 :vec3)
  :ivec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:isampler-cube :vec3 :vec3 :vec3)
  :ivec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-1d :float :float :float)
  :vec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-1d-array :vec2 :float :float)
  :vec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-1d-array-shadow :vec3 :float :float)
  :float)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-1d-shadow :vec3 :float :float)
  :float)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d :vec2 :vec2 :vec2)
  :vec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d-array :vec3 :vec2 :vec2)
  :vec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d-array-shadow :vec4 :vec2 :vec2)
  :float)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d-rect :vec2 :vec2 :vec2)
  :vec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d-rect-shadow :vec3 :vec2 :vec2)
  :float)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d-shadow :vec3 :vec2 :vec2)
  :float)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-3d :vec3 :vec3 :vec3)
  :vec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-cube :vec3 :vec3 :vec3)
  :vec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:sampler-cube-shadow :vec4 :vec3 :vec3)
  :float)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:usampler-1d :float :float :float)
  :uvec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:usampler-1d-array :vec2 :float :float)
  :uvec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:usampler-2d :vec2 :vec2 :vec2)
  :uvec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:usampler-2d-array :vec3 :vec2 :vec2)
  :uvec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:usampler-2d-rect :vec2 :vec2 :vec2)
  :uvec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:usampler-3d :vec3 :vec3 :vec3)
  :uvec4)

(v-defun :texture-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureGrad(~a, ~a, ~a, ~a)"
  (:usampler-cube :vec3 :vec3 :vec3)
  :uvec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-1d :float :float :float :int)
  :ivec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-1d-array :vec2 :float :float :int)
  :ivec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-2d :vec2 :vec2 :vec2 :ivec2)
  :ivec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-2d-array :vec3 :vec2 :vec2 :ivec2)
  :ivec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-2d-rect :vec2 :vec2 :vec2 :ivec2)
  :ivec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-3d :vec3 :vec3 :vec3 :ivec3)
  :ivec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-1d :float :float :float :int)
  :vec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-1d-array :vec2 :float :float :int)
  :vec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-1d-array-shadow :vec3 :float :float :int)
  :float)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-1d-shadow :vec3 :float :float :int)
  :float)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d :vec2 :vec2 :vec2 :ivec2)
  :vec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d-array :vec3 :vec2 :vec2 :ivec2)
  :vec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d-array-shadow :vec4 :vec2 :vec2 :ivec2)
  :float)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d-rect :vec2 :vec2 :vec2 :ivec2)
  :vec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d-rect-shadow :vec3 :vec2 :vec2 :ivec2)
  :float)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d-shadow :vec3 :vec2 :vec2 :ivec2)
  :float)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-3d :vec3 :vec3 :vec3 :ivec3)
  :vec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-1d :float :float :float :int)
  :uvec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-1d-array :vec2 :float :float :int)
  :uvec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-2d :vec2 :vec2 :vec2 :ivec2)
  :uvec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-2d-array :vec3 :vec2 :vec2 :ivec2)
  :uvec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-2d-rect :vec2 :vec2 :vec2 :ivec2)
  :uvec4)

(v-defun :texture-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-3d :vec3 :vec3 :vec3 :ivec3)
  :uvec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:isampler-1d :vec2 :float :float)
  :ivec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:isampler-1d :vec4 :float :float)
  :ivec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:isampler-2d :vec3 :vec2 :vec2)
  :ivec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:isampler-2d :vec4 :vec2 :vec2)
  :ivec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:isampler-2d-rect :vec3 :vec2 :vec2)
  :ivec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:isampler-2d-rect :vec4 :vec2 :vec2)
  :ivec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:isampler-3d :vec4 :vec3 :vec3)
  :ivec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-1d :vec2 :float :float)
  :vec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-1d :vec4 :float :float)
  :vec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-1d-shadow :vec4 :float :float)
  :float)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d :vec3 :vec2 :vec2)
  :vec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d :vec4 :vec2 :vec2)
  :vec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d-rect :vec3 :vec2 :vec2)
  :vec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d-rect :vec4 :vec2 :vec2)
  :vec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d-rect-shadow :vec4 :vec2 :vec2)
  :float)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-2d-shadow :vec4 :vec2 :vec2)
  :float)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:sampler-3d :vec4 :vec3 :vec3)
  :vec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:usampler-1d :vec2 :float :float)
  :uvec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:usampler-1d :vec4 :float :float)
  :uvec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:usampler-2d :vec3 :vec2 :vec2)
  :uvec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:usampler-2d :vec4 :vec2 :vec2)
  :uvec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:usampler-2d-rect :vec3 :vec2 :vec2)
  :uvec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:usampler-2d-rect :vec4 :vec2 :vec2)
  :uvec4)

(v-defun :texture-proj-grad (sampler p dpdx dpdy &context (:330 :440))
  "textureProjGrad(~a, ~a, ~a, ~a)"
  (:usampler-3d :vec4 :vec3 :vec3)
  :uvec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-1d :vec2 :float :float :int)
  :uvec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-1d :vec4 :float :float :int)
  :uvec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-2d :vec3 :vec2 :vec2 :vec2)
  :uvec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-2d :vec4 :vec2 :vec2 :vec2)
  :uvec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-2d-rect :vec3 :vec2 :vec2 :ivec2)
  :uvec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-2d-rect :vec4 :vec2 :vec2 :ivec2)
  :uvec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:isampler-3d :vec4 :vec3 :vec3 :vec3)
  :uvec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-1d :vec2 :float :float :int)
  :vec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-1d :vec4 :float :float :int)
  :vec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-1d-shadow :vec4 :float :float :int)
  :float)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d :vec3 :vec2 :vec2 :vec2)
  :vec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d :vec4 :vec2 :vec2 :vec2)
  :vec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d-rect :vec3 :vec2 :vec2 :ivec2)
  :vec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d-rect :vec4 :vec2 :vec2 :ivec2)
  :vec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d-rect-shadow :vec4 :vec2 :vec2 :ivec2)
  :float)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-2d-shadow :vec4 :vec2 :vec2 :vec2)
  :float)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:sampler-3d :vec4 :vec3 :vec3 :vec3)
  :vec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-1d :vec2 :float :float :int)
  :ivec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-1d :vec4 :float :float :int)
  :ivec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-2d :vec3 :vec2 :vec2 :vec2)
  :ivec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-2d :vec4 :vec2 :vec2 :vec2)
  :ivec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-2d-rect :vec3 :vec2 :vec2 :ivec2)
  :ivec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-2d-rect :vec4 :vec2 :vec2 :ivec2)
  :ivec4)

(v-defun :texture-proj-grad-offset (sampler p dpdx dpdy offset &context (:330 :440))
  "textureProjGradOffset(~a, ~a, ~a, ~a, ~a)"
  (:usampler-3d :vec4 :vec3 :vec3 :vec3)
  :ivec4)
