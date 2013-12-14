;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(defparameter *glsl-variables*
  '((t
     (:gl-max-clip-distances v-int t)
     (:gl-max-clip-planes v-int t)
     (:gl-max-draw-Buffers v-int t)
     (:gl-max-texture-units v-int t)
     (:gl-max-texture-coords v-int t)
     (:gl-max-geometry-texture-image-units v-int t)
     (:gl-max-texture-image-units v-int t)
     (:gl-max-vertex-attribs v-int t)
     (:gl-max-vertex-texture-image-units v-int t)
     (:gl-max-combined-texture-image-units v-int t)
     (:gl-max-geometry-varying-components v-int t)
     (:gl-max-varying-floats v-int t)
     (:gl-max-geometry-output-vertices v-int t)
     (:gl-max-fragment-uniform-components v-int t)
     (:gl-max-geometry-total-output-components v-int t)
     (:gl-max-geometry-uniform-components v-int t)
     (:gl-max-vertex-uniform-components v-int t))
    (:vertex 
     (:gl-vertex-id v-int t)
     (:gl-instance-id v-int t)
     (:gl-color v-vec4 t)
     (:gl-secondary-color v-vec4 t)
     (:gl-normal v-vec3 t)
     (:gl-vertex v-vec4 t)
     (:gl-multi-tex-coord-0 v-vec4 t)
     (:gl-multi-tex-coord-1 v-vec4 t)
     (:gl-multi-tex-coord-2 v-vec4 t)
     (:gl-multi-tex-coord-3 v-vec4 t)
     (:gl-multi-tex-coord-4 v-vec4 t)
     (:gl-multi-tex-coord-5 v-vec4 t)
     (:gl-multi-tex-coord-6 v-vec4 t)
     (:gl-multi-tex-coord-7 v-vec4 t)
     (:gl-fog-coord v-float t)
     (:gl-position v-vec4 t)
     (:gl-point-size v-float)
     (:gl-clip-distance (v-float t))
     (:gl-clip-vertex v-vec4)
     (:gl-front-color v-vec4)
     (:gl-back-color v-vec4)
     (:gl-front-secondary-color v-vec4)
     (:gl-back-secondary-color v-vec4)
     (:gl-tex-coord (v-vec4 t))
     (:gl-fog-frag-coord v-float))
    (:fragment 
     (:gl-frag-coord v-vec4 t)
     (:gl-front-facing v-bool  t)
     (:gl-clip-distance (v-float t) t)
     (:gl-point-coord v-vec2  t)
     (:gl-primitive-id v-int t)
     (:gl-frag-depth v-float nil))
    (:geometry
     (:gl-in (vgl-per-vertex-g t) t)
     (:gl-primitive-id-in v-int t)
     (:gl-position v-vec4)
     (:gl-point-size v-float)
     (:gl-clip-distance (v-float t))
     (:gl-primitive-id v-int)
     (:gl-layer v-int))))

