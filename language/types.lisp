(in-package :varjo)

(%vdefstruct vgl-per-vertex-v (:slot-prefix per-vertex
                                            :context-restriction ((:330) :vertex))
  (position :vec4 "gl_Position")
  (point-size :float "gl_PointSize")
  (clip-distance (:float t) "gl_ClipDistance")
  (clip-vertex :vec4 "gl_ClipVertex"))

(%vdefstruct vgl-per-vertex-g (:slot-prefix per-vertex
                                            :context-restriction ((:330) :fragment))
  (position :vec4 "gl_Position")
  (point-size :float "gl_PointSize")
  (clip-distance (:float t) "gl_ClipDistance"))


;; new format
;; (vdefstruct thing ()
;;   (a v-float)
;;   (to-long-to-blah v-int :accessor b))
