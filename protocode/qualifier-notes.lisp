
(defvar *glsl-qualifiers*
  '(:attribute
    :buffer
    :centroid
    :coherent
    :const
    :flat
    :highp
    :in
    :invariant
    :lowp
    :mediump
    :noperspective
    :out
    :readonly
    :restrict
    :sample
    :shared
    :smooth
    :uniform
    :varying
    :volatile
    :writeonly))

;; Qualifier order
;;
;; Unless you are in OpenGL 4.2 or ARB_shading_language_420pack,
;; qualifiers always come in a particular order. For non-parameter
;; values, the order is always this:
;;
;; invariant-qualifier interpolation-qualifier layout-qualifier
;; other-storage-qualifier precision-qualifier
;;
;; The centroid qualifier, if present, must immediately precede in or
;; out. For the purpose of ordering, it is considered part of the in or
;; out storage qualifier, not part of the interpolation qualifier.
;;
;; OpenGL 4.2 or ARB_shading_language_420pack removes the ordering
;; restriction in most cases. centroid still has to immediate precede in
;; or out. It also allows multiple layout qualifiers, but you can still
;; only use one qualifier from most other groups (there can be multiple
;; memory qualifiers). Also, all qualifiers must still come before the
;; type specifier. The groups of qualifiers match the main headings
;; above: storage, layout, precision, etc.
