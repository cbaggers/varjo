(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Geometry

;; points
;; line-strip
;; triangle-strip

(def-metadata-kind output-primitive (:binds-to :scope)
  kind
  max-vertices)

;;------------------------------------------------------------
