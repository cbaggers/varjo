(in-package :varjo)

;;-------------------------------------------------------------------------

(def-metadata-kind spatial-meta ()
  in-space)

(defmethod combine-metadata ((meta-a spatial-meta)
                             (meta-b spatial-meta))
  (let ((space-a (in-space meta-a))
        (space-b (in-space meta-b)))
    (if (eq space-a space-b)
        meta-a
        (error "Space Analysis Failed: Could not establish at compile time which
space the resulting svec was in between:
~a
and
~a" space-a space-b))))

;;-------------------------------------------------------------------------

(def-metadata-kind some-scope-decl (:binds-to :scope)
  kind
  max-vertices)
