(in-package :varjo)




(def-metadata-type space-meta ()
  (uniform-name))

(defmethod on-value (()))

(defmethod merge-metadata ((a space-meta) (b space-meta))
  )
