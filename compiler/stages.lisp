(in-package :varjo)
(in-readtable fn:fn-reader)

(defgeneric copy-stage (stage &key)
  (:method ((stage stage)
            &key (input-variables nil iv-set)
              (uniform-variables nil uv-set)
              (context nil c-set)
              (lisp-code nil lc-set)
              (previous-stage nil ps-set)
              (stemcells-allowed nil sa-set))
    (make-instance
     (type-of stage)
     :input-variables (if iv-set
                          input-variables
                          (input-variables stage))
     :uniform-variables (if uv-set
                            uniform-variables
                            (uniform-variables stage))
     :context (if c-set
                  context
                  (context stage))
     :lisp-code (if lc-set
                    lisp-code
                    (lisp-code stage))
     :previous-stage (if ps-set
                         previous-stage
                         (previous-stage stage))
     :stemcells-allowed (if sa-set
                            stemcells-allowed
                            (stemcells-allowed stage)))))

;;------------------------------------------------------------

(defmethod extract-stage-type ((stage vertex-stage))
  :vertex)

(defmethod extract-stage-type ((stage tesselation-control-stage))
  :tesselation-control)

(defmethod extract-stage-type ((stage tesselation-evaluation-stage))
  :tesselation-evaluation)

(defmethod extract-stage-type ((stage geometry-stage))
  :geometry)

(defmethod extract-stage-type ((stage fragment-stage))
  :fragment)

(defmethod extract-stage-type ((env environment))
  (extract-stage-type (stage env)))

(defmethod extract-stage-type ((ppp post-compile-process))
  (extract-stage-type (stage ppp)))

;;------------------------------------------------------------

(defmethod stage-is ((stage stage) name)
  (typep stage (stage-kind-to-type name)))

(defmethod stage-is ((env environment) name)
  (stage-is (stage env) name))

(defmethod stage-is ((ppp post-compile-process) name)
  (stage-is (stage ppp) name))

;;------------------------------------------------------------
