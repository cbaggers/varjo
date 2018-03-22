(in-package :vari.glsl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Switch

;; {TODO} check keys
(v-defspecial discard ()
  :return
  (let* ((discarded (type-spec->type 'v-discarded (flow-id!)))
         (type-set (make-type-set discarded)))
    (assert (typep (stage env) 'fragment-stage) ()
            'discard-not-in-fragment-stage
            :stage (type-of (stage env)))
    (values
     (make-compiled :type-set type-set
                    :current-line "discard"
                    :used-types nil
                    :pure nil)
     env)))

;;------------------------------------------------------------
