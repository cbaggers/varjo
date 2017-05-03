(in-package :varjo.tests)
(5am:in-suite metadata-tests)

;;------------------------------------------------------------
;; Helper data

(eval-when (:compile-toplevel :load-toplevel :execute)
  (varjo:v-deftype some-ephem-g ()
                   :vec4
                   :valid-metadata-kinds some-meta)

  (varjo:add-alternate-type-name 'some-ephem 'some-ephem-g))

(varjo:def-metadata-kind some-meta ()
  val)

;;------------------------------------------------------------
;; Tests


(5am:def-test metadata-0 (:suite metadata-tests)
  (finishes-p
   (compile-vert () :450 t
     ;; this is to text #'inject-implicit-uniform which is used
     ;; by the metadata api
     (varjo::lisp-code-as-uniform foo :int 10)
     (v! 0 0 0 1))))

(def-vbind-test metadata-1 (:suite metadata-tests) (meta)
    (is-true (equal (val meta) 10))
  (flow-id-scope
    (let ((env (make-env :vertex nil '((x some-ephem)))))
      (vbind (c e)
          (compile-form '(let ((y x))
                          (declare (some-meta (:val 10) y))
                          y)
                        env)
        (varjo::metadata-for-flow-id 'some-meta (flow-ids c) e)))))
