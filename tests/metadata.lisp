(in-package :varjo.tests)
(5am:in-suite metadata-tests)

;;------------------------------------------------------------
;; Helper data

(eval-when (:compile-toplevel :load-toplevel :execute)
  (varjo:v-deftype some-shadow-g ()
                   :vec4
                   :valid-metadata-kinds some-meta)

  (varjo:add-alternate-type-name 'some-shadow 'some-shadow-g))

(varjo:define-metadata-kind some-meta ()
  val)

;;------------------------------------------------------------

(varjo:v-deftype some-ephemeral () nil)

;;------------------------------------------------------------
;; Tests


(5am:def-test metadata-0 (:suite metadata-tests)
  (finishes-p
   (compile-vert () :410 t
     ;; this is to text #'inject-implicit-uniform which is used
     ;; by the metadata api
     (lisp-code-as-uniform foo :int 10)
     (v! 0 0 0 1))))

(define-vbind-test metadata-1 (:suite metadata-tests) (meta)
    (is-true (equal (val meta) 10))
  (flow-id-scope
    (let ((env (make-env :vertex nil '((x some-shadow)))))
      (vbind (c e)
          (compile-form
           '(let ((y x))
             (declare (some-meta (:val 10) y))
             y)
           env)
        (metadata-for-flow-id 'some-meta (flow-ids c) e)))))

(5am:def-test metadata-2 (:suite metadata-tests)
  (signals varjo-conditions:arrays-cannot-hold-ephemeral-types
    (compile-frag () :410 t
      (make-array 10 :element-type some-ephemeral)
      (vec4 1))))
