(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Swizzle

(v-defspecial swizzle (vec-form components)
  :args-valid t
  :v-place-index 0
  :return
  (let* ((vec-obj (compile-form vec-form env))
         (vec-type (primary-type vec-obj))
         (comp-string (extract-swizzle-string vec-type components))
         (element-type (v-element-type vec-type))
         (r-type (if (= (length comp-string) 1)
                     element-type
                     (vec-of element-type
                             (length comp-string))))
         (r-type (set-flow-id r-type (flow-id!)))
         (type-set (make-type-set r-type)))
    (values
     (copy-compiled
      vec-obj
      :type-set type-set
      :current-line (gen-swizzle-string vec-obj comp-string)
      :place-tree (calc-place-tree this (list vec-obj)))
     env)))

(defun extract-swizzle-string (vec-type components)
  (assert (swizzlable-p vec-type) () 'cannot-swizzle-this-type
          :vtype vec-type)
  (assert (keywordp components) () 'swizzle-keyword :item components)
  (let* ((components (string-downcase components))
         (swizzle-set (let ((key (char components 0)))
                        (cond
                          ((find key "xyzw") "xyzw")
                          ((find key "rgba") "rgba")
                          ((find key "stpq") "stpq")
                          (t (error "Varjo: swizzle form for ~a invalid: ~a"
                                    (type->type-spec vec-type)
                                    components)))))
         (allowed (subseq swizzle-set 0 (first (v-dimensions vec-type))))
         (new-len (length components)))
    (assert (and (every λ(find _ allowed) components)
                 (>= new-len 1)
                 (<= new-len 4))
            () "Varjo: swizzle form for ~a invalid: ~a"
            (type->type-spec vec-type)
            components)
    components))

;;------------------------------------------------------------
