(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Swizzle

(v-defmacro s~ (&rest args) `(swizzle ,@args))

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
      :node-tree (ast-node! 'swizzle
                            `(,(node-tree vec-obj) ,components)
                            type-set env env)
      :place-tree (calc-place-tree this (list vec-obj)))
     env)))

(defun extract-swizzle-string (vec-type components)
  (assert (swizzlable-p vec-type) () 'cannot-swizzle-this-type
            :vtype vec-type)
  (assert (keywordp components) () 'swizzle-keyword :item components)
  (let* ((allowed (subseq "xyzw" 0 (first (v-dimensions vec-type))))
         (components (string-downcase components))
         (new-len (length components)))
    (assert (and (every λ(find _ allowed) components)
                 (>= new-len 1)
                 (<= new-len 4))
            () (error "Varjo: swizzle form invalid: ~a" components))
    components))

;;------------------------------------------------------------
