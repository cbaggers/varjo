(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Swizzle

(v-defmacro s~ (&rest args) `(swizzle ,@args))
(v-defspecial swizzle (vec-form components)
  :args-valid t
  :return
  (let ((vec-obj (compile-form vec-form env)))
    (unless (v-typep (v-type-of vec-obj) 'v-vector)
      (let ((type (v-true-type (v-type-of vec-obj))))
        (error 'cannot-swizzle-this-type :vtype (type->type-spec type)
               :is-struct (typep type 'v-struct))))
    (let* ((allowed (subseq (list #\x #\y #\z #\w) 0
                            (first (v-dimensions (primary-type vec-obj)))))
           (comp-string (if (keywordp components)
                            (string-downcase (symbol-name components))
                            (error 'swizzle-keyword :item components)))
           (new-len (length comp-string))
           (vec-type (primary-type vec-obj))
           (element-type (v-element-type vec-type)))
      (if (and (>= new-len 1) (<= new-len 4)
               (v-typep vec-type 'v-vector)
               (loop :for c :across comp-string
                  :always (find c allowed)))
          (let* ((flow-id (flow-id!))
                 (r-type (set-flow-id (if (= new-len 1)
                                          element-type
                                          (vec-of element-type new-len))
                                      flow-id)))
            (values
             (copy-compiled
              vec-obj
              :type r-type
              :current-line (gen-swizzle-string vec-obj comp-string)
              :node-tree (ast-node! 'swizzle
                                    `(,(node-tree vec-obj) ,components)
                                    r-type env env)
              :multi-vals nil
              :place-tree nil)
             env))
          (error "swizzle form invalid")))))

;;------------------------------------------------------------
