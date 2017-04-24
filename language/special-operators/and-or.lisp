(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; And

(v-defspecial and (&rest forms)
  :args-valid t
  :return
  (let* ((objs (mapcar (lambda (x) (compile-form x env)) forms))
         (flow-id (flow-id!))
         (type-set (make-type-set (type-spec->type :bool flow-id))))
    (unless (loop for o in objs always (v-primary-type-eq o (first objs)))
      (error "all forms of an 'AND' form must resolve to the same type"))
    (if (v-typep (primary-type (first objs)) (type-spec->type :bool))
        (values (merge-compiled objs
                                :type-set type-set
                                :current-line (gen-bool-and-string objs)
                                :node-tree (ast-node! 'and
                                                      (mapcar #'node-tree objs)
                                                      type-set
                                                      env env))
                env) ;; pretty sure this env is wrong, what if side effects in
        ;;              forms?
        (values (last1 objs) env))))

;;------------------------------------------------------------
;; Or

;; pretty sure env is wrong in 'or and 'and, what if there are side effects in
;; forms?
;; In fact function calls in general should at least propagate the flow ids
;; down the arg compiles..could even the env be passed? may just work
(v-defspecial or (&rest forms)
  :args-valid t
  :return
  (let* ((objs (mapcar (lambda (x) (compile-form x env)) forms))
         (flow-id (flow-id!))
         (type-set (make-type-set (type-spec->type :bool flow-id))))
    (unless (loop for o in objs always (v-primary-type-eq o (first objs)))
      (error "all forms of an 'OR' form must resolve to the same type"))
    (if (v-typep (primary-type (first objs)) (type-spec->type :bool))
        (values (merge-compiled
                 objs
                 :type-set type-set
                 :current-line (gen-bool-or-string objs)
                 :node-tree (ast-node! 'or (mapcar #'node-tree objs)
                                       type-set env env))
                env)
        (values (first objs) env))))

;;------------------------------------------------------------
