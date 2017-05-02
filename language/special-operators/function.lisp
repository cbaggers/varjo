(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; First class functions

;; {TODO} qualify the arg types to disambiguate from overloads.
;; {TODO} proper error
(v-defspecial function (func-name)
  :args-valid t
  :return
  (let ((func (find-form-binding-by-literal func-name env)))
    (etypecase func
      (v-regular-macro (error "Varjo: Although legal in CL, Varjo does not allow taking a reference to a macro function"))
      (external-function (%function-for-external-funcs func func-name env))
      (v-function (%function-for-regular-funcs func-name func env))
      (v-function-set (%function-for-func-sets func-name func env))
      (null (error 'could-not-find-function :name func-name)))))

(defun %function-for-func-sets (func-name-form func-set env)
  (let* ((functions (functions func-set))
         (len (length functions))
         (has-external (some λ(typep _ 'external-function) functions)))
    (cond
      ((and has-external (= len 1))
       (%function-for-external-funcs (first functions) func-name-form env))
      (has-external (error 'multiple-external-func-match :matches functions))
      (t (let* ((type (v-type-of func-set))
                (type-set (make-type-set type)))
           (when (or (some #'implicit-args functions)
                     (and (some #'captured-vars functions)))
             (error 'closures-not-supported :func func-name-form))
           (values
            (make-compiled :type-set type-set
                           :current-line nil
                           :used-types (list type)
                           :node-tree (ast-node! 'function (list func-name-form)
                                                 type-set nil nil)
                           :pure t)
            env))))))

;; {TODO} shouldnt this have a new environment?
(defun %function-for-external-funcs (func func-name-form env)
  (compile-external-func-returning-ref func func-name-form env))

(defun %function-for-regular-funcs (func-name-form func env)
  (let* ((flow-id (flow-id!))
         (type (set-flow-id (v-type-of func) flow-id))
         (type-set (make-type-set type)))
    (when (implicit-args func)
      (error 'closures-not-supported :func func-name-form))
    (values
     (make-compiled :type-set type-set
                    :current-line nil
                    :used-types (list type)
                    :node-tree (ast-node! 'function (list func-name-form)
                                          type-set nil nil)
                    :pure t)
     env)))

;;------------------------------------------------------------
