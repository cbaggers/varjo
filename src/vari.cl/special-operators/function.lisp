(in-package :vari.cl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; First class functions

;; {TODO} qualify the arg types to disambiguate from overloads.
;; {TODO} proper error
(v-defspecial function (func-name)
  :args-valid t
  :return
  (if (and (listp func-name) (and (eq (first func-name) 'lambda)))
      (compile-form func-name env)
      (let ((func (find-form-binding-by-literal func-name env)))
        (etypecase func
          (v-regular-macro (error "Varjo: Although legal in CL, Varjo does not allow taking a reference to a macro function"))
          (external-function (%function-for-external-funcs func func-name env))
          (v-function (%function-for-regular-funcs func-name func env))
          (v-function-set (%function-for-func-sets func-name func env))
          (null (error 'could-not-find-function :name func-name))))))

(defun %function-for-func-sets (func-name-form func-set env)
  (let* ((functions (functions func-set))
         (external-count (count-if #'external-function-p functions)))
    (cond
      ;; if there isnt a local function, external functions win
      ((and (= external-count 1)
            (not (find-if #'user-function-p functions)))
       (%function-for-external-funcs (first functions) func-name-form env))
      ;;
      ;; If theres more than one external then something is messed up
      ((> external-count 1)
       (error 'varjo.internals::multiple-external-func-match
              :name func-name-form
              :matches (mapcar λ(typecase _
                                  (external-function
                                   (format-external-func-for-error _))
                                  (t _))
                               functions)))
      ;;
      ;; Otherwise there is a local or spec function so ditch the externals
      ;; and carry on
      (t (let* ((functions (remove-if #'external-function-p functions))
                (type (v-type-of (make-function-set functions)))
                (type-set (make-type-set type))
                (funcs-with-implicit-args (remove-if-not #'implicit-args functions))
                (funcs-with-captured-args (remove-if-not #'captured-vars functions)))
           (when (or funcs-with-implicit-args funcs-with-captured-args)
             (let ((details (extract-details-from-problematic-closures
                             (append funcs-with-implicit-args funcs-with-captured-args))))
               (error 'varjo.internals::closures-not-supported
                      :func func-name-form
                      :details details)))
           (values
            (make-compiled :type-set type-set
                           :current-line nil
                           :used-types nil
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
      (let ((details (extract-details-from-problematic-closures (list func))))
        (error 'varjo.internals::closures-not-supported
               :func func-name-form
               :details details)))
    (values
     (make-compiled :type-set type-set
                    :current-line nil
                    :used-types nil
                    :pure t)
     env)))

;;------------------------------------------------------------
