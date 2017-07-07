(in-package :vari.cl)
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
      (top-level-lisp-function-decl (%function-for-top-level-lisp-function-decls func func-name env))
      (v-function (%function-for-regular-funcs func-name func env))
      (v-function-set (%function-for-func-sets func-name func env))
      (null (error 'could-not-find-function :name func-name)))))

(defun %function-for-func-sets (func-name-form func-set env)
  (let* ((functions (functions func-set))
         (top-level-count (count-if #'top-level-lisp-function-decl-p functions)))
    (cond
      ;; if there isnt a local function, top-level functions win
      ((and (= top-level-count 1)
            (not (find-if #'lisp-function-p functions)))
       (%function-for-top-level-lisp-function-decls (first functions) func-name-form env))
      ;;
      ;; If theres more than one top-level function then something is messed up
      ((> top-level-count 1)
       (error 'varjo.internals::multiple-top-level-lisp-function-decl-match
              :name func-name-form
              :matches (mapcar λ(typecase _
                                  (top-level-lisp-function-decl
                                   (format-top-level-lisp-function-decl-for-error _))
                                  (t _))
                               functions)))
      ;;
      ;; Otherwise there is a local or spec function so ditch the top-level
      ;; functions and carry on
      (t (let* ((functions (remove-if #'top-level-lisp-function-decl-p functions))
                (type (v-type-of (make-function-set functions)))
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
(defun %function-for-top-level-lisp-function-decls (func func-name-form env)
  (compile-top-level-lisp-function-decl-returning-ref func func-name-form env))

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
