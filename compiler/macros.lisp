(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; Regular Macros

(defmacro v-defmacro (name lambda-list &body body)
  (vbind (func-code context) (gen-macro-function-code name lambda-list body)
    `(progn
       (add-form-binding
        (make-regular-macro ',name ,func-code ',context *global-env*)
        *global-env*)
       ',name)))

(defmethod make-regular-macro (name macro-function context env)
  (make-instance 'v-regular-macro
                 :name name
                 :macro-function macro-function
                 :context context
                 :function-scope (if (eq env *global-env*)
                                     0
                                     (v-function-scope env))))

;;------------------------------------------------------------
;; Symbol Macros

(defmethod make-symbol-macro (expansion-form function-scope env)
  (make-instance 'v-symbol-macro
                 :expansion expansion-form
                 :function-scope (or function-scope (v-function-scope env))))

;;------------------------------------------------------------
;; Compile Macros

(defmacro v-define-compiler-macro (name lambda-list &body body)
  (labels ((namedp (name x)
             (when (symbolp x)
               (string= name x))))
    (when (find-if λ(namedp :&uniforms _) lambda-list)
      (error 'uniform-in-cmacro :name name))
    (when (find-if λ(namedp :&optional _) lambda-list)
      (error 'optional-in-cmacro :name name))
    (when (find-if λ(namedp :&rest _) lambda-list)
      (error 'rest-in-cmacro :func-name name))
    (when (find-if λ(namedp :&key _) lambda-list)
      (error 'key-in-cmacro :func-name name))
    ;;
    (let* ((whole-pos (position-if λ(namedp _ :&whole) lambda-list))
           (llist (mapcar λ(if (listp _) (first _) _) lambda-list))
           (cleaned (if whole-pos
                        (append (subseq lambda-list 0 whole-pos)
                                (subseq lambda-list (+ 2 whole-pos)))
                        lambda-list))
           (arg-types (mapcar λ(type-spec->type (second _)) cleaned)))
      (vbind (func-code context) (gen-macro-function-code name llist body)
        `(progn
           (add-compiler-macro
            (make-compiler-macro ',name ,func-code ',arg-types ',context)
            *global-env*)
           ',name)))))

(defun make-compiler-macro (name macro-function arg-spec context)
  (make-instance 'v-compiler-macro
                 :name name
                 :context context
                 :arg-spec arg-spec
                 :macro-function macro-function))

(defun find-compiler-macro-for-func (func env)
  (unless (v-special-functionp func)
    (let* ((name (name func))
           (func-spec (v-argument-spec func))
           (candidates (get-compiler-macro name env)))
      (when candidates
        (let* ((scored (mapcar λ(basic-arg-matchp _ func-spec nil env
                                                  :allow-casting nil)
                               candidates))
               (trimmed (remove-if λ(or (null _) (> (score _) 0)) scored))
               (sorted (sort trimmed #'< :key #'secondary-score))
               (winner (first sorted))
               (macro (func winner)))
          macro)))))

;;------------------------------------------------------------
;; Helpers

(defun extract-arg-pair (lambda-list key)
  (let* ((key-pos (position key lambda-list :test #'symbol-name-equal))
         (value (when key-pos
                  (first (subseq lambda-list (1+ key-pos)))))
         (cleaned (if key-pos
                      (append (subseq lambda-list 0 key-pos)
                              (subseq lambda-list (+ 2 key-pos)))
                      lambda-list)))
    (values value cleaned)))

(defun gen-macro-function-code (name lambda-list body)
  (alexandria:with-gensyms (form-var g-env result)
    (vbind (context lambda-list) (extract-arg-pair lambda-list :&context)
      (vbind (env-var lambda-list) (extract-arg-pair lambda-list :&environment)
        (let* ((whole-var (extract-arg-pair lambda-list :&whole))
               (whole-check (if whole-var `(not (equal ,whole-var ,result)) t))
               (whole-rebind (when whole-var
                               `((,whole-var (cons ',name ,whole-var))))))
          (let* ((env-var (or env-var g-env)))
            (values
             `(lambda (,form-var ,env-var)
                (declare (ignorable ,env-var))
                (destructuring-bind ,lambda-list ,form-var
                  (let* (,@whole-rebind
                         (,result (progn ,@body))
                         (same-form ,whole-check))
                    (values ,result same-form))))
             context)))))))
