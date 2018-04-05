(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; Regular Macros

(defmacro define-vari-macro (name lambda-list &body body)
  (vbind (func-code context) (gen-macro-function-code name lambda-list body)
    `(progn
       (add-global-form-binding
        (make-regular-macro ',name ,func-code ',context nil))
       ',name)))

(defmacro v-defmacro (name lambda-list &body body)
  `(define-vari-macro ,name ,lambda-list ,@body))

(defgeneric make-regular-macro (name macro-function context env)
  (:method (name macro-function context env)
    (make-instance 'v-regular-macro
                   :name name
                   :macro-function macro-function
                   :context context
                   :function-scope (if env
                                       (v-function-scope env)
                                       0))))

;;------------------------------------------------------------
;; Symbol Macros

(defgeneric make-symbol-macro (expansion-form function-scope env)
  (:method (expansion-form function-scope env)
    (make-instance 'v-symbol-macro
                   :expansion expansion-form
                   :function-scope (or function-scope (v-function-scope env)))))

;;------------------------------------------------------------
;; Compile Macros

(defmacro define-vari-compiler-macro (name lambda-list &body body)
  (labels ((namedp (name x)
             (when (symbolp x)
               (string= name x)))
           (maybe-nth (n x)
             (if (listp x) (nth n x) x)))
    (when (find-if λ(namedp :&uniforms _) lambda-list)
      (error 'uniform-in-cmacro :name name))
    (when (find-if λ(namedp :&shared _) lambda-list)
      (error 'shared-in-cmacro :name name))
    (when (find-if λ(namedp :&optional _) lambda-list)
      (error 'optional-in-cmacro :name name))
    (when (find-if λ(namedp :&key _) lambda-list)
      (error 'key-in-cmacro :func-name name))
    ;;
    (let ((llist (mapcar λ(if (listp _) (first _) _) lambda-list)))
      (vbind (func-code context) (gen-macro-function-code name llist body)
        (let* ((args (nth-value 1 (extract-arg-pair lambda-list :&whole)))
               (args (nth-value 1 (extract-arg-pair args :&environment)))
               (arg-names (mapcar λ(maybe-nth 0 _) args))
               (arg-types (mapcar λ(arg-form->type (maybe-nth 1 _)) args)))
          `(progn
             (add-global-compiler-macro
              (make-compiler-macro ',name ,func-code ',arg-names ',arg-types
                                   ',context))
             ',name))))))

(defmacro v-define-compiler-macro (name lambda-list &body body)
  `(define-vari-compiler-macro ,name ,lambda-list ,@body))

(defun make-compiler-macro (name macro-function arg-names arg-spec context)
  (make-instance 'v-compiler-macro
                 :name name
                 :args arg-names
                 :context context
                 :arg-spec arg-spec
                 :&rest-pos (&rest-pos arg-spec)
                 :macro-function macro-function))

(defun find-compiler-macro-for-func (func)
  (unless (v-special-functionp func)
    (let* ((name (name func))
           (func-spec (v-argument-spec func))
           (rest-pos (&rest-pos func-spec))
           (candidates (get-global-compiler-macro name))
           (candidates (if rest-pos
                           (remove-if-not λ(= (&rest-pos _) rest-pos)
                                          candidates)
                           candidates))
           (func-spec (remove-if #'&rest-p func-spec)))
      (when candidates
        (let* ((scored (mapcar λ(basic-arg-matchp _ func-spec nil
                                                  :allow-casting nil)
                               candidates))
               (trimmed (remove-if λ(or (null _) (> (score _) 0)) scored))
               (sorted (sort trimmed #'< :key #'secondary-score))
               (winner (first sorted)))
          (when winner
            (func winner)))))))

;;------------------------------------------------------------
;; Helpers

(defun extract-arg-pair (lambda-list key)
  (labels ((forgiving-name-equal (x y)
             (when (and (symbolp x) (symbolp y))
               (string= x y))))
    (let* ((key-pos (position key lambda-list :test #'forgiving-name-equal))
           (value (when key-pos
                    (first (subseq lambda-list (1+ key-pos)))))
           (cleaned (if key-pos
                        (append (subseq lambda-list 0 key-pos)
                                (subseq lambda-list (+ 2 key-pos)))
                        lambda-list)))
      (values value cleaned))))

(defun gen-macro-function-code (name lambda-list body)
  (alexandria:with-gensyms (form-var g-env result)
    (vbind (context lambda-list) (extract-arg-pair lambda-list :&context)
      (vbind (env-var lambda-list) (extract-arg-pair lambda-list :&environment)
        (let* ((whole-var (extract-arg-pair lambda-list :&whole))
               (whole-check (if whole-var `(not (equal ,whole-var ,result)) t))
               (whole-rebind (when whole-var
                               `((,whole-var (cons ',name ,whole-var))))))
          (let* ((env-var (or env-var g-env)))
            (vbind (body declarations) (extract-declares-and-doc-string body)
              (values
               `(lambda (,form-var ,env-var)
                  (declare (ignorable ,env-var))
                  (destructuring-bind ,lambda-list ,form-var
                    ,@declarations
                    (let* (,@whole-rebind
                           (,result (progn ,@body))
                           (same-form ,whole-check))
                      (values ,result same-form))))
               context))))))))
