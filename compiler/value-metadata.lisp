(in-package :varjo)
(in-readtable fn:fn-reader)

;;-------------------------------------------------------------------------

(defvar *metadata-styles* '(:value :scope))
(defvar *metadata-kinds* nil)

;;-------------------------------------------------------------------------

;; {TODO} proper error
(defmacro def-metadata-kind (name (&key conc-name (binds-to :value))
                             &body slot-names)
  (check-metadata-slots slot-names)
  (let ((type (ecase binds-to
                (:value 'standard-value-metadata)
                (:scope 'standard-scope-metadata))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,name (,type)
           ,(mapcar λ`(,_ :initform nil :initarg ,(kwd _))
                    slot-names)))
       ,@(mapcar λ`(defgeneric ,(if conc-name (symb conc-name _) _) (metadata))
                 slot-names)
       ,@(mapcar λ`(defmethod ,(if conc-name (symb conc-name _) _)
                       ((metadata-collection list))
                     (let ((data (cdr (assoc ',name metadata-collection))))
                       (when data
                         (slot-value data ',_))))
                 slot-names)
       ,@(mapcar λ`(defmethod ,(if conc-name (symb conc-name _) _)
                       ((metadata ,name))
                     (slot-value metadata ',_))
                 slot-names)
       ,(when slot-names (gen-meta-init-check name slot-names))
       (defmethod print-object ((obj ,name) stream)
         (print-unreadable-object (obj stream :type t :identity t)
           (with-slots ,slot-names obj
             (format stream ,(format nil "~{:~a ~~a~^ ~}" slot-names)
                     ,@slot-names))))
       (pushnew ',name *metadata-kinds*))))

(defun gen-meta-init-check (name slot-names)
  (let ((init-args (loop :for name :in slot-names :collect
                      `(,name nil ,(gensym (symbol-name name))))))
    `(defmethod initialize-instance :after
       ((md ,name) &rest all-args &key ,@init-args)
       (declare (ignore ,@slot-names))
       (assert (and ,@(mapcar #'third init-args)) ()
               'v-metadata-missing-args
               :name ',name
               :required ',slot-names
               :provided all-args
               :missing (remove nil (list ,@(loop :for (n nil c) :in init-args
                                               :collect `(unless ,c ',n))))))))


;; {TODO} proper error
(defun check-metadata-slots (slots)
  (assert (every #'symbolp slots)))

(defun known-metadata-kind-p (name)
  (not (null (member name *metadata-kinds*))))

;;-------------------------------------------------------------------------
;; Combining Metadata
;;
;; If you throw an error from this method it will be caught, extra details will
;; be added and then it will be rethrown.

(defgeneric combine-metadata (meta-a meta-b)
  ;;
  (:method ((meta-a standard-value-metadata) (meta-b standard-value-metadata))
    nil)
  ;;
  (:method ((meta-a standard-value-metadata) (meta-b null))
    meta-a)
  ;;
  (:method ((meta-a null) (meta-b standard-value-metadata))
    meta-b)
  ;;
  ;; {TODO} proper error
  (:method ((meta-a null) (meta-b null))
    (error "Varjo: Compiler Bug: Tried to combine metadata with two null objects")))

;;-------------------------------------------------------------------------
;; Find similar declaration names

(defun find-alternative-declaration-kinds (decl-name)
  (find-similarly-named-symbol decl-name *metadata-kinds*))

;;-------------------------------------------------------------------------
;; Extracting Declarations

;; Valid in these forms
;;
;; defgeneric                 do-external-symbols   prog
;; define-compiler-macro      do-symbols            prog*
;; define-method-combination  dolist                restart-case
;; define-setf-expander       dotimes               symbol-macrolet
;; defmacro                   flet                  with-accessors
;; defmethod                  handler-case          with-hash-table-iterator
;; defsetf                    labels                with-input-from-string
;; deftype                    let                   with-open-file
;; defun                      let*                  with-open-stream
;; destructuring-bind         locally               with-output-to-string
;; do                         macrolet              with-package-iterator
;; do*                        multiple-value-bind   with-slots
;; do-all-symbols             pprint-logical-block

(defvar +cl-standard-declaration-ids+
  '(dynamic-extent  ignore     optimize
    ftype           inline     special
    ignorable       notinline  type))

;;-------------------------------------------------------------------------
;; Extracting Declarations

(defun extract-declares-and-doc-string (body full-form)
  (labels ((declp (x) (and (listp x) (eq (first x) 'declare))))
    (if (= (length body) 1)
        (values body nil nil)
        (let (doc-string declarations)
          (loop :for form :in body :for i :from 0
             :while
             (cond
               ;;
               ((declp form) (push form declarations))
               ;;
               ((stringp form) (if doc-string
                                   (error 'duplicate-varjo-doc-string
                                          :dup form :form full-form)
                                   (setf doc-string form))))
             :finally (return (values (subseq body i)
                                      declarations
                                      doc-string)))))))

(defun extract-declares (body)
  (labels ((declp (x) (and (listp x) (eq (first x) 'declare))))
    (if (= (length body) 1)
        (values body nil)
        (let (declarations)
          (loop :for form :in body :for i :from 0
             :while (declp form) :do (push form declarations)
             :finally (return (values (subseq body i)
                                      (reverse declarations))))))))

;;-------------------------------------------------------------------------
;; Compiling Declarations

(defun compile-declares (declaration-specifiers env)
  (add-declarations-to-env declaration-specifiers env)
  ;; we return nil so env-> will be satisfied. We are mutating the environment
  ;; so there is not environment to return
  nil)

(defun add-declarations-to-env (declaration-specifiers env)
  (when declaration-specifiers
    (let ((decl-forms (loop :for decl :in declaration-specifiers :append
                         (progn
                           (assert (eq (first decl) 'declare))
                           (rest decl)))))
      (loop :for decl-form :in decl-forms :do
         (dbind (decl-name . decl-rest) decl-form
           (assert (subtypep decl-name 'standard-metadata))
           (assert (not (find decl-name +cl-standard-declaration-ids+))
                   () 'v-unsupported-cl-declaration :decl decl-form)
           (assert (known-metadata-kind-p decl-name) (decl-name)
                   'v-unrecognized-declaration :decl decl-form)
           (cond
             ((subtypep decl-name 'standard-value-metadata)
              (let* ((has-args (listp (first decl-rest)))
                     (decl-args (when has-args (first decl-rest)))
                     (decl-targets (if has-args
                                       (rest decl-rest)
                                       decl-rest)))
                (compile-bound-decl decl-name decl-args decl-targets env)))
             ((subtypep decl-name 'standard-scope-metadata)
              (compile-scope-decl decl-name decl-rest env))
             (t (error "Varjo Bug: Invalid metadata kind ~a" decl-name)))))))
  env)

(defun compile-bound-decl (decl-name decl-args decl-targets env)
  (assert (subtypep decl-name 'standard-value-metadata))
  (assert (every #'symbolp decl-targets) (decl-targets)
          'v-only-supporting-declares-on-vars
          :targets (remove-if #'symbolp decl-targets))
  (loop :for target :in decl-targets :do
     (let ((binding (get-symbol-binding target t env)))
       (etypecase binding
         (v-value nil)
         (null (error 'v-declare-on-nil-binding :target target))
         (v-symbol-macro (error 'v-declare-on-symbol-macro
                                :target target)))
       (let ((id (flow-ids (v-type-of binding))))
         (setf (metadata-for-flow-id id env)
               (apply #'make-instance decl-name decl-args))))))

(defun compile-scope-decl (decl-name decl-args env)
  (assert (subtypep decl-name 'standard-scope-metadata))
  (setf (metadata-for-scope env)
        (apply #'make-instance decl-name decl-args)))

;;-------------------------------------------------------------------------

(defgeneric meta-kinds-to-infer (varjo-type)
  (:method (varjo-type)
    (declare (ignore varjo-type))
    nil))

(defgeneric infer-meta-by-type (varjo-type metadata-kind env)
  (:method (varjo-type metadata-kind env)
    (declare (ignore varjo-type metadata-kind env))
    nil))

(defun infer-meta (code-obj env)
  (assert (typep code-obj 'compiled))
  (assert (typep env 'environment))
  (unless (emptyp (type-set code-obj))
    (let* ((flow-ids (map 'list #'flow-ids (type-set code-obj)))
           (ext-env (make-instance 'extended-environment :env env))
           (type (primary-type code-obj)))
      (loop :for flow-id :in flow-ids :do
         (when (singular-flow-id-p flow-id)
           (loop :for kind :in (meta-kinds-to-infer type) :do
              (unless (metadata-for-flow-id kind flow-id env)
                (let ((meta-args (multiple-value-list
                                  (infer-meta-by-type type kind ext-env))))
                  (unless (or (null meta-args)
                              (equal meta-args '(nil)))
                    (let ((meta (apply #'make-instance kind meta-args)))
                      (setf (metadata-for-flow-id flow-id env) meta))))))))))
  code-obj)

(defmacro def-metadata-infer (varjo-type metadata-kind env-var &body body)
  (assert (symbolp varjo-type))
  (assert (symbolp metadata-kind))
  (assert (subtypep metadata-kind 'standard-value-metadata))
  (assert (type-specp varjo-type))
  ;; The conversion below this comment may look redundent but its important
  ;; it is a lazy way of getting the true type of varjo-type. This is required
  ;; as we then specialize infer-meta-by-type on it. If this is left out you
  ;; won't always get and error but for certain types your metadata will fail
  ;; to infer.
  ;;                        ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  (let ((varjo-type (type->type-spec (type-spec->type varjo-type))))
    (with-gensyms (type kind)
      `(defmethod infer-meta-by-type ((,type ,varjo-type)
                                      (,kind (eql ',metadata-kind))
                                      ,env-var)
         ,@body))))
