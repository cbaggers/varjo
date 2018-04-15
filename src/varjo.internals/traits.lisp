(in-package :varjo.internals)

;;------------------------------------------------------------
;;
;; - adding functions needs to check if the name is bound to a trait function
;;   if so then fail
;; - defining a function that takes or return traits is fine. They will need to
;;   be monomorphised though. They won't appear in the final glsl as their
;;   call-count will always be zero.
;; - For now dont allow parametized type names to implement traits (except
;;   arrays which will be hacked in the implementation) HMMMMMM

(defclass trait-function (v-function)
  ((trait :initarg :trait)))
(defun trait-function-p (x) (typep x 'trait-function))
;;------------------------------------------------------------

(defvar *traits*
  (make-hash-table))

(defclass trait-spec ()
  ((type-vars :initarg :type-vars)
   (function-signatures :initarg :function-signatures)))

(defmacro define-vari-trait (name (&rest type-vars) &body func-signatures)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-v-type-class ,name (v-trait) ())
     (register-trait
      ',name
      (make-instance
       'trait-spec
       :type-vars ',type-vars
       :function-signatures (parse-trait-specs
                             ',name ',type-vars ',func-signatures)))))

(defun remove-redundent-trait-functions (trait-name new-trait)
  (let ((old-trait (gethash trait-name *traits*)))
    (when old-trait
      (let ((old-funcs
             (mapcar #'first (slot-value old-trait 'function-signatures)))
            (new-funcs
             (mapcar #'second (slot-value new-trait 'function-signatures))))
        (loop :for name :in (set-difference old-funcs new-funcs) :do
           (delete-external-function name :all))))))

(defun check-for-trait-function-collision (spec)
  ;; - adding a trait has to check for existing functions with that name
  ;;   if they arent functions of that same trait then fail
  (let ((funcs (slot-value spec 'function-signatures)))
    (loop :for (func-name . nil) :in funcs :do
       (let* ((bindings (get-global-form-binding func-name))
              (functions (when bindings
                           (remove-if #'trait-function-p
                                      (functions bindings)))))
         (assert (null functions) ()
                 "Varjo: Trait function cannot be an overload of any existing function:~%Found:~{~%~a~}"
                 (mapcar (lambda (fn)
                           (cons func-name
                                 (rest (type->type-spec (v-type-of fn)))))
                         functions))))))

(defun add-trait-functions (trait-name spec)
  (check-for-trait-function-collision spec)
  (remove-redundent-trait-functions trait-name spec)
  (let ((top (type-spec->type t))
        (top-set (make-type-set (type-spec->type t)))
        (funcs (slot-value spec 'function-signatures)))
    (flet ((finalize-type (x)
             (etypecase x
               (v-type x)
               (symbol top))))
      (loop
         :for (func-name . arg-types) :in funcs
         :for final-arg-types := (mapcar #'finalize-type arg-types)
         :do (add-global-form-binding
              (make-trait-function-obj (type-spec->type trait-name)
                                       func-name
                                       final-arg-types
                                       top-set))))))

(defun register-trait (trait-name spec)
  (add-trait-functions trait-name spec)
  (setf (gethash trait-name *traits*) spec))

(defun get-trait (trait-name &key (errorp t))
  (or (gethash trait-name *traits*)
      (when errorp
        (error "Varjo：No trait named ~a is currently known" trait-name))))

(defun parse-trait-specs (trait-name type-vars function-signatures)
  (let ((name-map
         (loop :for (name type) :in (cons (list :self trait-name) type-vars)
            :collect (cons name (if (string= type "_")
                                    name
                                    (type-spec->type type))))))
    (flet ((as-type (name)
             (or (assocr name name-map)
                 (type-spec->type name))))
      (loop :for (func-name . func-spec) :in function-signatures
         :do
         (assert (find :self func-spec) ()
                 "Trait function specs must have at least one :self argument")
         :collect
         (cons func-name (mapcar #'as-type func-spec))))))

;;------------------------------------------------------------

(defvar *trait-implementations*
  (make-hash-table))

(defclass impl-spec ()
  ((function-signatures :initarg :function-signatures)))

(defun check-impl-spec (trait-name trait-args type-name spec)
  (assert (vtype-existsp type-name))
  (assert (vtype-existsp trait-name))
  (let* ((trait (get-trait trait-name))
         (required-funcs (slot-value trait 'function-signatures))
         (provided-funcs (slot-value spec 'function-signatures))
         (type-var-requirements (make-hash-table :test #'eq)))
    (assert (= (length required-funcs) (length provided-funcs)))
    ;; We need to check that the types used are consistent in the implementation in cases
    ;; where type-vars were used.
    ;; Relies on Varjo producing new types for each call to type-spec->type
    (flet ((get-var-req (type)
             (gethash type type-var-requirements))
           (set-var-req (req ours)
             (setf (gethash req type-var-requirements)
                   ours)
             t)
           (complete-req-type (type)
             (etypecase type
               (v-type type)
               (symbol (or (assocr type trait-args :test #'string=)
                           (error "Varjo: Could not find the '~a' trait arg required by ~a"
                                  type trait-name))))))
      (loop :for (name func) :in provided-funcs :do
         (assert
          (loop
             :for (nil . req-types) :in required-funcs
             :thereis
             (let ((complete-req-types (mapcar #'complete-req-type req-types)))
               (and (func-args-satisfy-p func complete-req-types)
                    (loop :for req-type :in complete-req-types
                       :for req-spec :in req-types
                       :for type :in (v-argument-spec func)
                       :for i :from 0
                       :always
                       (let ((strict (get-var-req req-type)))
                         (when (symbolp req-spec)
                           (assert (v-type-eq type req-type) ()
                                   "Varjo: Trait argument states that argument ~a of ~a should be ~a"
                                   i name (type->type-spec req-type)))
                         (if strict
                             (v-type-eq type strict)
                             (set-var-req req-type type)))))))
          () "Varjo: No func in~%~a~%matched~%~a" required-funcs func)))))

(defun register-trait-implementation (trait-name trait-args type-name spec)
  (let ((trait-table (or (gethash type-name *trait-implementations*)
                         (setf (gethash type-name *trait-implementations*)
                               (make-hash-table)))))
    (unless (gethash trait-name trait-table)
      (setf (gethash trait-name trait-table) t))
    (unwind-protect
         (check-impl-spec trait-name trait-args type-name spec)
      (setf (gethash trait-name trait-table) nil))
    (setf (gethash trait-name trait-table)
          spec)))

(defun get-trait-implementation (trait type &key (errorp t))
  (check-type trait v-type)
  (check-type type v-type)
  (let* ((trait-name (slot-value trait 'type-name))
         (type-name (slot-value type 'type-name))
         (impl-table (gethash type-name *trait-implementations*)))
    (if impl-table
        (or (gethash trait-name impl-table)
            (when errorp
              (error "Varjo: Type ~a does not satisfy the trait named ~a"
                     type-name trait-name)))
        (when errorp
          (error "Varjo: Type ~a currently satisfies no traits.~%Looking for: ~a"
                 type-name trait-name)))))

(defun parse-impl-specs (function-signatures)
  (loop :for (trait-func-name impl-func) :in function-signatures
     :for binding := (find-global-form-binding-by-literal impl-func t)
     :collect
     (list trait-func-name binding)))

(defmacro define-vari-trait-implementation
    (impl-type-name (trait-name &rest trait-args &key &allow-other-keys)
     &body implementations &key &allow-other-keys)
  (let ((grouped
         (loop :for (k v) :on implementations :by #'cddr :collect
            (list k v)))
        (trait-args
         (loop :for (k v) :on trait-args :by #'cddr :collect
            (cons k v))))
    `(progn
       (register-trait-implementation
        ',trait-name ',trait-args ',impl-type-name
        (make-instance 'impl-spec :function-signatures
                       (parse-impl-specs ',grouped)))
       ',impl-type-name)))

;;------------------------------------------------------------

#+nil
(progn
  (v-deftype blerp () :ivec2)
  (v-def-glsl-template-fun blerp (x) "blerp(~a)" (:int) blerp)

  (v-def-glsl-template-fun boop (x) "boop(~a)" (blerp) blerp)
  (v-def-glsl-template-fun checker (x y) "check(~a, ~a)" (blerp blerp) :bool)
  (v-def-glsl-template-fun splat (x y) "splat(~a,~a)" (blerp :int) :int)

  (v-deftype narp () :ivec3)
  (v-def-glsl-template-fun narp (x) "narp(~a)" (:int) narp)

  (v-def-glsl-template-fun nboop (x) "nboop(~a)" (narp) narp)
  (v-def-glsl-template-fun checker (x y) "check(~a, ~a)" (narp narp) :bool)
  (v-def-glsl-template-fun splat (x y) "splat(~a,~a)" (narp :int) :int)

  (define-vari-trait trat ()
    (booper :self)
    (checkerer :self :self)
    (splatter :self t))

  (define-vari-trait-implementation blerp (trat)
    :booper (boop blerp)
    :checkerer (checker blerp blerp)
    :splatter (splat blerp :int))

  (define-vari-trait-implementation narp (trat)
    :booper (nboop narp)
    :checkerer (checker narp narp)
    :splatter (splat narp :int)))

;;------------------------------------------------------------
