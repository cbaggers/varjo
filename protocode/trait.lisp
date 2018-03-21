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

(defclass trait-function (v-function) ())
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
    (loop :for (func-name . arg-types) :in funcs :do
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
  (let ((funcs (slot-value spec 'function-signatures))
        (top (make-type-set (type-spec->type t))))
    (loop :for (func-name . arg-types) :in funcs :do
       (add-global-form-binding
        (make-trait-function-obj func-name arg-types top)))))

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
            :collect (cons name (type-spec->type type)))))
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

(defun check-impl-spec (trait-name type-name spec)
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
             t))
      (loop :for (name func) :in provided-funcs :do
         (assert
          (loop :for (req-name . req-types) :in required-funcs :thereis
             (and (func-args-satisfy-p func req-types)
                  (loop :for req-type :in req-types
                     :for type :in (v-argument-spec func)
                     :always
                     (let ((strict (get-var-req req-type)))
                       (if strict
                           (v-type-eq type strict)
                           (set-var-req req-type type))))))
          () "Varjo: No func in~%~a~%matched~%~a" required-funcs func)))))

(defun register-trait-implementation (trait-name type-name spec)
  (let ((trait-table (or (gethash type-name *trait-implementations*)
                         (setf (gethash type-name *trait-implementations*)
                               (make-hash-table)))))
    (unless (gethash trait-name trait-table)
      (setf (gethash trait-name trait-table) t))
    (unwind-protect
         (check-impl-spec trait-name type-name spec)
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

(defmacro define-implementation
    (impl-type-name (trait-name &key &allow-other-keys)
     &body implementations &key &allow-other-keys)
  (let ((grouped
         (loop :for (k v) :on implementations :by #'cddr :collect
            (list k v))))
    `(progn
       (register-trait-implementation
        ',trait-name ',impl-type-name
        (make-instance 'impl-spec :function-signatures
                       (parse-impl-specs ',grouped)))
       ',impl-type-name)))

;;------------------------------------------------------------

(v-deftype blerp () :ivec2)
(v-def-glsl-template-fun boop (x) "boop(~a~)" (blerp) blerp)
(v-def-glsl-template-fun checker (x y) "(~a?~a)" (blerp blerp) :bool)
(v-def-glsl-template-fun splat (x y) "~a=~a" (blerp :int) :int)

#+nil
(define-vari-trait trat ()
  (booper :self)
  (checkerer :self :self)
  (splatter :self t))

#+nil
(define-implementation blerp (trat)
  :booper (boop blerp)
  :checkerer (checker blerp blerp)
  :splatter (splat blerp :int))

#+nil
(v-defun reduce ((fn function) (seq iterable))
  (let ((result)
        (limit (limit seq)))
    (for (state (create-iterator-state seq))
         (iterator-limit-check state limit)
         (setq state (next-iterator-state state))
         (let ((elem (element-for-state seq state)))
           (funcall fn elem)))))

;;------------------------------------------------------------
