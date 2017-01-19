(in-package :varjo)
(in-readtable fn:fn-reader)

;;-------------------------------------------------------------------------

(defvar *metadata-kinds* nil)

;;-------------------------------------------------------------------------

(defclass standard-value-metadata () ())

;;-------------------------------------------------------------------------

;; {TODO} proper error
(defmacro def-metadata-kind (name (&key conc-name) &body slot-names)
  (check-metadata-slots slot-names)
  `(progn
     (defclass ,name (standard-value-metadata)
       ,(mapcar λ`(,_ :initform nil :initarg ,(kwd _))
                slot-names))
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
     (defmethod print-object ((obj ,name) stream)
       (print-unreadable-object (obj stream :type t :identity t)
         (with-slots ,slot-names obj
           (format stream ,(format nil "~{:~a ~~a~^ ~}" slot-names)
                   ,@slot-names))))
     (push ',name *metadata-kinds*)))

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

(defmethod combine-metadata ((meta-a standard-value-metadata)
                             (meta-b standard-value-metadata))
  (values nil nil))

;; {TODO} proper error
(defmethod combine-metadata ((meta-a standard-value-metadata)
                             (meta-b null))
  (error "Varjo: Compiler Bug: The second argument to #'combine-metadata should
never be null"))

;; {TODO} proper error
(defmethod combine-metadata ((meta-a null)
                             (meta-b null))
  (error "Varjo: Compiler Bug: Tried to combine metadata with two null objects"))

;;-------------------------------------------------------------------------
;; Testing shiz

(def-metadata-kind space-meta ()
  uniform-name)

(defmethod combine-metadata ((meta-a space-meta)
                             (meta-b space-meta))
  (let ((u-a (uniform-name meta-a))
        (u-b (uniform-name meta-b)))
    (if (eq u-a u-b)
        u-a
        (error "Space Analysis Failed: Could not establish at compile time which
space was returned between:
~a
and
~a" u-a u-b))))

(def-v-type-class v-space (v-type) ())

(v-defmacro in (space &body body)
  `(space-boundary-convert
    (let ((*current-space* ,space))
      ,@body)))

(defmacro in (space &body body)
  (declare (ignore space body))
  (error "the 'in' macro can only be used inside shaders"))

(v-defspecial space-boundary-convert (form)
  :args-valid t
  :return
  (vbind (form-obj post-form-env) (compile-form form env)
    (if (v-typep (code-type form-obj) 'v-svec)
        (let* ((outer-space (get-var '*current-space* env)))
          (if outer-space
              (let* ((inner-space (get-space-from-svec form-obj post-form-env))
                     (outer-name (get-uniform-name-from-space outer-space env))
                     (inner-name (get-uniform-name-from-space inner-space env)))
                (break "sup ~a ~a ~a"
                       form-obj
                       outer-name
                       inner-name)
                (values form-obj post-form-env))
              (values form-obj post-form-env))))))

(defun get-uniform-name-from-space (space env)
  (let ((space-id (flow-ids space)))
    (or (first (find space-id (v-uniforms env) :test #'id=
                     :key λ(flow-ids (second _))))
        (error "Varjo: No uniform var found for space ~a" space))))

;;-------------------------------------------------------------------------
;; More testing

(def-metadata-kind spatial-meta ()
  in-space)

(defmethod combine-metadata ((meta-a standard-value-metadata)
                             (meta-b standard-value-metadata))
  (let ((space-a (in-space meta-a))
        (space-b (in-space meta-b)))
    (if (eq space-a space-b)
        space-a
        (error "Space Analysis Failed: Could not establish at compile time which
space the resulting svec was in between:
~a
and
~a" space-a space-b))))

(defmethod combine-metadata ((meta-a null)
                             (meta-b standard-value-metadata))
  (values nil nil))

(defun get-space-from-svec (svec-code-obj env)
  (in-space (metadata-for-flow-id (flow-ids svec-code-obj) env)))

(def-v-type-class v-svec (v-type)
  ((glsl-string :initform "vec4" :reader v-glsl-string)))

(v-defspecial sv! ((x :float) (y :float) (z :float) (w :float))
  :return
  (let ((space (get-var '*current-space* env)))
    (if space
        (let* ((space-type (v-type space))
               (flow-id (flow-id!))
               (type (type-spec->type 'v-svec flow-id)))
          (assert (v-typep space-type 'v-space))
          (setf (metadata-for-flow-id flow-id env)
                (make-instance 'spatial-meta :in-space space-type))
          (values
           (merge-obs (list x y z w)
                      :type type
                      :current-line (format nil "vec4(~a, ~a, ~a, ~a)"
                                            (current-line x)
                                            (current-line y)
                                            (current-line z)
                                            (current-line w))
                      :to-block (append (to-block x)
                                        (to-block y)
                                        (to-block z)
                                        (to-block w))
                      :node-tree (ast-node!
                                  'sv!
                                  (list x y z w)
                                  type
                                  env
                                  env))
           env))
        (compile-form `(v! ,x ,y ,z ,w) env))))
