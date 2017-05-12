(in-package :varjo)
(in-readtable :fn.reader)

(v-defspecial slot-value (form slot-name)
  :args-valid t
  :return
  (vbind (v e) (compile-form form env)
    (let ((type (primary-type v)))
      (assert (typep type 'v-struct) () 'slot-value-on-non-struct
              :type type :slot-name slot-name)
      ;;
      ;; NAH, this bit wont work. Instead we need to add another field to
      ;; the v-struct type to hold the transform strings. Then we can query
      ;; that here
      ;;
      (let ((slot (find slot-name (v-slots type) :key #'first)))
        (assert slot () 'slot-not-found
                :type type :slot-name slot-name)
        (dbind (name slot-type accessor slot-transform-string) slot
          (declare (ignore name accessor))
          (let* ((type-set (make-type-set (set-flow-id slot-type (flow-id!))))
                 (ast (ast-node! 'slot-value (list (node-tree v) slot-name)
                                 type-set env env))
                 (cline (format nil slot-transform-string (current-line v))))
            (values
             (copy-compiled v :current-line cline :type-set type-set
                            :node-tree ast)
             e)))))))

(v-defmacro with-slots (slots form &body body)
  (let* ((name (gensym "with"))
         (bindings (mapcar λ(dbind (mname &optional sname) (ensure-list _)
                              (let ((sname (or sname mname)))
                                `(,mname (slot-value ,name ,sname))))
                           slots)))
    `(let ((,name ,form))
       (symbol-macrolet ,bindings ,@body))))

(v-defmacro with-accessors (bindings form &body body)
  (let* ((name (gensym "with"))
         (bindings (mapcar λ(dbind (accessor-symbol-name accessor-name)
                                (ensure-list _)
                              `(,accessor-symbol-name (,accessor-name ,name)))
                           bindings)))
    `(let ((,name ,form))
       (symbol-macrolet ,bindings ,@body))))

;; Below is proof to myself that with-accessors is dumb in CL too
;;
;; (defclass blep () ((blip :initarg :blip :accessor blipper)))
;;
;; (flet ((blipper (x) (error "dumb ~a" x)))
;;   (with-accessors ((b blipper)) tmp0
;;     b))
