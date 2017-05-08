(in-package :varjo)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defun largest-primitive-for-stage (type)
  (let ((prim
         (case type
           (vertex-stage :triangles-adjacency)
           (tessellation-control-stage '(:patch 32))
           (tessellation-evaluation-stage '(:patch 32))
           (geometry-stage :triangles-adjacency)
           (fragment-stage nil)
           (otherwise (error "Varjo: Invalid stage kind name ~a" type)))))
    (when prim
      (primitive-name-to-instance prim))))

;;----------------------------------------------------------------------

(defun %test-translate-for-kind (stage kind)
  (assert (find kind *stage-names*))
  (with-slots (input-variables
               uniform-variables context lisp-code
               stemcells-allowed previous-stage primitive-in)
      stage
    (let* ((type (stage-kind-to-type kind))
           (stage (make-instance
                   type
                   :input-variables input-variables
                   :uniform-variables uniform-variables
                   :context context
                   :lisp-code lisp-code
                   :stemcells-allowed stemcells-allowed
                   :previous-stage previous-stage
                   :primitive-in (unless primitive-in
                                   (largest-primitive-for-stage type)))))
      (translate stage))))

(defun test-translate (stage &key (stages *stage-names*))
  (loop :for kind :in stages :collect
     (handler-case (%test-translate-for-kind stage kind)
       (error (e) e))))

;;----------------------------------------------------------------------

(defun %test-translate-raising (stage stages error-on-any-p &optional nth-pass)
  (restart-case
      (let* ((stages (ensure-list stages))
             (results (if nth-pass
                          (%test-translate-for-kind stage (first stages))
                          (test-translate stage :stages stages))))
        (labels ((errorp (x) (typep x 'error)))
          (if (or (and error-on-any-p (find-if #'errorp results))
                  (every #'errorp results))
              (raise-test-translate-error results stages)
              (remove-if #'errorp results))))
    ;;
    (retest-assuming-stage (replacement-stage-name)
      (assert (find replacement-stage-name *stage-names* :test #'string=) ()
              "Varjo: replacement stage name must be one of ~s~%Found ~s"
              *stage-names*)
      (%test-translate-raising stage replacement-stage-name error-on-any-p t))
    (retest-assuming-vertex-stage ()
      (%test-translate-raising stage :vertex error-on-any-p t))
    (retest-assuming-tessellation-control-stage ()
      (%test-translate-raising stage :tessellation-control error-on-any-p t))
    (retest-assuming-tessellation-evaluation-stage ()
      (%test-translate-raising stage :tessellation-evaluation error-on-any-p t))
    (retest-assuming-geometry-stage ()
      (%test-translate-raising stage :geometry error-on-any-p t))
    (retest-assuming-fragment-stage ()
      (%test-translate-raising stage :fragment error-on-any-p t))))

(defun test-translate-raising
    (stage &key (stages *stage-names*) (error-on-any-p nil))
  (%test-translate-raising stage stages error-on-any-p))

(defun raise-test-translate-error (errors stage-types)
  (let ((groups (group-by (compose #'princ-to-string #'first)
                          (remove-if-not
                           (lambda (x) (typep (first x) 'condition))
                           (mapcar #'list errors stage-types)))))
    (if (= 1 (length groups))
        (error (caaar groups))
        (let ((grouped (mapcar (lambda (grp)
                                 (cons (mapcar #'second grp)
                                       (princ-to-string (caar grp))))
                               groups)))
          (error 'test-translate-failed :grouped-errors grouped)))))

;;----------------------------------------------------------------------

(defun gen-dummy-out-for-stage-type (stage-type)
  (case stage-type
    (geometry-stage '(values))
    (otherwise '(v! 1 2 3 4))))

(defun gen-dummy-call (name args)
  `(,name ,@(mapcar λ`(%synthesize ,(second _)) args)))

(defun gen-dummy-declarations-for-stage (stage-type)
  (case stage-type
    (tessellation-control-stage
     `((declare (varjo::output-patch :vertices 32))))
    (geometry-stage
     `((declare (output-primitive :kind :triangle-strip :max-vertices 3))))))

(defun %test-translate-function (kind name args body)
  (assert (find kind *stage-names*))
  (let* ((name (or name (gensym "test-lambda")))
         (stage-type (stage-kind-to-type kind))
         (stage (make-stage kind nil nil '(:450)
                            `(,@(gen-dummy-declarations-for-stage stage-type)
                              (labels ((,name ,args ,@body))
                                ,(gen-dummy-call name args)
                                ,(gen-dummy-out-for-stage-type stage-type)))
                            nil (largest-primitive-for-stage stage-type))))
    (translate stage)))

(defun test-func-translate (name args body &key (stages *stage-names*))
  (loop :for kind :in stages :collect
     (handler-case (%test-translate-function kind name args body)
       (error (e) e))))

(v-defspecial %synthesize (type)
  :args-valid t
  :return
  (let* ((type (type-spec->type type (flow-id!)))
         (str (string-downcase
               (format nil "<dummy ~a>" (type->type-spec type)))))
    (if (typep type 'v-function-type)
        (let* ((func (make-dummy-function-from-type type))
               (ftype (set-flow-id (v-type-of func) (flow-id!))))
          (compile-form `(glsl-expr ,str ,ftype) env))
        (compile-form `(glsl-expr ,str ,type) env))))

;; (%test-translate-function
;;  :tessellation-evaluation 'foo '((x :int) (func (function (:vec2) :vec2)))
;;  '((funcall func (v! 1 2)) (+ x 10)))

;;----------------------------------------------------------------------
