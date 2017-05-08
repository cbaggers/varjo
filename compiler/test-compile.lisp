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

;;----------------------------------------------------------------------

(defun funcall-catching (func &rest args)
  (handler-case (apply func args)
    (error (e) e)))

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

(defun %test-translate-raising (request-stage-func stage-kinds)
  (restart-case
      (let* ((stage-types (mapcar #'stage-kind-to-type
                                  (ensure-list stage-kinds)))
             (call-func (if (> (length stage-types) 1)
                            #'funcall-catching
                            #'funcall))
             (results (mapcar λ(funcall call-func
                                        #'translate
                                        (funcall request-stage-func _))
                              stage-types)))
        (labels ((errorp (x) (typep x 'error)))
          (if (every #'errorp results)
              (raise-test-translate-error results stage-kinds)
              (remove-if #'errorp results))))
    ;;
    ;; Restarts
    (retest-assuming-stage (replacement-stage-name)
      (assert (find replacement-stage-name *stage-names* :test #'string=) ()
              "Varjo: replacement stage name must be one of ~s~%Found ~s"
              *stage-names*)
      (%test-translate-raising request-stage-func replacement-stage-name))

    (retest-assuming-vertex-stage ()
      (%test-translate-raising request-stage-func :vertex))

    (retest-assuming-tessellation-control-stage ()
      (%test-translate-raising request-stage-func :tessellation-control))

    (retest-assuming-tessellation-evaluation-stage ()
      (%test-translate-raising request-stage-func :tessellation-evaluation))

    (retest-assuming-geometry-stage ()
      (%test-translate-raising request-stage-func :geometry))

    (retest-assuming-fragment-stage ()
      (%test-translate-raising request-stage-func :fragment))))

;;----------------------------------------------------------------------

(defun test-translate-stage (stage &optional (stages-kinds *stage-names*))
  (labels ((get-stage (stage-type)
             (copy-stage
              stage
              :stage-type stage-type
              :primitive-in (unless (primitive-in stage)
                              (largest-primitive-for-stage stage-type)))))
    (%test-translate-raising #'get-stage stages-kinds)))

(defun test-translate-function (name args body
                                &optional (stage-kinds *stage-names*)
                                  (allow-stemcells t))
  (let* ((name (or name (gensym "test-lambda"))))
    (labels ((get-stage (stage-type)
               (make-stage stage-type nil nil '(:450)
                           `(,@(gen-dummy-declarations-for-stage stage-type)
                               (labels ((,name ,args ,@body))
                                 ,(gen-dummy-call name args)
                                 ,(gen-dummy-out-for-stage-type stage-type)))
                           allow-stemcells
                           (largest-primitive-for-stage stage-type))))
      (%test-translate-raising #'get-stage stage-kinds))))

(defun test-translate-function-split-details
    (name in-args uniforms context body
     &optional (stage-kinds *stage-names*) (allow-stemcells t))
  (let* ((args in-args)
         (name (or name (gensym "test-lambda"))))
    (labels ((get-stage (stage-type)
               (make-stage stage-type nil uniforms context
                           `(,@(gen-dummy-declarations-for-stage stage-type)
                               (labels ((,name ,args ,@body))
                                 ,(gen-dummy-call name args)
                                 ,(gen-dummy-out-for-stage-type stage-type)))
                           allow-stemcells
                           (largest-primitive-for-stage stage-type))))
      (%test-translate-raising #'get-stage stage-kinds))))

;;----------------------------------------------------------------------
