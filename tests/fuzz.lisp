(in-package :varjo.tests.fuzz)

(defun gen-in-args ()
  nil)

(defun gen-uniforms ()
  nil)

(defvar *rough-desired-depth*)
(defvar *rough-depth*)

(defun fuzz-vert (&optional (rough-desired-depth 10))
  (let* ((version :450)
         (*rough-desired-depth* rough-desired-depth)
         (*rough-depth* 0)
         (stage (gen-vert-stage version))
         (compiled (translate stage)))
    (print (glsl-code compiled))
    `(progn ,@(ast->code compiled))))

(defun gen-vert-stage (&optional (version :450))
  (let* ((args (gen-in-args))
         (uniforms (gen-uniforms)))
    (make-stage :vertex args uniforms (list version)
                (gen-progn-body (type-spec->type :vec4)))))

(defun gen-progn-body (&optional required-type)
  (let ((required (and required-type (not (eq required-type t)))))
    (append (loop :for i :from 0 :below (+ (if required 0 1) (random 5))
               :collect `(gen-form nil))
            (when required
              `((gen-form ,required-type))))))

(v-defspecial gen-form (required-type)
  :args-valid t
  :return
  (let ((*rough-depth* (incf *rough-depth*)))
    (if (> *rough-depth* *rough-desired-depth*)
        (compile-form (gen-value required-type env)
                      env)
        (compile-form (rand-case
                        (2 (gen-function-call required-type env))
                        ;; (1 (gen-funcall required-type env))
                        (1 (gen-value required-type env))
                        (1 (gen-progn required-type env))
                        (1 (gen-let required-type env))
                        (1 (gen-if required-type env))
                        ;; (1 (gen-flet required-type env))
                        ;; (1 (gen-labels required-type env))
                        )
                      env))))

(defmacro g-typecase (form &body cases)
  (let ((gform (gensym "form")))
    `(let* ((,gform ,form)
            (,gform (typecase ,gform
                      (symbol (type-spec->type ,gform))
                      (list (type-spec->type ,gform))
                      (v-type ,gform)
                      (otherwise (v-type-of ,gform)))))
       (cond
         ,@(loop :for (test . body) :in cases :collect
              (if (eq test t)
                  `(t ,@body)
                  `((v-type-eq ,gform (type-spec->type ,test)) ,@body)))
         ,@(unless (find t cases :key #'first)
             `((t (error "g-typecase: No match for ~a in ~a"
                         ,gform ',(mapcar #'first cases)))))))))

(defun tag= (a b)
  (and (symbolp a)
       (symbolp b)
       (string= a b)))

(defun is-v-type-p (x)
  (typep x 'v-type))

(defun pick-function (required-type env)
  (assert (or (null required-type) (is-v-type-p required-type)))
  (let ((available-types (available-types env)))
    (labels ((good-type-p (x)
               (and (is-v-type-p x)
                    (find x available-types :test #'v-typep)))
             (good-func-p (func)
             (let ((args (varjo.internals::v-argument-spec func))
                   (ret-spec (varjo.internals::v-return-spec func)))
               (and (listp args)
                    (not (find '&rest args :test #'tag=))
                    (vectorp ret-spec)
                    (every #'good-type-p ret-spec)
                    (if required-type
                        (v-typep (varjo.internals::primary-type ret-spec)
                                 required-type)
                        t)))))
      (let* ((base-env (varjo.internals::get-base-env env))
             (all-bindings (loop :for (name . set) :in (varjo.internals::v-form-bindings base-env) :append
                              (loop :for binding :in set
                                 :when (typep binding 'varjo.internals::v-function)
                                 :collect binding)))
             (applicable-bindings
              (loop :for func :in all-bindings
                 :when (good-func-p func)
                 :collect func)))
        (when applicable-bindings
          (random-elt applicable-bindings))))))

(defun gen-function-call (required-type env)
  (let ((function (pick-function required-type env)))
    (if function
        (cons (name function)
              (loop :for type :in (varjo.internals::v-argument-spec function)
                 :collect `(gen-form ,type)))
        (gen-value required-type env))))

(defun gen-funcall (required-type env)
  (declare (ignore required-type env))
  nil)

(defparameter *generatable-primitives*
  (mapcar #'type-spec->type
          '(:bool :int :uint :float :double :vec2 :vec3 :vec4)))

(defun gen-value (required-type env)
  (if (null required-type)
      (rand-case
        (1 (gen-literal nil env))
        (1 (or (gen-var nil env)
               (gen-literal nil env))))
      (or (gen-var nil env)
          (gen-literal nil env))))

(defun available-types (env)
  (loop :for name :in (all-symbol-binding-names env) :collect
     (v-type-of (varjo.internals::get-symbol-binding name t env))))

(defun gen-var (required-type env)
  (let* ((names
          (all-symbol-binding-names env))
         (applicable
          (loop :for name :in names
             :for binding := (varjo.internals::get-symbol-binding name t env)
             :when (or (null required-type)
                       (v-typep (v-type-of binding) required-type))
             :collect name)))
    (when applicable
      (random-elt applicable))))

(defun gen-literal (required-type env)
  (declare (ignore env))
  (let ((required-type (or required-type
                           (random-elt *generatable-primitives*))))
    (g-typecase required-type
      (:bool (random-elt '(t nil)))
      (:int (- (random 20000) 10000))
      (:uint  (random 20000))
      (:float (- (random 20000f0) 10000f0))
      (:double (float (- (random 20000d0) 10000d0) 1d0))
      (:mat2 `(m! ,(random 10f0) ,(random 10f0)
                  ,(random 10f0) ,(random 10f0)))
      (:mat3  `(m! ,(random 10f0) ,(random 10f0) ,(random 10f0)
                   ,(random 10f0) ,(random 10f0) ,(random 10f0)
                   ,(random 10f0) ,(random 10f0) ,(random 10f0)))
      (:mat4  `(m! ,(random 10f0) ,(random 10f0) ,(random 10f0) ,(random 10f0)
                   ,(random 10f0) ,(random 10f0) ,(random 10f0) ,(random 10f0)
                   ,(random 10f0) ,(random 10f0) ,(random 10f0) ,(random 10f0)
                   ,(random 10f0) ,(random 10f0) ,(random 10f0) ,(random 10f0)))
      (:vec2  `(v! ,(random 10f0) ,(random 10f0)))
      (:vec3  `(v! ,(random 10f0) ,(random 10f0) ,(random 10f0)))
      (:vec4  `(v! ,(random 10f0) ,(random 10f0) ,(random 10f0) ,(random 10f0))))))

(defun gen-progn (required-type env)
  (declare (ignore env))
  `(progn
     ,@(gen-progn-body required-type)))

(defun gen-let (required-type env)
  (declare (ignore env))
  `(let ,(loop :for i :below (random 4) :collect
            `(,(gen-var-name) (gen-form nil)))
     ,@(gen-progn-body required-type)))

(defun gen-flet (required-type env)
  (declare (ignore required-type env))
  nil)

(defun gen-labels (required-type env)
  (declare (ignore required-type env))
  nil)

(defun gen-if (required-type env)
  (declare (ignore env))
  `(if (= 1 2)
       (gen-form ,required-type)
       (gen-form ,required-type)))

(defvar *var-names*
  '(bize boxy bozo buzz chez cozy czar dozy faze fizz flux fozy friz
    fuji futz fuze fuzz hadj hajj hazy jabs jack jagg jake jamb jams
    jape jauk jaup java jazz jeep jeez jefe jehu jerk jeux jibb jibe
    jibs jiff jimp jink jinx jive jivy jobs jock john joke joky jouk
    jowl jows juba jube juco juga jugs juju juke juku jump junk jupe
    jury koji lazy maze mazy meze mojo mozo phiz pixy poxy prez puja
    putz qoph quag quay quey quip quiz razz waxy whiz yutz zany zaps
    zarf zebu zeks zeps zerk zinc zonk zouk zyme))

(defvar *name-id* 0)

(defun gen-var-name (&optional except-these-names)
  (loop :for name := (elt *var-names* (mod (incf *name-id*) (length *var-names*)))
     :when (not (member name except-these-names))
     :return name))
