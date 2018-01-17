(in-package :varjo.tests)

(defparameter *failures* nil)

(defun fuzz-many ()
  (loop :for i :below 10000 :do
     (when (= 0 (mod i 50))
       (print i))
     (handler-case (fuzz-vert)
       (error (e)
         (print "found failure")
         (push e *failures*)))))

(defun gen-in-args ()
  nil)

(defun gen-uniforms ()
  nil)

(defvar *rough-desired-depth*)
(defvar *rough-depth*)
(defvar *available-types*)
(defvar *available-funcs*)

(defun fuzz-vert (&optional (rough-desired-depth 10))
  (let* ((version :410-core)
         (*rough-desired-depth* rough-desired-depth)
         (*rough-depth* 0)
         (*available-types* nil)
         (*available-funcs* nil)
         (stage (gen-vert-stage version))
         (compiled (translate stage)))
    (assert (typep compiled 'compiled-stage) ()
            "(typep compiled 'compiled-stage)~%~a~%~a" (glsl-code compiled) (ast->code compiled))
    (assert (varjo.tests::ast-stabalizes-p compiled) ()
            "(varjo.tests::ast-stabalizes-p compiled)~%~a~%~a" (glsl-code compiled) (ast->code compiled))
    (assert (not (varjo.tests::glsl-contains-invalid compiled)) ()
            "(not (varjo.tests::glsl-contains-invalid compiled))~%~a~%~a" (glsl-code compiled) (ast->code compiled))
    (assert (not (varjo.tests::glsl-contains-nil compiled)) ()
            "(not (varjo.tests::glsl-contains-nil compiled))~%~a~%~a" (glsl-code compiled) (ast->code compiled))
    (assert (varjo.tests::glsl-compiles-p compiled) ()
            "(varjo.tests::glsl-compiles-p compiled)~%~a~%~a" (glsl-code compiled) (ast->code compiled))
    (values (glsl-code compiled)
            (ast->code compiled))))

(defun gen-vert-stage (&optional (version :450-core))
  (let* ((args (gen-in-args))
         (uniforms (gen-uniforms)))
    (make-stage :vertex args uniforms (list version)
                (gen-progn-body (type-spec->type :vec4) t))))

(defun gen-progn-body (required-type must-be-value)
  (let ((required (and required-type (not (eq required-type t)))))
    (append (loop :for i :from 0 :below (+ (if required 0 1) (random 5))
               :collect `(gen-form nil nil))
            (when (or required must-be-value)
              `((gen-form ,required-type ,must-be-value))))))

(v-defspecial gen-form (required-type must-be-value)
  :args-valid t
  :return
  (let ((*rough-depth* (incf *rough-depth*)))
    (if (> *rough-depth* *rough-desired-depth*)
        (compile-form `(gen-value ,required-type ,must-be-value) env)
        (compile-form (rand-case
                        (2 `(gen-function-call ,required-type ,must-be-value))
                        ;; (1 `(gen-funcall ,required-type ,must-be-value))
                        (1 `(gen-value ,required-type ,must-be-value))
                        (1 `(gen-progn ,required-type ,must-be-value))
                        (1 `(gen-let ,required-type ,must-be-value))
                        (1 `(gen-if ,required-type ,must-be-value))
                        (1 `(gen-flet ,required-type ,must-be-value))
                        (1 `(gen-labels ,required-type ,must-be-value))
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

(defun pick-function (required-type available-types available-funcs
                      must-be-value env)
  (assert (or (null required-type) (is-v-type-p required-type)))
  (labels ((good-type-p (x)
             (and (is-v-type-p x)
                  (find x available-types :test #'v-typep)))
           (good-func-p (func)
             (let ((args (v-argument-spec func))
                   (ret-spec (v-return-spec func)))
               (and (valid-for-contextp func env)
                    (listp args)
                    (not (find '&rest args :test #'tag=))
                    (vectorp ret-spec)
                    (every #'good-type-p args)
                    (every #'is-v-type-p ret-spec)
                    (if must-be-value
                        (and (not (v-voidp ret-spec))
                             (not (v-returned-p ret-spec))
                             (not (v-discarded-p ret-spec))))
                    (if required-type
                        (v-typep (primary-type ret-spec)
                                 required-type)
                        t)))))
    (let* ((all-bindings available-funcs)
           (applicable-bindings
            (loop :for func :in all-bindings
               :when (good-func-p func)
               :collect func)))
      (when applicable-bindings
        (random-elt applicable-bindings)))))

(defun available-funcs (env)
  (let ((base-env (get-base-env env)))
    (loop :for (name . set) :in (v-form-bindings base-env) :append
       (loop :for binding :in set
          :when (typep binding 'v-function)
          :collect binding))))

(defun vars-of-type (type env)
  (loop :for name :in (all-symbol-binding-names env)
     :when (typep (v-type-of (get-symbol-binding name t env))
                  type)
     :collect name))

(v-defspecial gen-function-call (required-type must-be-value)
  :args-valid t
  :return
  (let ((*available-types* (or *available-types*
                               (available-types env)))
        (*available-funcs* (or *available-funcs*
                               (available-funcs env)))
        (function (pick-function required-type
                                 *available-types*
                                 *available-funcs*
                                 must-be-value env)))
    (compile-form
     (if function
         (cons (name function)
               (loop :for type :in (v-argument-spec function)
                  :collect `(gen-form ,type ,must-be-value)))
         `(gen-value ,required-type ,must-be-value))
     env)))

(v-defspecial gen-funcall (required-type must-be-value)
  :args-valid t
  :return
  nil)

(defparameter *generatable-primitives*
  (mapcar #'type-spec->type
          '(:bool :int :uint :float :double :vec2 :vec3 :vec4)))

(v-defspecial gen-value (required-type must-be-value)
  :args-valid t
  :return
  (compile-form
   (if (null required-type)
       (rand-case
         (1 `(gen-literal nil ,must-be-value))
         (1 `(gen-var nil ,must-be-value)))
       `(gen-var ,required-type ,must-be-value))
   env))


(defun available-types (env)
  (loop :for name :in (all-symbol-binding-names env) :collect
     (v-type-of (get-symbol-binding name t env))))

(v-defspecial gen-var (required-type must-be-value)
  :args-valid t
  :return
  (let* ((names
          (all-symbol-binding-names env))
         (applicable
          (loop :for name :in names
             :for binding := (get-symbol-binding name t env)
             :when (or (null required-type)
                       (v-typep (v-type-of binding) required-type))
             :collect name)))
    (compile-form
     (if applicable
         (random-elt applicable)
         `(gen-literal ,required-type ,must-be-value))
     env)))

(v-defspecial gen-literal (required-type must-be-value)
  :args-valid t
  :return
  (let ((required-type (or required-type
                           (random-elt *generatable-primitives*))))
    (compile-form
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
       (:vec4  `(v! ,(random 10f0) ,(random 10f0) ,(random 10f0) ,(random 10f0))))
     env)))

(v-defspecial gen-progn (required-type must-be-value)
  :args-valid t
  :return
  (compile-form
   `(progn
      ,@(gen-progn-body required-type must-be-value))
   env))


(v-defspecial gen-let (required-type must-be-value)
  :args-valid t
  :return
  (let ((*available-types* nil))
    (compile-form
     `(let ,(loop :for i :below (random 4) :collect
               `(,(gen-var-name) (gen-form nil t)))
        ,@(gen-progn-body required-type must-be-value))
     env)))

(v-defspecial gen-if (required-type must-be-value)
  :args-valid t
  :return
  (let* ((*available-types* (or *available-types*
                                (available-types env)
                                *generatable-primitives*))
         (required-type
          (or required-type
              (when must-be-value
                (random-elt *available-types*)))))
    (compile-form
     `(if (= 1 2)
          (gen-form ,required-type ,must-be-value)
          (gen-form ,required-type ,must-be-value))
     env)))

(v-defspecial gen-flet (required-type must-be-value)
  :args-valid t
  :return
  (let ((*available-types* nil)
        (*available-funcs* nil))
    (compile-form
     `(flet ,(loop :for i :below (random 4) :collect
                `(,(gen-func-name) ,(gen-func-args)
                   ,@(gen-progn-body nil t)))
        ,@(gen-progn-body required-type must-be-value))
     env)))

(v-defspecial gen-labels (required-type must-be-value)
  :args-valid t
  :return
  (let ((*available-types* nil)
        (*available-funcs* nil))
    (compile-form
     `(flet ,(loop :for i :below (random 4) :collect
                `(,(gen-func-name) ,(gen-func-args)
                   ,@(gen-progn-body nil t)))
        ,@(gen-progn-body required-type must-be-value))
     env)))

(defun gen-func-args ()
  (loop :for i :below 6 :collect
     (list (gen-var-name) (type->type-spec (random-elt *generatable-primitives*)))))

(defvar *var-names*
  '(bize boxy bozo buzz chez cozy czar dozy faze fizz flux fozy friz
    fuji futz fuze fuzz hadj hajj hazy jabs jack jagg jake jamb jams
    jape jauk jaup java jazz jeep jeez jefe jehu jerk jeux jibb jibe
    jibs jiff jimp jink jinx jive jivy jobs jock john joke joky jouk
    jowl jows juba jube juco juga jugs juju juke juku jump junk jupe
    jury koji lazy maze mazy meze mojo mozo phiz pixy poxy prez puja
    putz qoph quag quay quey quip quiz razz waxy whiz))

(defvar *name-id* 0)

(defun gen-var-name (&optional except-these-names)
  (loop :for name := (elt *var-names* (mod (incf *name-id*) (length *var-names*)))
     :when (not (member name except-these-names))
     :return name))

(defvar *func-name-id* 0)

(defvar *func-names*
  '(xebec xenia xenic xenon xeric xerox xerus xylan xylem xylol xylyl
    xysti xysts yabby yacht yacks yaffs yager yagis yahoo yaird yamen
    yamun yangs yanks yapok yapon yards yarer yarns yauds yauld yaups
    yawed yawey yawls yawns yawps yclad yeahs yeans yearn years yeast
    yecch yechs yechy yeggs yelks yells yelps yenta yente yerba yerks
    yeses yetis yetts yeuks yeuky yield yikes yills yince yipes yirds
    yirrs yirth ylems yobbo yocks yodel yodhs yodle yogas yogee yoghs
    yogic yogin yogis yoked yokel yokes yolks yolky yomim yonic yonis
    yores young yourn yours youse youth yowed yowes yowie yowls yoyos
    yuans yucas yucca yucch yucks yucky yugas yukky yulan yules yummy
    yupon yuppy yurta yurts zaire zamia zanza zappy zarfs zaxes zayin
    zazen zeals zebec zebra zebus zeins zerks zeros zests zesty zetas
    zibet zilch zills zincs zincy zineb zines zings zingy zinky zippy
    ziram zitis zizit zlote zloty zoeae zoeal zoeas zombi zonae zonal
    zoned zoner zones zonks zooey zooid zooks zooms zoons zooty zoril
    zoris zouks zowie zuzim zymes))

(defun gen-func-name (&optional except-these-names)
  (loop :for name := (elt *func-names* (mod (incf *func-name-id*)
                                            (length *func-names*)))
     :when (not (member name except-these-names))
     :return name))
