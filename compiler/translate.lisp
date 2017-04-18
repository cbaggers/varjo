(in-package :varjo)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(defgeneric translate (stage)
  (:method ((stage stage))
    (with-slots (stemcells-allowed) stage
      (flow-id-scope
        (let ((env (%make-base-environment
                    stage :stemcells-allowed stemcells-allowed)))
          (pipe-> (stage env)
            #'set-env-context
            #'process-primitive-type
            #'add-context-glsl-vars
            #'process-in-args
            #'process-uniforms
            #'compile-pass
            #'make-post-process-obj
            #'check-stemcells
            #'post-process-ast
            #'filter-used-items
            #'gen-in-arg-strings
            #'gen-in-decl-strings
            #'gen-out-var-strings
            #'process-output-primtive
            #'final-uniform-strings
            #'dedup-used-types
            #'final-string-compose
            #'package-as-final-result-object))))))

;;----------------------------------------------------------------------

(defun set-env-context (stage env)
  (setf (v-context env) (context stage))
  (values stage env))

;;----------------------------------------------------------------------

(defun add-context-glsl-vars (stage env)
  (values stage (add-glsl-vars env)))

;;----------------------------------------------------------------------

;; {TODO} Proper error
(defun process-in-args (stage env)
  "Populate in-args and create fake-structs where they are needed"
  (loop :for var :in (input-variables stage) :do
     ;; with-v-arg (name type qualifiers declared-glsl-name) var
     (let* ((glsl-name (glsl-name var))
            (type (v-type-of var))
            (name (name var)))
       (typecase type
         (v-struct (add-fake-struct var env))
         (v-ephemeral-type (error "Varjo: Cannot have in-args with ephemeral types:~%~a has type ~a"
                                  name type))
         (t (let* ((type (set-flow-id type (flow-id!)))
                   (type (if (and (stage-is stage :geometry)
                                  (typep type 'v-array))
                             (make-into-block-array
                              type *in-block-instance-name*)
                             type)))
              (%add-symbol-binding
               name (v-make-value type env :glsl-name glsl-name) env)
              (add-lisp-name name env glsl-name)
              (setf (v-in-args env)
                    (cons-end (make-instance 'input-variable
                                             :name name
                                             :glsl-name glsl-name
                                             :type type
                                             :qualifiers (qualifiers var))
                              (v-in-args env))))))))
  (values stage env))

;;----------------------------------------------------------------------

(defun process-uniforms (stage env)
  (let ((uniforms (uniform-variables stage)))
    (map nil
         λ(with-slots (name type qualifiers glsl-name) _
            (case-member qualifiers
              (:ubo (process-ubo-uniform name glsl-name type qualifiers env))
              (:fake (add-fake-struct stage env))
              (otherwise (process-regular-uniform name glsl-name type
                                                  qualifiers env))))
         uniforms)
    (values stage env)))

;; mutates env
(defun process-regular-uniform (name glsl-name type qualifiers env)
  (let* ((true-type (set-flow-id (v-true-type type) (flow-id!)))
         (glsl-name (or glsl-name (safe-glsl-name-string name))))
    (%add-symbol-binding
     name
     (v-make-value true-type env :glsl-name glsl-name :read-only t)
     env)
    (add-lisp-name name env glsl-name)
    (let ((type-with-flow (set-flow-id type (flow-ids true-type))))
      (push (list name type-with-flow qualifiers glsl-name) (v-uniforms env))))
  env)

;; mutates env
(defun process-ubo-uniform (name glsl-name type qualifiers env)
  (let* ((true-type (set-flow-id (v-true-type type) (flow-id!)))
         (glsl-name (or glsl-name (safe-glsl-name-string name))))
    (%add-symbol-binding
     name (v-make-value true-type env :glsl-name glsl-name
                                 :function-scope 0 :read-only t)
     env)
    (let ((type-with-flow (set-flow-id type (flow-ids true-type))))
      (push (list name type-with-flow qualifiers glsl-name) (v-uniforms env))))
  env)

;;----------------------------------------------------------------------

(defgeneric %process-primitive-type (stage primitive env)
  ;;
  (:method ((stage vertex-stage) primitive env)
    (declare (ignore stage env))
    primitive)

  (:method ((stage tesselation-control-stage)
            primitive env)
    (declare (ignore stage primitive env))
    (error "%process-primitive-type: IMPLEMENT ME!")
    nil)

  (:method ((stage tesselation-evaluation-stage)
            primitive env)
    (declare (ignore stage primitive env))
    (error "%process-primitive-type: IMPLEMENT ME!")
    nil)

  (:method ((stage geometry-stage) primitive env)
    (assert (typep primitive 'geometry-primitive) ()
            'invalid-primitive-for-geometry-stage
            :prim (type-of primitive))
    primitive)

  (:method ((stage fragment-stage) primitive env)
    (declare (ignore stage env))
    nil))

(defun process-primitive-type (stage env)
  (labels ((get-primitive (name)
             (let ((type (find name *draw-modes*)))
               (assert type () "Varjo: Could not find primitive named ~a" name)
               (make-instance (find-symbol (symbol-name name) :varjo)))))
    (let* ((prim (primitive stage))
           (prim (etypecase prim
                   (null nil)
                   (symbol (get-primitive prim))
                   (primitive prim))))
      (setf (primitive stage) (%process-primitive-type stage prim env)))
    (values stage env)))

;;----------------------------------------------------------------------

(defun compile-pass (stage env)
  (values (build-function :main () (lisp-code stage) nil env)
          stage
          env))

;;----------------------------------------------------------------------

(defgeneric establish-out-set-for-stage (stage main-func)
  ;;
  (:method ((stage geometry-stage) main-func)
    (assert (emptyp (return-set main-func)) ()
            'returns-in-geometry-stage :return-set (return-set main-func))
    (when (slot-boundp main-func 'emit-set)
      (or (slot-value main-func 'emit-set) #())))
  ;;
  (:method (stage main-func)
    (return-set main-func)))

(defun make-post-process-obj (main-func stage env)
  (make-instance
   'post-compile-process
   :stage stage
   :env env
   :used-external-functions (remove-duplicates (used-external-functions env))
   :all-functions (cons main-func (all-cached-compiled-functions env))
   :out-set (establish-out-set-for-stage stage main-func)
   :main-metadata (top-level-scoped-metadata main-func)))

;;----------------------------------------------------------------------

(defun check-stemcells (post-proc-obj)
  "find any stemcells in the result that that the same name and
   a different type. Then remove duplicates"
  (let ((stemcells
         (reduce #'append (mapcar #'stemcells (all-functions post-proc-obj)))))
    (mapcar
     (lambda (x)
       (with-slots (name (string string-name) type flow-id) x
         (declare (ignore string flow-id))
         (when (find-if (lambda (x)
                          (with-slots ((iname name) (itype type)) x
                            (and (equal name iname)
                                 (not (equal type itype)))))
                        stemcells)
           (error "Symbol ~a used with different implied types" name))))
     ;; {TODO} Proper error here
     stemcells)
    (setf (stemcells post-proc-obj)
          (remove-duplicates stemcells :test #'equal
                             :key (lambda (x)
                                    (slot-value x 'name))))
    post-proc-obj))

;;----------------------------------------------------------------------

(defun post-process-func-ast
    (func env flow-origin-map val-origin-map node-copy-map)
  ;; prime maps with args (env)
  ;; {TODO} need to prime in-args & structs/array elements
  (labels ((uniform-raw (val)
             (slot-value (first (ids (first (listify (flow-ids val)))))
                         'val)))
    (let ((env (get-base-env env)))
      (loop :for (name) :in (v-uniforms env) :do
         (let ((binding (get-symbol-binding name nil env)))
           ;; {TODO} proper errror
           (assert (not (typep binding 'v-symbol-macro)) ()
                   "Varjo: Compiler Bug: Unexpanded symbol-macro found in ast")
           (let ((key (uniform-raw binding))
                 (val (make-uniform-origin :name name)))
             (setf (gethash key flow-origin-map) val)
             (setf (gethash key val-origin-map) val))))))

  (labels ((post-process-node (node walk parent &key replace-args)
             ;; we want a new copy as we will be mutating it
             (let ((new (copy-ast-node
                         node
                         :parent (gethash parent node-copy-map))))
               ;; store the lookup tables with every node
               (setf (slot-value new 'flow-id-origins) flow-origin-map
                     (slot-value new 'val-origins) val-origin-map)
               (with-slots (args val-origin flow-id-origin kind) new
                 ;;    maintain the relationship between this copied
                 ;;    node and the original
                 (setf (gethash node node-copy-map) new
                       ;; walk the args. OR, if the caller pass it,
                       ;; walk the replacement args and store them instead
                       args (mapcar λ(funcall walk _ :parent node)
                                    (or replace-args (ast-args node)))
                       ;;
                       val-origin (val-origins new)
                       ;; - - - - - - - - - - - - - - - - - - - - - -
                       ;; flow-id-origins gets of DESTRUCTIVELY adds
                       ;; the origin of the flow-id/s for this node.
                       ;; - - - - - - - - - - - - - - - - - - - - - -
                       ;; {TODO} redesign this madness
                       ;; - - - - - - - - - - - - - - - - - - - - - -
                       flow-id-origin (flow-id-origins new))
                 ;;
                 (when (typep (ast-kind new) 'v-function)
                   (setf kind (name (ast-kind new)))))
               new))

           (walk-node (node walk &key parent)
             (cond
               ;; remove progns with one form
               ((and (ast-kindp node 'progn) (= (length (ast-args node)) 1))
                (funcall walk (first (ast-args node)) :parent parent))

               ;; splice progns into let's implicit progn
               ((ast-kindp node 'let)
                (let ((args (ast-args node)))
                  (post-process-node
                   node walk parent :replace-args
                   `(,(first args)
                      ,@(loop :for a :in (rest args)
                           :if (and (typep a 'ast-node)
                                    (ast-kindp a 'progn))
                           :append (ast-args a)
                           :else :collect a)))))

               ;; remove return nodes
               ((ast-kindp node 'return)
                (funcall walk (first (ast-args node)) :parent parent))

               (t (post-process-node node walk parent)))))

    (let ((ast (walk-ast #'walk-node (ast func) :include-parent t)))
      (setf (slot-value func 'ast) ast))))

(defun post-process-ast (post-proc-obj)
  (let ((flow-origin-map (make-hash-table))
        (val-origin-map (make-hash-table))
        (node-copy-map (make-hash-table :test #'eq)))
    (map nil λ(post-process-func-ast _ (env post-proc-obj)
                                     flow-origin-map
                                     val-origin-map
                                     node-copy-map)
         (all-functions post-proc-obj)))
  post-proc-obj)

;;----------------------------------------------------------------------

(defun find-used-user-structs (functions env)
  (declare (ignore env))
  (let* ((used-types (normalize-used-types
                      (reduce #'append (mapcar #'used-types functions))))
         (struct-types
          (remove nil
                  (loop :for type :in used-types
                     :if (or (typep type 'v-struct)
                             (and (typep type 'v-array)
                                  (typep (v-element-type type) 'v-struct)))
                     :collect type)))
         (result (order-structs-by-dependency struct-types)))
    result))

(defun filter-used-items (post-proc-obj)
  "This changes the code-object so that used-types only contains used
   'user' defined structs."
  (with-slots (env) post-proc-obj
    (setf (used-types post-proc-obj)
          (find-used-user-structs (all-functions post-proc-obj) env)))
  post-proc-obj)

;;----------------------------------------------------------------------

(defun calc-locations (types)
  ;;   "Takes a list of type objects and returns a list of positions
  ;; - usage example -
  ;; (let ((types (mapcar #'type-spec->type '(:mat4 :vec2 :float :mat2 :vec3))))
  ;;          (mapcar #'cons types (calc-positions types)))"
  (labels ((%calc-location (sizes type)
             (cons (+ (first sizes) (v-glsl-size type)) sizes)))
    (reverse (reduce #'%calc-location (butlast types) :initial-value '(0)))))


(defun gen-in-arg-strings (post-proc-obj)
  (with-slots (env stage) post-proc-obj
    (let* ((type-objs (mapcar #'v-type-of (v-in-args env)))
           (locations (if (member :vertex (v-context env))
                          (calc-locations type-objs)
                          (loop for i below (length type-objs) collect nil))))
      ;;
      (setf (in-args post-proc-obj)
            (loop :for var :in (v-in-args env)
               :for location :in locations
               :for type :in type-objs
               :collect
               (make-instance
                'input-variable
                :name (name var)
                :glsl-name (glsl-name var)
                :type (if (typep type 'v-block-array)
                          (block-array-to-regular-array type)
                          type)
                :qualifiers (qualifiers var)
                :glsl-decl (gen-in-var-string (or (glsl-name var) (name var))
                                              type
                                              (qualifiers var)
                                              location))))
      ;;
      (setf (input-variables post-proc-obj)
            (loop :for var :in (input-variables stage) :collect
               (or (find (name var) (in-args post-proc-obj) :key #'name)
                   var)))))
  post-proc-obj)

;;----------------------------------------------------------------------

(defun gen-in-decl-strings (post-proc-obj)
  (when (stage-is post-proc-obj :geometry)
    (setf (in-declarations post-proc-obj)
          (list (gen-geom-input-primitive-string (primitive post-proc-obj)))))
  post-proc-obj)

;;----------------------------------------------------------------------

(defun gen-out-var-strings (post-proc-obj)
  (with-slots (out-set env) post-proc-obj
    (let* ((out-types (map 'list #'v-type-of out-set))
           (locations (if (stage-is env :fragment)
                          (calc-locations out-types)
                          (loop for i below (length out-types) collect nil)))
           (stage-kind (extract-stage-type post-proc-obj)))
      (setf (out-vars post-proc-obj)
            (loop :for ret-val :across out-set
               :for i :from 0
               :for glsl-name := (if (typep ret-val 'external-return-val)
                                     (out-name ret-val)
                                     (nth-return-name i stage-kind))
               :for type := (v-type-of ret-val)
               :for qualifiers := (qualifiers ret-val)
               :for location :in locations
               :collect
               (make-instance
                'output-variable
                :name (make-symbol glsl-name)
                :glsl-name glsl-name
                :type type
                :qualifiers qualifiers
                :location location
                :glsl-decl (gen-out-var-string
                            glsl-name type qualifiers location))))
      post-proc-obj)))

;;----------------------------------------------------------------------

(defun process-output-primtive (post-proc-obj)
  (with-slots (main-metadata) post-proc-obj
    (cond
      ((stage-is post-proc-obj :geometry)
       (let* ((tl (find 'output-primitive main-metadata :key #'type-of)))
         (assert tl () "Varjo: The function used as a geometry stage must has a top level output-primitive declaration")
         (setf (out-declarations post-proc-obj)
               (list (gen-geom-output-primitive-string tl)))
         (setf (primitive-out post-proc-obj)
               (primitive-name-to-instance (slot-value tl 'kind)))))
      (t (setf (primitive-out post-proc-obj)
               (primitive (stage post-proc-obj))))))
  post-proc-obj)

;;----------------------------------------------------------------------

(defun final-uniform-strings (post-proc-obj)
  (with-slots (env) post-proc-obj
    (let* ((final-strings nil)
           (structs (used-types post-proc-obj))
           (uniforms (v-uniforms env))
           (implicit-uniforms nil))
      (loop :for (name type-obj qualifiers glsl-name) :in uniforms :do
         (let ((string-name (or glsl-name (safe-glsl-name-string name))))
           (push (make-instance
                  'uniform-variable
                  :name name
                  :qualifiers qualifiers
                  :glsl-name string-name
                  :type type-obj
                  :glsl-decl (cond
                               ((member :ubo qualifiers)
                                (write-ubo-block :uniform string-name
                                                 (v-slots type-obj)))
                               ((ephemeral-p type-obj) nil)
                               (t (gen-uniform-decl-string string-name type-obj
                                                           qualifiers))))
                 final-strings))
         (when (and (v-typep type-obj 'v-user-struct)
                    (not (find type-obj structs :key #'type->type-spec
                               :test #'v-type-eq)))
           (push type-obj structs)))
      (loop :for s :in (stemcells post-proc-obj) :do
         (with-slots (name string-name type cpu-side-transform) s
           (when (eq type :|unknown-type|) (error 'symbol-unidentified :sym name))
           (let ((type-obj (type-spec->type type)))
             (push (make-instance
                    'implicit-uniform-variable
                    :name name
                    :glsl-name string-name
                    :type type-obj
                    :cpu-side-transform cpu-side-transform
                    :glsl-decl (unless (ephemeral-p type-obj)
                                 (gen-uniform-decl-string
                                  (or string-name (error "stem cell without glsl-name"))
                                  type-obj
                                  nil)))
                   implicit-uniforms)

             (when (and (v-typep type-obj 'v-user-struct)
                        (not (find type-obj structs
                                   :key #'type->type-spec
                                   :test #'v-type-eq)))
               (push type-obj structs)))))
      ;;
      (setf (used-types post-proc-obj) structs)
      (setf (uniforms post-proc-obj) final-strings)
      (setf (stemcells post-proc-obj) implicit-uniforms)
      post-proc-obj)))

;;----------------------------------------------------------------------

(defun dedup-used-types (post-proc-obj)
  (with-slots (main-env) post-proc-obj
    (setf (used-types post-proc-obj)
          (remove-duplicates
           (mapcar #'v-signature (used-types post-proc-obj))
           :test #'equal)))
  post-proc-obj)

;;----------------------------------------------------------------------

(defun final-string-compose (post-proc-obj)
  (values (gen-shader-string post-proc-obj)
          post-proc-obj))

;;----------------------------------------------------------------------

(defun package-as-final-result-object (final-glsl-code post-proc-obj)
  (with-slots (env stage) post-proc-obj
    (let* ((context (process-context-for-result (v-context env))))
      (make-instance
       'varjo-compile-result
       :glsl-code final-glsl-code
       :stage-type (extract-stage-type post-proc-obj)
       :in-args (in-args post-proc-obj)
       :input-variables (input-variables post-proc-obj)
       :out-vars (out-vars post-proc-obj)
       :uniform-variables (uniforms post-proc-obj)
       :implicit-uniforms (stemcells post-proc-obj)
       :context context
       :stemcells-allowed (allows-stemcellsp env)
       :used-external-functions (used-external-functions post-proc-obj)
       :function-asts (mapcar #'ast (all-functions post-proc-obj))
       :lisp-code (lisp-code stage)
       :primitive-in (primitive (stage post-proc-obj))
       :primitive-out (primitive-out post-proc-obj)))))

(defun process-context-for-result (context)
  ;; {TODO} having to remove this is probably a bug
  (remove :main context))
