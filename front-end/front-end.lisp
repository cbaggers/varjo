;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

;;----------------------------------------------------------------------

(defmacro defshader (name args &body body)
  (declare (ignore name))
  `(translate ',args '(progn ,@body)))

(defmacro defpipline (name args &body stages)
  (declare (ignore name))
  `(format nil "~{~a~%~}"(mapcar #'glsl-code (rolling-translate ',args ',stages))))

;;----------------------------------------------------------------------

(defun translate (args body)
  (let ((env (make-instance 'environment)))
    (pipe-> (args body env)
      #'split-input-into-env
      #'process-context
      #'process-in-args
      #'process-uniforms
      #'wrap-in-main-function
      #'add-context-glsl-vars
      (stabilizedp #'macroexpand-pass
                   #'compiler-macroexpand-pass)
      #'compile-pass
      #'filter-used-items
      #'gen-in-arg-strings
      #'gen-out-var-strings
      #'final-uniform-strings
      #'dedup-strings
      #'final-string-compose
      #'code-obj->result-object)))

(defun stabilizedp (last-pass one-before-that)
  (equal (first last-pass) (first one-before-that)))

(defun rolling-translate (args stages)
  (let* ((uni-pos (symbol-name-position '&uniform args))
         (context-pos (symbol-name-position '&context args))
         (in-vars (subseq args 0 (or uni-pos context-pos)))
         (uniforms (when uni-pos (subseq args (1+ uni-pos) context-pos)))
         (context (when context-pos (subseq args (1+ context-pos))))
         (wip nil))
    (loop :for (stage-type . code) :in stages 
       :for new-args = `(,@in-vars ,@(when uniforms (cons '&uniforms uniforms))
                         &context ,@(cons stage-type context))
       :do (let ((result (translate new-args `(progn ,@code))))
             (setf in-vars 
                   (loop :for (name qualifiers value) :in (out-vars result)
                      :collect `(,name ,(type->type-spec (v-type value))
                                       ,@qualifiers)))             
             (push result wip))
       :finally (return (reverse wip)))))

;;----------------------------------------------------------------------

;;[TODO] Move these errors
(defun check-arg-forms (in-args &aux )
  (loop for stream in in-args :do 
       (when (or (not (every #'keywordp (cddr stream))) (< (length stream) 2))
         (error "Declaration ~a is badly formed.~%Should be (-var-name- -var-type- &optional qualifiers)" stream)))
  t)

(defun check-for-dups (in-vars uniforms)  
  (if (intersection (mapcar #'first in-vars) (mapcar #'first uniforms))
      (error "Varjo: Duplicates names found between in-vars and uniforms")
      t))

(defun split-input-into-env (args body env)
  (let* ((uni-pos (symbol-name-position '&uniform args))
         (context-pos (symbol-name-position '&context args))
         (in-vars (subseq args 0 (or uni-pos context-pos)))
         (uniforms (when uni-pos (subseq args (1+ uni-pos) context-pos)))
         (context (when context-pos (subseq args (1+ context-pos)))))
    (when (and (check-arg-forms uniforms) (check-arg-forms in-vars)
               (check-for-dups in-vars uniforms))
      (setf (v-raw-in-args env) in-vars)
      (setf (v-raw-uniforms env) uniforms)
      (setf (v-raw-context env) context)
      (when (not context-pos) (setf (v-context env) *default-context*))
      (values body env))))

;;----------------------------------------------------------------------

(defun process-context (code env)
  ;; ensure there is a version
  (unless (loop :for item :in (v-raw-context env) 
             :if (find item *supported-versions*) :return item)
    (push *default-version* (v-raw-context env)))
  (setf (v-context env) 
        (loop :for item :in (v-raw-context env)
           :if (find item *valid-contents-symbols*) :collect item
           :else :do (error 'invalid-context-symbol :context-symb item)))
  (values code env))

;;----------------------------------------------------------------------

(defun process-in-args (code env)
  "Populate in-args and create fake-structs where they are needed"
  (let ((in-args (v-raw-in-args env)))
    (loop :for (name type . qualifiers) :in in-args :do
       (let* ((type-obj (type-spec->type type :place t)))
         (if (typep type-obj 'v-struct)
             (add-fake-struct name type-obj qualifiers env)
             (progn
               (add-var name (make-instance 'v-value :type type-obj 
                                            :glsl-name (safe-glsl-name-string 
                                                        (free-name name)))
                        env t)
               (push `(,name ,(type->type-spec type-obj) ,qualifiers) 
                     (v-in-args env))))))
    (values code env)))

;;----------------------------------------------------------------------

(defun process-uniforms (code env)
  (let ((uniforms (v-raw-uniforms env)))
    (loop :for (name type) :in uniforms :do
       (let ((true-type (v-true-type (type-spec->type type))))
         (add-var name (make-instance 'v-value
                                      :glsl-name (safe-glsl-name-string 
                                                  (free-name name))
                                      :type (set-place-t (type-spec->type 
                                                          true-type))) 
                  env t))
       (push (list name type) (v-uniforms env)))
    (values code env)))

;;----------------------------------------------------------------------

(defun wrap-in-main-function (code env)
  (values `(%make-function :main () ,code)
          env))

;;----------------------------------------------------------------------

(defun add-context-glsl-vars (code env)
  (values code (add-glsl-vars env)))

;;----------------------------------------------------------------------

(defun v-macroexpand-all (code env)
  (cond ((atom code) code)
        (t (let* ((head (first code))
                  (m (get-macro head env)))
             (if m 
                 (v-macroexpand-all (apply m (rest code)) env)
                 (loop :for c :in code :collect (v-macroexpand-all c env)))))))

(defun macroexpand-pass (code env)
  (values (v-macroexpand-all code env) env))

;;----------------------------------------------------------------------

(defun v-compiler-macroexpand-all (code env)
  (cond ((atom code) code)
        (t (let* ((head (first code))
                  (m (get-compiler-macro head env)))
             (if m 
                 (v-compiler-macroexpand-all (apply m (rest code)) env)
                 (loop :for c :in code :collect (v-compiler-macroexpand-all c env)))))))

(defun compiler-macroexpand-pass (code env)
  (values (v-compiler-macroexpand-all code env) env))

;;----------------------------------------------------------------------

(defun compile-pass (code env)
  (varjo->glsl code env))

;;----------------------------------------------------------------------

(defun filter-used-items (code env)
  "This changes the code-object so that used-types only contains used
   'user' defined structs."
  (setf (stemcells code) (normalize-used-types (stemcells code)))
  (setf (used-types code) 
        (loop :for i :in (remove-duplicates (find-used-user-structs code env))
           :collect (type-spec->type i :env env)))
  (values code env))

;;----------------------------------------------------------------------

(defun gen-in-arg-strings (code env &aux position)
  ;;`(,fake-slot-name ,slot-type ,qualifiers)
  (when (find :vertex (v-context env)) (setf position 0))
  (setf (v-in-args env) 
        (loop :for (name type qualifiers) :in (v-in-args env)
           :for type-obj = (type-spec->type type :env env)           
           :collect (gen-in-var-string name type-obj qualifiers position)
           :if position :do (incf position (v-glsl-size  type-obj))))
  (values code env))

;;----------------------------------------------------------------------

(defun dedup-out-vars (out-vars)
  (let ((seen (make-hash-table))
        (deduped nil))
    (loop :for (name qualifiers value) :in out-vars
       :do (let ((tspec (type->type-spec (v-type value))))
             (if (gethash name seen)
                 (unless (equal tspec (gethash name seen))
                   (error 'out-var-type-mismatch :var-name name 
                          :var-types (list tspec (gethash name seen))))
                 (setf (gethash name seen) tspec
                       deduped (cons (list name qualifiers value) deduped)))))
    deduped))

(defun gen-out-var-strings (code env)
  (let ((out-vars (dedup-out-vars (out-vars code))))
    (setf (out-vars code)
          (loop :for (name qualifiers value) :in out-vars
             :collect (list name qualifiers value 
                            (gen-out-var-string name qualifiers value))))
    (values code env)))

;;----------------------------------------------------------------------

(defun final-uniform-strings (code env)
  (let (final-strings 
        (structs (used-types code)))
    (loop :for (name type) :in (v-uniforms env)
       :for type-obj = (type-spec->type type) 
       :do (push (gen-uniform-decl-string name type-obj) final-strings)
       :do (when (and (v-typep type-obj 'v-user-struct)
                      (not (find (type->type-spec type-obj) structs
                                 :key #'type->type-spec :test #'equal)))
             (push type-obj structs)))
    (setf (used-types code) structs)
    (setf (v-uniforms env) final-strings))
  (values code env))

;;----------------------------------------------------------------------

(defun dedup-strings (code env)
  (setf (to-top code)
        (remove-duplicates (to-top code) :test #'equal))
  (setf (signatures code)
        (remove-duplicates (signatures code) :test #'equal))
  (setf (used-types code)
        (remove-duplicates (mapcar #'v-signature (used-types code)) 
                           :test #'equal))
  (values code env))

;;----------------------------------------------------------------------

(defun final-string-compose (code env)
  (setf (current-line code) (gen-shader-string code env))
  (values code env))

;;----------------------------------------------------------------------

(defun code-obj->result-object (code env) 
  (declare (ignore env))
  (make-instance 'varjo-compile-result
                 :glsl-code (current-line code)
                 :out-vars (loop :for (name qualifiers value string)
                              :in (out-vars code) :collect
                              (list name qualifiers value))
                 :used-external-functions (used-external-functions code)))
