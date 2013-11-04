;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

;;----------------------------------------------------------------------

(defun extract-uniforms (args)
  (let ((uni-pos (symbol-name-position '&uniform args)))
    (when uni-pos (subseq args (1+ uni-pos) 
                          (symbol-name-position '&context args)))))

(defun uniform-default-val (x)
  (when (consp (first x)) (second x)))

(defun uniform->var (x)
  (if (uniform-default-val x) (first x) x))

(defun flesh-out-arg (var)
  (list (var-name var)
        (flesh-out-type (or (expand-built-in-type (var-type var))
                            (var-type var)))
        (safe-gl-name (var-name var))
        t))

(defun flesh-out-args (in-vars)
  "This fleshes out the type, adds a lowercase version
   of the name and sets the variable to read-only"
  (mapcar #'flesh-out-arg in-vars))

(defun get-uniform-struct-types (uniforms) 
  (remove-i
f #'null
             (remove-duplicates
              (loop for u in uniforms
                 :if (not (type-built-inp (var-type u)))
                 :collect (type-principle (var-type u))))))

(defun expand-struct-in-vars (in-vars qualifiers)  
  "Transforms struct invars into the component variables"
  (loop for i in in-vars
     for q in qualifiers
     :if (type-built-inp (var-type i))
     :collect (list i q)
     :else
     :append 
       (mapcar #'list 
               (fake-struct-vars
                (var-name i) (type-principle (var-type i))))))

(defun create-fake-structs-from-in-vars (in-vars) 
  "Transforms struct invars into the component variables"
  (remove-duplicates
   (loop for i in in-vars
      :if (not (type-built-inp (var-type i)))
      :collect (make-fake-struct (type-principle (var-type i))))
   :test #'equal))

(defun layout-size (type-spec)
  (let* ((type (flesh-out-type type-spec))
         (principle (first type))
         (length (second type))
         (size (assocr principle *glsl-type-sizes*)))
    (cond ((null length) size)
          ((integerp length) (* size length))
          (t (error "Cannot have array of unknown size as an input")))))

(defun add-layout-qualifiers-to-in-vars (compiled-in-vars)
  (loop for ob in compiled-in-vars
     :with total = 0	
     :collect (list 
               (qualify ob (fmt "layout(location=~a)" total))
               total)
     :do (setf total (+ total (layout-size (code-type ob))))))

(defun compile-declarations (vars &optional default-qualifier)
  (loop for (var qualifiers) in vars
     :collect (end-line
               (%compile-var (var-gl-name var) (var-type var)
                             (append qualifiers
                                     (list default-qualifier))))))


;;----------------------------------------------------------------------

(defun compile-main (code env)
  (varjo->glsl `(%make-function :main () ,@code) env))

(defun rolling-translate (args shaders &optional accum (first-shader t))  
  (if (find :type args)
      (error "Varjo: It is invalid to specify a shader type in a program definition")
      (if shaders
          (let* ((shader (first shaders))
                 (type (first shader)))
            (destructuring-bind (glsl new-args)
                (varjo:translate (if (find '&context args 
                                           :test #'symbol-name-equal)
                                     (append args `(:type ,type))
                                     (append args `(&context :type ,type)))
                                 (rest shader) first-shader)
              (rolling-translate new-args (rest shaders) (cons glsl accum) nil)))
          (progn (reverse accum)))))

(defun translate (args code &optional first-shader)
  (destructuring-bind (shader-type version in-vars 
                                   in-var-declarations uniform-vars
                                   struct-functions struct-definitions types)
      (parse-shader-args args)
    (let* ((*shader-context* (list :core shader-type version))
           (*types* (acons-many (loop for i in types
                                   collect (list i nil)) 
                                *built-in-types*))
           (*glsl-variables* (append (built-in-vars 
                                      *shader-context*)
                                     uniform-vars 
                                     in-vars))
           (*glsl-functions* (acons-many struct-functions 
                                         *glsl-functions*))
           (compiled-obj (compile-main code))
           (compiled-in-vars (let ((compiled (compile-declarations
                                              in-var-declarations :in))) 
                               (if first-shader
                                   (add-layout-qualifiers-to-in-vars compiled)
                                   (mapcar #'list compiled))))
           (compiled-uniforms (compile-declarations 
                               (mapcar #'list uniform-vars)
                               :uniform))
           (deduped-out-vars (check-and-dedup-out-vars (out-vars compiled-obj)))
           (out-vars (loop for i in deduped-out-vars
                        :collect `(,(first i) ,(set-place-nil (second i))
                                    ,@(cdddr i))))
           (compiled-out-vars (compile-declarations 
                               (loop for var in deduped-out-vars
                                  collect (list (subseq var 0 3)
                                                (subseq var 3))) :out)))
      (list (list (kwd shader-type '-shader)
                  (write-output-string version struct-definitions
                                       compiled-obj compiled-in-vars
                                       compiled-out-vars
                                       compiled-uniforms))
            `(,@out-vars ,@(when uniform-vars 
                                 (cons '&uniform (mapcar #'(lambda (x) 
                                                             (subseq x 0 2))
                                                         uniform-vars)))
                &context :version ,version)))))

(defun check-and-dedup-out-vars (out-vars)
  (let* ((dedup (remove-duplicates out-vars :test #'equal))
         (names (remove-duplicates (mapcar #'first dedup))))
    (if (< (length names) (length dedup))
        (error "Varjo: Sorry you can't have out variables~%that share a name but don't have the same type:~%~s " dedup)
        dedup)))
