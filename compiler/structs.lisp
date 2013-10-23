(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Structs
;;--------------

(defun struct-init-form (struct)
  (let* ((struct-name (safe-gl-name (first struct)))
         (slots (rest struct)))
    (format nil "struct ~(~a~) {~%~{~a~%~}};"
            struct-name (mapcar #'compile-struct-type slots))))

(defun compile-struct-type (slot)
  (let ((name (safe-gl-name (or (third slot) (first slot))))
        (type (flesh-out-type (second slot))))
    (let ((principle (varjo-type->glsl-type (first type)))
          (len (third type)))
      (if len
          (format nil "    ~a ~a[~a];" 
                  principle name len)
          (format nil "    ~a ~a;" 
                  principle name)))))

(defun type-struct-p (type)
  (let ((ftype (flesh-out-type type)))
    (not (null (assoc (type-principle ftype) *struct-definitions*)))))

(defun struct-definition (type-name)
  (let ((descrip (assoc type-name *struct-definitions*)))
    (or (rest descrip) 
        (error "Varjo: Struct ~a does not exist" type-name))))

(defun get-struct-definitions (types)
  (if (not types) 
      (error "Varjo: get-struct-definitions called with no types")

      (let* ((found (loop for type in types 
                       :collect (assoc type
                                       *struct-definitions*)))
             (error-pos (position-if #'null found)))
        (if (not error-pos)
            found
            (error "Varjo: Struct ~a does not exist" 
                   (nth error-pos types))))))

(defun fake-struct-vars (var-name struct-name)
  (let ((slots (rest (first (get-struct-definitions 
                             (list struct-name))))))
    (loop for slot in slots
       :collect `(,(symb '-f- var-name '- (var-name slot))
                   ,(flesh-out-type (var-type slot))
                   ,(safe-gl-name '-f- var-name '- (var-name slot))))))

(defun make-fake-struct (struct-name)
  (let ((fake-type (symb '-f- struct-name))
        (slots (rest (first (get-struct-definitions 
                             (list struct-name))))))    
    (list
     (list struct-name fake-type)
     (loop :for slot :in slots 
        :collect
        (list (or (fifth slot) (symb struct-name '- (first slot)))
              (vlambda :in-args `((x (,fake-type)))
                       :output-type
                       (literal-number-output-type
                        (set-place-t 
                         (flesh-out-type 
                          (second slot))))
                       :transform (format nil "_f_~~(~~a_~a~~)" 
                                          (safe-gl-name (first slot)))))))))

(defun literal-number-output-type (type)
  (loop for i in type :collect (if (numberp i) (list i) i)))

(defun struct-funcs (struct)
  (%struct-funcs (first struct) nil nil 
                 (loop for slot in (rest struct)
                    collect (list (safe-gl-name (first slot))
                                  (second slot)))))

(defun %struct-funcs (name slot-prefix context-restriction slots)
  (cons 
   (list (symb 'make- (or slot-prefix name))
         (vlambda :in-args (loop for slot in slots
                              :collect (subseq slot 0 2))
                  :output-type name
                  :transform (format nil "~a(~{~a~^,~^ ~})"
                                     name
                                     (loop for slot in slots
                                        collect "~a"))
                  :context-restriction context-restriction))
   (loop :for slot :in slots 
      :collect
      (list (or (fifth slot)
                (symb (or slot-prefix name) '- (first slot)))
            (vlambda :in-args `((x (,name)))
                     :output-type 
                     (literal-number-output-type
                      (set-place-t (flesh-out-type (second slot))))
                     :transform (format nil "~~a.~a" 
                                        (or (third slot) (first slot)))
                     :context-restriction context-restriction)))))

(defmacro vdefstruct (name &body slots)
  (let ((*types* (cons (list name nil) *built-in-types*))) 
    `(progn     
       (setf *glsl-functions* 
             (acons-many ',(%struct-funcs name nil nil slots)
                         *glsl-functions*))
       (setf *struct-definitions*
             (acons ',name 
                    ',(loop :for (name vtype gl-name read-only restriction) 
                         :in slots :collect (list name vtype
                                                  (safe-gl-name name)
                                                  read-only restriction))
                    *struct-definitions*))
       ',name)))

;; [TODO] context-restriction limits functions but type?
(defmacro %vdefstruct (name (&key slot-prefix context-restriction)
                       &body slots)
  (let ((*types* (cons (list name nil) *built-in-types*))) 
    `(progn
       (setf *glsl-functions* 
             (acons-many ',(%struct-funcs name slot-prefix
                                          context-restriction 
                                          slots)
                         *glsl-functions*))
       (setf *built-in-types* 
             (acons ',name '(nil) *built-in-types*))
       ',name)))

(defun substitute-alternate-struct-types (in-vars type-alist)
  (loop :for in-var in in-vars
     :collect (list (var-name in-var)
                    (or (first 
                         (assocr 
                          (type-principle (var-type in-var)) 
                          type-alist))
                        (var-type in-var))
                    (var-gl-name in-var)
                    (var-read-only in-var))))
