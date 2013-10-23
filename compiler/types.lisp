(in-package :varjo)


(defun flesh-out-type-with-check (type)
  (if (not (listp type))
      (flesh-out-type-with-check (list type))
      (if (if (consp (first type))
              (every #'(lambda (x) (assoc x *types*))
                     (first type))
              (assoc (first type) *types*))
          (flesh-out-type type)
          (error "Varjo: '~s' is not a valid type in this context ~a" type *shader-context*))))

(defun flesh-out-type (type-spec)
  (if (consp type-spec)
      (if (> (length type-spec) 4)
          (error "Invalid GLSL Type Definition: ~s has more than 4 components." type-spec)
          (list (type-principle type-spec)
                (type-array-length type-spec)
                (type-place type-spec)
                (or (type-gl-name type-spec)
                    (when (symbolp (type-principle type-spec))
                      (safe-gl-name (type-principle type-spec))))))
      (flesh-out-type (list type-spec))))

(defun glsl-valid-type (candidate spec)
  (let ((type-s (first spec)) (type-c (first candidate))
        (length-s (second spec)) (length-c (second candidate)))
    (not
     (null
      (and (or (eq type-s t) (if (listp type-s) 
                                 (find type-c type-s) 
                                 (eq type-c type-s)))
           (or (eq length-c length-s)
               (and (eq length-s t) length-c)
               (when (and (numberp length-c) (numberp length-s))
                 (<= length-c length-s))))))))

(defun set-place-t (type)
  (list (first type) (second type) t (fourth type)))

(defun set-place-nil (type)
  (list (first type) (second type) nil (fourth type)))

(defun get-place (x)
  (third x))

(defun placep (object)
  (get-place (code-type object)))

(defun glsl-typep (object type)
  (glsl-valid-type (code-type object) type))

(defun type-equal (a b)
  (equal (subseq a 0 2) (subseq b 0 2)))

;;-----------
(defun type-principle (type)
  (first type))

(defun type-arrayp (type)
  (not (null (second type))))

(defun type-array-length (type)
  (second type))

(defun type-place (type)
  (third type))

(defun type-placep (type)
  (third type))

(defun type-gl-name (type)
  (fourth type))

(defun type-built-inp (type)
  (not (null (assoc (type-principle type) *built-in-types*))))

(defun built-in-vars (context)
  (loop :for part :in context
     :append (assocr part *built-in-vars*
                     :test #'symbol-name-equal)))
;;-----------

(defun glsl-castablep (minor-type major-type)
  "Returns whether the type minor-type can be cast up to type major-type"
  (or (type-equal major-type minor-type)
      (not (null (find minor-type (assoc major-type 
                                         *implicit-type-casts*
                                         :test #'type-equal)
                       :test #'type-equal)))))

(defun superior-type (&rest types)
  "find the superior type, types are defined in order or superiority"
  (let ((type-strengths 
         (remove-if #'null 
                    (mapcar (lambda (x) 
                              (position x *types*
                                        :test #'type-equal))
                            types))))
    (when type-strengths
      (elt *types* (apply #'max type-strengths)))))

(defun types-compatiblep (&rest types)
  "Make sure every type is or can be cast up to the superior type"
  (let ((superior (apply #'superior-type types)))
    (every #'(lambda (x) (glsl-castablep x superior)) types)))

(defun type-aggregate-p (type-spec)
  (let* ((full-type (flesh-out-type type-spec))
         (type (type-principle full-type))
         (length (rest (assoc type *glsl-component-counts*))))
    (when length t)))

(defun type-component-count (type-spec)
  (let* ((full-type (flesh-out-type type-spec))
         (type (type-principle full-type))
         (length (assocr type *glsl-component-counts*)))
    (if length
        length
        (error "Type '~a' is not a vector or matrix componant type" full-type))))

(defun type-component-type (type)
  (let* ((ftype (flesh-out-type type))
         (ptype (type-principle ftype)))
    (if (type-arrayp ftype) 
        ptype
        (if (assocr ptype *glsl-component-type*)
            (assocr ptype *glsl-component-type*)
            (error "Type '~s' is not a vector or matrix componant type" 
                   ptype)))))

(defun type-glsl-size (type-spec)
  (let* ((full-type (flesh-out-type type-spec))
         (type (type-principle full-type))
         (length (rest (assoc type *glsl-type-sizes*))))
    (if length
        length
        (error "Type '~a' is not a vector or matrix componant type" type))))

(defun mat-typep (mat-type)
  (equal "MAT" (subseq (symbol-name (type-principle (flesh-out-type mat-type))) 0 3)))

(defun mat/vec-length (mat-type)
  (let ((name (symbol-name (type-principle (flesh-out-type mat-type)))))
    (parse-integer (string (elt name (1- (length name)))))))

(defun vec-typep (mat-type)
  (let ((name (symbol-name (type-principle (flesh-out-type mat-type)))))
    (equal "VEC" (subseq name (max 0 (- (length name) 4)) (1- (length name))))))

(defun type-mat-col-to-vec (mat-type)
  (kwd (format nil "VEC~a" 
               (elt (symbol-name 
                     (type-principle (flesh-out-type mat-type))) 
                    3))))

(defun type-vec-core-type (vec-type)
  (let* ((name (symbol-name (type-principle (flesh-out-type vec-type))))
         (id (elt name 0)))
    (cond ((eq id #\V) :float)
          ((eq id #\I) :int)
          ((eq id #\U) :uint)
          (t (error "unknown vector type")))))

(defun change-vec-length (vec-type length)
  (if (type-arrayp vec-type)
      (error "Varjo: Expected vector, got vector array")
      (let* ((type (string (type-principle vec-type)))
             (len (length type))
             (base (subseq type 0 (1- len))))
        (flesh-out-type (kwd base length)))))

(defun varjo-type->glsl-type (type-spec)
  (let* ((type (flesh-out-type type-spec))
         (type-name (or (type-gl-name type) (type-principle type)))
         (len (second type)))
    (if len
        (format nil "~a[~a]" type-name (if (numberp len) len ""))
        (format nil "~a" type-name))))
