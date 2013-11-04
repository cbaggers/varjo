(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Structs
;;--------------

;; (vdefstruct thing ()
;;   (a v-float)
;;   (to-long-to-blah v-int :accessor b))

(defmacro vdefstruct (name context &body slots)
  `(progn 
     (defclass ,name (v-struct) 
       ((glsl-string :initform ,(format nil "struct ~(~a~) {~%~{~a~%~}};"
                                        name (mapcar #'gen-slot-string slots))
                     :initarg :glsl-string :reader v-glsl-string)
        (slots :initform ',slots :reader v-slots)))
     (v-defun ,(symb 'make- name) 
         ,(append (loop :for slot :in slots :collect (first slot))
                  (when context `(&context ,@context)))
       ,(format nil "~a(~{~a~^,~^ ~})" name 
                (loop :for slot :in slots :collect "~a"))
       ,(loop :for slot :in slots :collect (second slot))
       ,name :place nil)
     ,@(loop :for (slot-name slot-type . acc) :in slots :collect
          (let ((accessor (if (eq :accessor (first acc)) (second acc) slot-name)))
            `(v-defun ,accessor (,(symb name '-ob) ,@(when context `(&context ,@context)))
               ,(concatenate 'string "~a." (string slot-name))
               (,name) ,slot-type :place t)))))

(defun gen-slot-string (slot)
  (destructuring-bind (slot-name slot-type &key accessor) slot
    (let ((name (or accessor slot-name))
          (type-obj (type-spec->type slot-type)))
      (if (typep type-obj 'v-array)
          (format nil "    ~a ~a[~a];" 
                  (v-glsl-string (v-element-type type-obj)) 
                  name 
                  (v-dimensions type-obj))
          (format nil "    ~a ~a;" 
                  (v-glsl-string type-obj) name)))))

;;[TODO] Need to make fake type
;;       a bit tricky as how do we define this new type?
;;       we need the functions to specialise on it.
;;       we cnat defclass at runtime, so we could have a fake-struct type
;;       and special logic in the get-functions code to handle it. 
;;       Fake-struct types should have a name slot which must match
(defmethod make-fake-struct (type (env environment))
  (let* ((type (if (typep type 'v-struct) type (type-spec->type type)))
         (slots (v-slots type)))
    (loop :for (slot-name slot-type . acc) :in slots :collect
       (let ((accessor (if (eq :accessor (first acc)) (second acc) slot-name)))
         (add-function accessor
                       (v-make-f-spec (concatenate 'string "~a." 
                                                   (string slot-name))
                                      '(obj) '(v-struct) slot-type :place nil)
                       env)))))

;; (defun fake-struct-vars (var-name struct-name)
;;   (let ((slots (rest (first (get-struct-definitions (list struct-name))))))
;;     (loop for slot in slots
;;        :collect `(,(symb '-f- var-name '- (var-name slot))
;;                    ,(flesh-out-type (var-type slot))
;;                    ,(safe-gl-name '-f- var-name '- (var-name slot))))))

;; (defun make-fake-struct (struct-name)
;;   (let ((fake-type (symb '-f- struct-name))
;;         (slots (rest (first (get-struct-definitions 
;;                              (list struct-name))))))    
;;     (list
;;      (list struct-name fake-type)
;;      (loop :for slot :in slots 
;;         :collect
;;         (list (or (fifth slot) (symb struct-name '- (first slot)))
;;               (vlambda :in-args `((x (,fake-type)))
;;                        :output-type
;;                        (literal-number-output-type
;;                         (set-place-t 
;;                          (flesh-out-type 
;;                           (second slot))))
;;                        :transform (format nil "_f_~~(~~a_~a~~)" 
;;                                           (safe-gl-name (first slot)))))))))


