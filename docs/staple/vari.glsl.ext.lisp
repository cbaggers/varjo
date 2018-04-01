(in-package :varjo.tests)

(defmethod staple:system-options append
    ((system (eql (asdf:find-system :varjo))))
  (list :template (asdf:system-relative-pathname
                   :varjo "docs/staple/template.ctml")
        :name "Vari"
        :packages '(:vari :cl)
        :documentation (asdf:system-relative-pathname
                        :varjo "docs/staple/vari-ref-doc-header.md")
        :out (asdf:system-relative-pathname
              :varjo "docs/staple/vari-reference.html")
        :if-exists :supersede))

(defmethod staple:render-docstring
    (string (system (eql (asdf:find-system :varjo))))
  (typecase string
    (string (staple:render-docstring-see-also string))
    ;;(string (staple:render-docstring-markdown string))
    (null (plump:parse "<i>No docstring provided.</i>"))))

;;------------------------------------------------------------

(defclass glsl-func (staple:symb-function)
  ((args :initform nil :initarg :args)))

(defclass glsl-var (staple:symb-variable)
  ())

(defclass cl-func (staple:symb-function)
  ((args :initform nil :initarg :args)))

(defclass cl-var (staple:symb-variable)
  ())

(defun parse-type-to-vari-string (glsl-type)
  (let ((spec
         (varjo:type->type-spec
          (varjo:type-spec->type
           (vari.glsl::parse-gl-type-name glsl-type)))))
    (typecase spec
      (null spec)
      (list (format nil "~{~a~}" (mapcar #'parse-type-to-vari-string spec)))
      (keyword (format nil "~s" spec))
      (otherwise (format nil "~a" spec)))))

(defmethod staple:symb< ((a glsl-func) (b glsl-func))
  (string< (symbol-name (staple:symb-symbol a))
           (symbol-name (staple:symb-symbol b))))

(defmethod staple:symb< ((a glsl-var) (b glsl-var))
  (string< (symbol-name (staple:symb-symbol a))
           (symbol-name (staple:symb-symbol b))))

(defmethod staple:symb-type-order ((symb (eql 'glsl-var)))
  200)

(defmethod staple:symb-type-order ((symb (eql 'glsl-func)))
  210)

(defun format-vari-symb (symbol)
  (let* ((doc (vari:vari-describe symbol nil))
         (copy (search "Copyright" doc)))
    (if copy
        (string-trim
         '(#\space)
         (concatenate
          'string
          (subseq doc 0 copy)
          "Copyright © 2011-2014 Khronos Group"))
        doc)))

(defmethod staple:symb-documentation ((symb glsl-func))
  (format-vari-symb (staple:symb-symbol symb)))

(defmethod staple:symb-documentation ((symb glsl-var))
  (format-vari-symb (staple:symb-symbol symb)))

(defmethod staple:symb-documentation ((symb cl-func))
  (format-vari-symb (staple:symb-symbol symb)))

(defmethod staple:symb-documentation ((symb cl-var))
  (format-vari-symb (staple:symb-symbol symb)))

(defun get-func-specs (symb)
  (loop :for func :in glsl-spec:*functions*
     :when (destructuring-bind (&key lisp-name &allow-other-keys) func
             (string= symb lisp-name))
     :collect func))

(staple:define-converter glsl-func (symbol package)
  (let ((for-cl (eq package (find-package :cl))))
    (when (and (or (eq package (find-package :vari))
                   (and for-cl
                        (not (find-symbol (symbol-name symbol) :vari))))
               (vari:vari-describe symbol nil))
      (list
       (if (eq (symbol-package symbol)
               (find-package :glsl-symbols.variables))
           (make-instance
            (if for-cl
                'cl-var
                'glsl-var)
            :symbol symbol)
           (make-instance
            (if for-cl
                'cl-func
                'glsl-func)
            :symbol symbol
            :args (loop
                     :for spec :in (get-func-specs symbol) :collect
                     (destructuring-bind (&key args &allow-other-keys) spec
                       args))))))))

(defmethod staple:symb-arguments ((symb glsl-func))
  (let ((count (length (slot-value symb 'args))))
    (if (= count 1)
        (list count '#:|overload|)
        (list count '#:|overloads|))))

(defmethod staple:symb-arguments ((symb cl-func))
  (let ((count (length (slot-value symb 'args))))
    (if (= count 1)
        (list count '#:|overload|)
        (list count '#:|overloads|))))

;;------------------------------------------------------------
