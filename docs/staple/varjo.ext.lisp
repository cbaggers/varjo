(in-package :varjo.tests)

;;------------------------------------------------------------

(defmethod staple:system-options append
    ((system (eql (asdf:find-system :varjo))))
  (list :template (asdf:system-relative-pathname
                   :varjo "docs/staple/template.ctml")
        :name "Varjo"
        :packages '(:varjo.api)
        :documentation (asdf:system-relative-pathname
                        :varjo "docs/staple/varjo-ref-doc-header.md")
        :out (asdf:system-relative-pathname
              :varjo "docs/staple/varjo-reference.html")
        :if-exists :supersede))

(defmethod staple:render-docstring
    (string (system (eql (asdf:find-system :varjo))))
  (typecase string
    (string (staple:render-docstring-markdown string))
    (null (plump:parse "<i>No docstring provided.</i>"))))

;;------------------------------------------------------------
