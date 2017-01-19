(in-package :varjo)
(in-readtable fn:fn-reader)

;;-------------------------------------------------------------------------

(defvar *metadata-kinds* nil)

;;-------------------------------------------------------------------------

;; {TODO} proper error
(defmacro def-metadata-kind (name (&key conc-name) &body slot-names)
  (check-metadata-slots slot-names)
  `(progn
     (defclass ,name (standard-value-metadata)
       ,(mapcar λ`(,_ :initform nil :initarg ,(kwd _))
                slot-names))
     ,@(mapcar λ`(defmethod ,(if conc-name (symb conc-name _) _)
                     ((metadata-collection list))
                   (let ((data (cdr (assoc ',name metadata-collection))))
                     (when data
                       (slot-value data ',_))))
               slot-names)
     ,@(mapcar λ`(defmethod ,(if conc-name (symb conc-name _) _)
                     ((metadata ,name))
                   (slot-value metadata ',_))
               slot-names)
     (defmethod print-object ((obj ,name) stream)
       (print-unreadable-object (obj stream :type t :identity t)
         (with-slots ,slot-names obj
           (format stream ,(format nil "~{:~a ~~a~^ ~}" slot-names)
                   ,@slot-names))))
     (push ',name *metadata-kinds*)))

;; {TODO} proper error
(defun check-metadata-slots (slots)
  (assert (every #'symbolp slots)))

(defun known-metadata-kind-p (name)
  (not (null (member name *metadata-kinds*))))

;;-------------------------------------------------------------------------
;; Combining Metadata
;;
;; If you throw an error from this method it will be caught, extra details will
;; be added and then it will be rethrown.

(defmethod combine-metadata ((meta-a standard-value-metadata)
                             (meta-b standard-value-metadata))
  (values nil nil))

;; {TODO} proper error
(defmethod combine-metadata ((meta-a standard-value-metadata)
                             (meta-b null))
  (error "Varjo: Compiler Bug: The second argument to #'combine-metadata should
never be null"))

;; {TODO} proper error
(defmethod combine-metadata ((meta-a null)
                             (meta-b null))
  (error "Varjo: Compiler Bug: Tried to combine metadata with two null objects"))

;;-------------------------------------------------------------------------
