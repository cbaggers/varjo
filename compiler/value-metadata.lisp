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
     ,(when slot-names (gen-meta-init-check name slot-names))
     (defmethod print-object ((obj ,name) stream)
       (print-unreadable-object (obj stream :type t :identity t)
         (with-slots ,slot-names obj
           (format stream ,(format nil "~{:~a ~~a~^ ~}" slot-names)
                   ,@slot-names))))
     (push ',name *metadata-kinds*)))

(defun gen-meta-init-check (name slot-names)
  (let ((init-args (loop :for name :in slot-names :collect
                      `(,name nil ,(gensym (symbol-name name))))))
    `(defmethod initialize-instance :after
       ((md ,name) &rest all &key ,@init-args)
       (declare (ignore ,@slot-names))
       (assert (and ,@(mapcar #'third init-args)) ()
               'v-metadata-missing-args
               :name ',name
               :required ',slot-names
               :provided all
               :missing (remove nil (list ,@(loop :for (n nil c) :in init-args
                                               :collect `(unless ,c ',n))))))))


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
;; Find similar declaration names

(defun find-alternative-declaration-kinds (decl-name)
  (find-similarly-named-symbol decl-name *metadata-kinds*))
