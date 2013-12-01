;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :varjo)

;; [TODO] Proper error needed here
(defmethod initialize-instance :after
    ((code-obj code) &key (type nil set-type))
  (unless set-type (error "Type must be specified when creating an instance of varjo:code"))
  ;;[TODO] dont pass in the name of a fake struct here, must be object
  (let* ((type-obj (if (typep type 'v-t-type) type (type-spec->type type)))
         (type-spec (v-type-name type-obj)))
    (setf (slot-value code-obj 'type) type-obj)
    (when (and (not (find type-spec (used-types code-obj)))
               (not (eq type-spec 'v-none)))
      (push (listify type-spec) (used-types code-obj)))))

;; [TODO] this doesnt work (properly) yet but is a fine starting point
(defgeneric copy-code (code-obj &key type current-line to-block to-top 
                                  out-vars invariant returns))
(defmethod copy-code ((code-obj code) 
                      &key type current-line 
                        (signatures nil set-sigs)
                        (to-block nil set-block)
                        (to-top nil set-top)
                        (out-vars nil set-out-vars)
                        (invariant nil) (returns nil set-returns))
  (make-instance 'code 
                 :type (if type type (code-type code-obj)) 
                 :current-line (if current-line current-line 
                                   (current-line code-obj)) 
                 :signatures (if set-sigs signatures (signatures code-obj))
                 :to-block (if set-block to-block (to-block code-obj))
                 :to-top (if set-top to-top (to-top code-obj))
                 :out-vars (if set-out-vars out-vars (out-vars code-obj))
                 :invariant (if invariant invariant (invariant code-obj))
                 :returns (if set-returns returns (returns code-obj))
                 :used-types (used-types code-obj)
                 :stemcells (stemcells code-obj)))

(defmethod merge-obs ((objs list) &key type current-line 
                                    (signatures nil set-sigs)
                                    (to-block nil set-block)
                                    (to-top nil set-top)
                                    (out-vars nil set-out-vars)
                                    (invariant nil) (returns nil set-returns))
  (make-instance 'code
                 :type (if type type (error "type is mandatory")) 
                 :current-line current-line 
                 :signatures (if set-sigs signatures 
                                 (mapcan #'signatures objs))
                 :to-block (if set-block to-block (mapcan #'to-block objs))
                 :to-top (if set-top to-top (mapcan #'to-top objs))
                 :out-vars (if set-out-vars out-vars (mapcan #'out-vars objs))
                 :invariant invariant
                 :returns (if set-returns returns (mapcan #'returns objs))
                 :used-types (mapcar #'used-types objs)
                 :stemcells (mapcar #'stemcells objs)))

(defmethod merge-obs ((objs code) 
                      &key (type nil set-type)
                        (signatures nil set-sigs)
                        (current-line nil set-current-line) 
                        (to-block nil set-block)
                        (to-top nil set-top)
                        (out-vars nil set-out-vars)
                        (invariant nil) (returns nil set-returns))
  (make-instance 'code
                 :type (if set-type type (code-type objs)) 
                 :current-line (if set-current-line current-line 
                                   (current-line objs)) 
                 :signatures (if set-sigs signatures (signatures objs))
                 :to-block (if set-block to-block (remove nil (to-block objs)))
                 :to-top (if set-top to-top (remove nil (to-top objs)))
                 :out-vars (if set-out-vars out-vars (out-vars objs))
                 :invariant invariant
                 :returns (if set-returns returns (returns objs))
                 :used-types (used-types objs)
                 :stemcells (stemcells objs)))

(defun make-none-ob ()
  (make-instance 'code :type :none :current-line nil))

(defun normalize-used-types (types)
  (loop :for item :in (remove nil types) :append
     (cond ((atom item) (list item))
           ((and (listp item) (numberp (second item))) (list item))
           (t (normalize-used-types item)))))

(defun find-used-user-structs (code-obj)
  (let ((used-types (normalize-used-types (used-types code-obj))))
    (remove nil (loop :for type :in used-types :collect
                   (let ((principle-type (if (listp type) (first type) type)))
                     (when (typep (make-instance principle-type) 'v-user-struct)
                       principle-type))))))


