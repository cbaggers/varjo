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
    ((code-ob code) &key (type nil set-type))
  (unless set-type (error "Type must be specified when creating an instance of varjo:code"))
  (setf (slot-value code-ob 'type) 
        (if (typep type 'v-t-type) type (type-spec->type type))))

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
                 :returns (if set-returns returns (returns code-obj))))

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
                 :returns (if set-returns returns (mapcan #'returns objs))))

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
                 :returns (if set-returns returns (returns objs))))

(defun make-none-ob ()
  (make-instance 'code :type :none :current-line nil))

;;[TODO] This is why break needs a semicolon
(defun end-line (obj)
  (when obj
    (if (typep (code-type obj) 'v-none)
        obj
        (if (null (current-line obj))
            (peek obj)
            (merge-obs obj :current-line (format nil "~a;" (current-line obj)))))))
