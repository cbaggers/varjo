;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :varjo)

(defmethod initialize-instance :after 
    ((code-ob code) &key (type nil set-type)
                      (current-line nil set-current))
  (if (not (and set-current set-type))
      (error "Type and current-line must be specified when creating an instance of varjo:code"))
  (setf (slot-value code-ob 'type-spec) (flesh-out-type type)
        (slot-value code-ob 'current-line) current-line))


(defmethod merge-obs ((objs list) &key type current-line 
                                    (to-block nil set-block)
                                    (to-top nil set-top)
                                    (out-vars nil set-out-vars)
                                    (invariant nil) (returns nil set-returns))
  (make-instance 'code
                 :type (if type type (error "type is mandatory")) 
                 :current-line current-line 
                 :to-block (if set-block to-block (mapcan #'to-block objs))
                 :to-top (if set-top to-top (mapcan #'to-top objs))
                 :out-vars (if set-out-vars out-vars (mapcan #'out-vars objs))
                 :invariant invariant
                 :returns (if set-returns returns (mapcan #'returns objs))))

(defmethod merge-obs ((objs code) 
                      &key (type nil set-type)
                        (current-line nil set-current-line) 
                        (to-block nil set-block)
                        (to-top nil set-top)
                        (out-vars nil set-out-vars)
                        (invariant nil) (returns nil set-returns))
  (make-instance 'code
                 :type (if set-type type (code-type objs)) 
                 :current-line (if set-current-line current-line 
                                   (current-line objs)) 
                 :to-block (if set-block to-block (to-block objs))
                 :to-top (if set-top to-top (to-top objs))
                 :out-vars (if set-out-vars out-vars (out-vars objs))
                 :invariant invariant
                 :returns (if set-returns returns (returns objs))))

(defun make-none-ob ()
  (make-instance 'code :type :none :current-line ""))

(defun end-line (ob)
  (if (find (type-principle (code-type ob)) '(:none :void))
      ob
      (merge-obs ob :current-line (format nil "~a;" (current-line ob)))))
