(in-package :vari.cl)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(define-vari-trait iter-state ()
  (iter-state-next :self)
  (iterator-limit-check :self :self)
  (index-for-state :self))

(define-vari-trait v-user-sequence ((state iter-state)
                                    (elem-type _))
  (sequence-length :self)
  (sequence-make-like :self)
  (sequence-make-like-with-length :self :int)
  (sequence-limit :self)
  (sequence-create-iterator-state :self)
  (sequence-element-for-state :self state)
  (sequence-set-element-for-state :self state elem-type))

;;------------------------------------------------------------

(v-deftype array-iter-state () :int)

(v-def-glsl-template-fun
 array-iter-state-from-int (i) "~a" (:int) array-iter-state)

(v-def-glsl-template-fun
 array-iter-state-next (s) "(~a++)" (array-iter-state) array-iter-state)

(v-def-glsl-template-fun
 array-iter-limit-check (a b) "(~a < ~a)" (array-iter-state array-iter-state)
 :bool)

(v-def-glsl-template-fun
 array-iter-to-int (a) "~a" (array-iter-state) :int)

(define-vari-trait-implementation array-iter-state (iter-state)
  :iter-state-next (array-iter-state-next array-iter-state)
  :iterator-limit-check (array-iter-limit-check array-iter-state
                                                array-iter-state)
  :index-for-state (array-iter-to-int array-iter-state))

;;------------------------------------------------------------

(v-defspecial make-array-like ((arr v-array))
  :return
  (let* ((arr-type (primary-type arr))
         (elem-type (v-element-type arr-type))
         (len (first (v-dimensions arr-type))))
    (compile-form
     `(make-array ,len :element-type ',(type->type-spec elem-type))
     env)))

(v-defspecial make-array-like-with-len ((arr v-array) (len :int))
  :return
  (let* ((arr-type (primary-type arr))
         (elem-type (v-element-type arr-type)))
    (compile-form
     `(make-array ,len :element-type ',(type->type-spec elem-type))
     env)))

(v-def-glsl-template-fun
 array-iter-limit (a) "~a.length()" (v-array) array-iter-state)

(v-defun array-create-iter-state ((arr v-array)) 0)
(v-define-compiler-macro array-create-iter-state ((arr v-array))
  (declare (ignore arr))
  `(array-iter-state-from-int 0))

(v-def-glsl-template-fun
 set-array-elem (s i e) "|set-array-elem|(~a, ~a, ~a)"
 (v-array array-iter-state t) t)

(v-define-compiler-macro set-array-elem ((arr v-array) (state array-iter-state)
                                         (val t))
  `(setf (aref ,arr (array-iter-to-int ,state)) ,val))

(define-vari-trait-implementation v-array (v-user-sequence :elem-type t)
  :sequence-length (length v-array)
  :sequence-make-like (make-array-like v-array)
  :sequence-make-like-with-length (make-array-like-with-len v-array :int)
  :sequence-limit (array-iter-limit v-array)
  :sequence-create-iterator-state (array-create-iter-state v-array)
  :sequence-element-for-state (aref v-array :int) ;; !!!------WARNING--------!!! BUG
  :sequence-set-element-for-state (set-array-elem v-array array-iter-state t))

;;------------------------------------------------------------

(v-defun mapseq ((fn v-function-type) (seq v-user-sequence))
  (let ((result (sequence-make-like seq))
        (limit (sequence-limit seq)))
    (for (state (sequence-create-iterator-state seq))
         (iterator-limit-check state limit)
         (setq state (iter-state-next state))
         (let ((elem (sequence-element-for-state seq state)))
           (sequence-set-element-for-state result
                                           state
                                           (funcall fn elem))))
    result))

(v-define-compiler-macro mapseq ((fn v-function-type) (seq v-user-sequence))
  `(let* ((seq ,seq)
          (result (sequence-make-like seq))
          (limit (sequence-limit seq)))
     (for (state (sequence-create-iterator-state seq))
          (iterator-limit-check state limit)
          (setq state (iter-state-next state))
          (let ((elem (sequence-element-for-state seq state)))
            (sequence-set-element-for-state result
                                            state
                                            (funcall ,fn elem))))
     result))

(v-defun reduce ((fn v-function-type) (seq v-user-sequence) (initial-value t))
  (let* ((result initial-value)
         (limit (sequence-limit seq)))
    (for (state (sequence-create-iterator-state seq))
         (iterator-limit-check state limit)
         (setq state (iter-state-next state))
         (let ((elem (sequence-element-for-state seq state)))
           (setf result
                 (funcall fn result elem))))
    result))

(v-define-compiler-macro reduce ((fn v-function-type) (seq v-user-sequence) (initial-value t))
  `(let* ((seq ,seq)
          (result ,initial-value)
          (limit (sequence-limit seq)))
     (for (state (sequence-create-iterator-state seq))
          (iterator-limit-check state limit)
          (setq state (iter-state-next state))
          (let ((elem (sequence-element-for-state seq state)))
            (setf result
                  (funcall ,fn result elem))))
     result))
