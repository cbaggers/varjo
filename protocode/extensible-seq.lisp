(in-package :vari.cl)
(in-readtable :fn.reader)

;;------------------------------------------------------------

#+nil
(defgeneric get-vari-iterator-spec (vari-type))

#+nil
(defmacro define-vari-iterable ((vari-type) &body body)
  (alexandria:with-gensyms (type-arg)
    `(defmethod get-vari-iterator-spec ((,type-arg ,vari-type))
       (declare (ignore ,type-arg))
       ,@body)))

#+nil
(defclass vari-iterable-spec ()
  ((like :initform nil :initarg :like)
   (limit :initform nil :initarg :limit)))

#+nil
(defun vari-iterable-spec (&key like limit)
  (make-instance 'vari-iterable-spec
                 :like like
                 :limit limit))

#+nil
(v-defmacro map (type-spec function sequence)
  (declare (ignore type-spec))
  (alexandria:with-gensyms (map-result seq)
    `(let ((,seq ,sequence)
           (,map-result (make-sequence-like ,seq)))
       (map-into ,map-result ,function ,seq)
       ,map-result)))

#+nil
(v-def-glsl-template-fun map-into (result-sequence function sequence)
                         "|map-into|(~a, ~a, ~a)" (t t t) t)

#+nil
(v-define-compiler-macro map-into
    (result-sequence function sequence &environment env)
  (let* ((src-type (varjo.api:argument-type sequence env))
         (dst-type (varjo.api:argument-type result-sequence env))
         (src-spec (get-vari-iterator-spec src-type))
         (dest-spec (get-vari-iterator-spec dst-type)))
    `(for )))

;; six are functions which, respectively:
;; - return a state one step ahead, if possible
;; - test the state against the limit for termination
;; - retrieve the element at the current iteration state from the sequence
;; - set the element at the current iteration state to a new value
;; - return the index corresponding to the current iteration state
;; - return a distinct iteration state representing a copy of the current one.

#+nil
(progn
  (v-defun make-sequence-like ((seq some-seq)))
  (v-defun make-sequence-like ((seq some-seq) (length :int))))

;;------------------------------------------------------------
;; Test


(v-deftype range () :ivec2)
(v-def-glsl-template-fun range (start end)
                         "ivec2(max(0,~a), max(0,~a))"
                         (:int :int) range)
(v-def-glsl-template-fun range-start (r) "~a.x" (range) :int)
(v-def-glsl-template-fun range-end (r) "~a.y" (range) :int)

(v-deftype range-iter-state () :int)
(v-def-glsl-template-fun
 range-iter-state (x) "~a" (:int) range-iter-state)
(v-def-glsl-template-fun
 inc-range-state (x) "~a++" (range-iter-state) range-iter-state)
(v-def-glsl-template-fun
 dec-range-state (x) "~a++" (range-iter-state) range-iter-state)
(v-def-glsl-template-fun
 range-state-to-int (x) "~a" (range-iter-state) :int)

#+nil
(v-defspecial make-sequence (type length)
  :args-valid t
  :return
  ())

#+nil
(v-def-glsl-template-fun
 blort (fn seq) "|blort|(~a, ~a)" (v-function-type t) t)

#+nil
(v-define-compiler-macro blort ((fn v-function-type) (seq t))
  (alexandria:with-gensyms (gfn gseq)
    `(let ((,gfn ,fn)
           (,gseq ,seq))
       (labels ((blort ()
                  (let ((limit (limit ,gseq)))
                    (for (state (create-iterator-state ,gseq))
                         (iterator-limit-check state limit)
                         (setq state (next-iterator-state state))
                         (let ((elem (element-for-state ,gseq state)))
                           (funcall ,gfn elem))))))
         (blort)))))

;;----------------

(v-defun make-sequence-like ((seq range) (length :int))
  (range 0 length))

(v-defun make-sequence-like ((seq range))
  (range (range-start seq) (range-end seq)))

;;----------------

(v-defun length ((seq range))
  (abs (- (range-end seq) (range-start seq))))

;;----------------

(v-defun limit ((seq range))
  (range-iter-state (range-end seq)))

(v-defun limit ((seq range) (from-end :bool))
  (range-iter-state (if from-end
                        (range-start seq)
                        (range-end seq))))

(v-define-compiler-macro limit ((seq range))
  `(range-iter-state (range-end ,seq)))

;;----------------

(v-defun create-iterator-state ((seq range))
  (range-iter-state (range-start seq)))

(v-defun create-iterator-state ((seq range) (from-end :bool))
  (range-iter-state (if from-end
                        (range-end seq)
                        (range-start seq))))

(v-define-compiler-macro create-iterator-state ((seq range))
  `(range-iter-state (range-start ,seq)))

;;----------------

(v-def-glsl-template-fun
 next-iterator-state (state) "(~a + 1)" (range-iter-state) range-iter-state)

(v-defun next-iterator-state ((state range-iter-state) (from-end :bool))
  (if from-end (dec-range-state state) (inc-range-state state)))

;;----------------

(v-def-glsl-template-fun iterator-limit-check (limit state)
                         "(~a < ~a)"
                         (range-iter-state range-iter-state) :bool)

(v-defun iterator-limit-check ((limit range-iter-state)
                               (state range-iter-state)
                               (from-end :bool))
  (if from-end (> state limit) (< state limit)))

;;----------------

(v-def-glsl-template-fun element-for-state (seq state)
                         "~a[~a]"
                         (range range-iter-state)
                         :int)

;;----------------

(v-def-glsl-template-fun index-for-state (state)
                         "~a" (range-iter-state)
                         :int)

(v-define-compiler-macro index-for-state ((state range-iter-state))
  `(range-state-to-int ,state))

;; set doesnt make sense for range
;;
;; (v-defun element-for-state ((seq range) (state :int) (new :int))
;;   state)


;;------------------------------------------------------------
;; Arrays as Sequences
;;
;; This isnt the best example of how to implement a sequence
;; as we use compiler macros here both for optimization and
;; to handle many kinds of array at once.

#+nil
(define-vari-iterable (v-array)
  (vari-iterable-spec
   :length length
   :like make-array-like
   :limit))

#+nil
(v-def-glsl-template-fun
 make-array-like (arr len) "|make-array-like|(~a)" (v-array :int) v-array)

#+nil
(v-define-compiler-macro make-array-like ((arr v-array) (length :int)
                                          &environment env)
  (declare (ignore arr))
  (let* ((arr-type (varjo.api:argument-type 'arr env))
         (elem-type (v-element-type arr-type)))
    `(make-array ,length :element-type ',(type->type-spec elem-type))))

;;------------------------------------------------------------

;; The make-sequence-iterator operator constructs one of
;; these iterator objects: after the required sequence argument,
;; it accepts keyword :start, :end and :from-end arguments,
;; and returns nine values. The first three of those values are
;; an iterator state, a limit and the from-end argument; the re-
;; maining six are functions which, respectively, return a state
;; one step ahead, if possible; test the state against the limit
;; for termination; retrieve the element at the current iteration
;; state from the sequence; set the element at the current iter-
;; ation state to a new value; return the index corresponding
;; to the current iteration state; and return a distinct iteration
;; state representing a copy of the current one.
;; The default method on make-sequence-iterator is in-
;; tended for convenience: for most uses, it is unnecessary to
;; construct the nine return values; instead, the default method
;; (specialized to sequence) on make-sequence-iterator gen-
;; erates the first three of the return values by calling make-
;; simple-sequence-iterator, and returns in addition six
;; protocol generic functions: #’iterator-step (which ad-
;; vances the iteration state); #’iterator-endp, testing an
;; iteration for termination; the #’iterator-element reader
;; and the #’(setf iterator-element) writer; #’iterator-
;; index, returning the sequence index corresponding to the it-
;; erator state; and finally #’iterator-copy, returning a copy
;; of the iteration state. These functions have methods special-
;; ized to list and vector to provide iterators for the built-in
;; sequence classes.
;; While implementors of sequence classes may choose to use
;; this CLOS-based iterator protocol (at the potential loss of
;; efficiency through generic function dispatch at each step),
;; users of the iteration protocol (who define functions which
;; perform iterations over sequences) may not assume that the
;; sequence class implementor has done so, and so must call
;; make-sequence-iterator or the operators discussed below.
;; A small dose of syntactic sugar around make-sequence-
;; iterator is provided by with-sequence-iterator, which
;; binds as if by multiple-value-bind the variables in its
;; first argument to the result of applying make-sequence-
;; iterator to its second argument, and then executes the
;; body. A slightly simpler macro to use is with-sequence-
;; iterator-functions, which binds the six names in its first
;; argument to six local functions (which have dynamic ex-
;; tent and close over the return values from make-sequence-
;; iterator) which perform the various iterator manipula-
;; tions.
