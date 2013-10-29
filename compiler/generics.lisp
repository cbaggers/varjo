(in-package :varjo)

;;types
(defclass v-type () 
  ((core :initform nil :reader core-typep)
   (place :initform nil :reader v-placep)
   (glsl-string :initform "<invalid>" :reader v-glsl-string)
   (glsl-size :initform 1)
   (casts-to :initform nil)))

(defgeneric v-glsl-size (type))
(defgeneric v-casts-to-p (from-type to-type))

;; environment
(defclass environment () 
  ((variables :initform nil :initarg :variables :accessor v-variables)
   (functions :initform nil :initarg :functions :accessor v-functions)
   (macros :initform nil :initarg :macros :accessor v-macros)
   (types :initform nil :initarg :types :accessor v-types)
   (context :initform nil :initarg :context :accessor v-context)))

;; code
(defclass code ()
  ((type :initarg :type :initform nil :accessor code-type)
   (current-line :initarg :current-line :initform nil :accessor current-line)
   (to-block :initarg :to-block :initform nil :accessor to-block)
   (to-top :initarg :to-top :initform nil :accessor to-top)
   (out-vars :initarg :out-vars :initform nil :accessor out-vars)
   (invariant :initarg :invariant :initform nil :accessor invariant)
   (returns :initarg :returns :initform nil :accessor returns)))

(defgeneric merge-obs (objs &key type current-line to-block 
                              to-top out-vars invariant returns))

;; values
(defclass v-value ()
  ((type :initarg :type :initform nil :accessor v-type)
   (inferred-val :initarg :inferred-val :initform nil :accessor v-inferred-val)
   (inferring :initarg :inferring :initform nil :accessor v-inferringpsli)))



;; string generation
(defgeneric v-type->string (x))
