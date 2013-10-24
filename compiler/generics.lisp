(in-package :varjo)

(defgeneric v-nativep (x))
(defgeneric v-arrayp (x))
(defgeneric v-typep (x))

(defclass varjo-type () 
  ((principle :initform nil :initarg :principle :accessor principle)
   (array-length :initform nil :initarg :array-length :accessor array-length)
   (place :initform nil :initarg :place :accessor v-place)
   (gl-name :initform nil :initarg :gl-name :accessor gl-name)))

(defclass code ()
  ((type-spec :initarg :type :initform nil :accessor code-type)
   (current-line :initarg :current-line :initform nil :accessor current-line)
   (to-block :initarg :to-block :initform nil :accessor to-block)
   (to-top :initarg :to-top :initform nil :accessor to-top)
   (out-vars :initarg :out-vars :initform nil :accessor out-vars)
   (invariant :initarg :invariant :initform nil :accessor invariant)
   (returns :initarg :returns :initform nil :accessor returns)))

