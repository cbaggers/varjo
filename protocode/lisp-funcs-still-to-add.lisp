;;
eq
eql
equal
equalp

;;
svref
row-major-aref
array-dimension
array-rank
array-row-major-index
array-total-size
adjustable-array-p
array-has-fill-pointer-p
array-in-bounds-p
arrayp
simple-vector-p - always nil
vectorp - if it's 1d then yup

;;
;; Data and Control Flow
values
every
notany
notevery
some
compiled-function-p
functionp

;;
;; Numbers
1+
1-
boole
dpb
ldb
gcd
ash
fceiling
ffloor
floor
fround
ftruncate
truncate
rem
decode-float
float-digits
float-precision
float-radix
float-sign
integer-decode-float
scale-float
logand
logandc1
logandc2
logcount
logeqv
logior
lognand
lognor
lognot
logorc1
logorc2
logtest
logxor
isqrt ;; floor(sqrt(x))
signum
ceiling
complex
ldb-test
deposit-field
mask-field
phase
random-state-p
complexp
evenp
floatp
integerp
logbitp
minusp
numberp
oddp
plusp
rationalp
realp
zerop

;;
;; Sequences - will only have to work on arrays in varjo
copy-seq
make-sequence ;; eh
subseq ;; will be ugly
concatenate  ;; will be ugly
elt
reduce
length ;; how do we resolve with glsl length?
mismatch ;; eh
count
count-if
count-if-not

;;
;; Structures
copy-structure

;;
;; Types and Classes
coerce


;;
;; Evaluation & Compilation
constantp
special-operator-p

;;------

;;
;; Data Flow & Controlo
fboundp

;;
;; Numbers
random ;; harder to match the CL behaviour

;;
;; Sequences
nsubstitute
nsubstitute-if
nsubstitute-if-not
map-into
fill ;; could make as a macro

;;------

case
declaim
destructuring-bind ;; could make it for vectors
do
do*
dotimes
loop
loop-finish
multiple-value-setq
nth-value
prog2
typecase

;;------

quote ;; opens up a world of crazy :p symbols at compile time

;;------
