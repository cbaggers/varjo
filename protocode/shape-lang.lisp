;; Generative geometry, from simple primitives
;; see topology for torus

(construst
 +-+10
 +-+10)

1-+
- -
+-1

[]10  -> [][][][][][][][][][]

0-1-n
- -
1-n
-
n
;; ^^^ to hard to write fast

;;------------------------------
;; Ok onto something here,
;; feels like comfortable way of writing topology diagrams

;;point
0

;;line segment
01

;;quad
01
23

;;latice
01*
23*
***

;;triangle
0120

;;triangle tube
01230
01230

;;square tube, n points long
01230
*****
;;tube with n points around and along
0*0
***

;;tube with n points around and 1 segment long
0*0
1*1

;;'circle with n points
0*0

023450
167891

;;sphere? ... nope
000
0*0
000

;;hmm or is it.. nope
0*0
**
0

;; ah well ... we also permitted to use other symbols

;; cylinder n points around and m points long
0n0
mmm

;; Assuming unit vector length, and avoiding degenerate or intersecting forms
;; calculate points in shape.

;;Text being a 2d medium assume 3 dimensions max for now

;;--------------------------------------------------
;; steal their diagrams?

ab
cd

a>a
^ v
a>a

a>b
b<a

a>b
v ^
c<a

aa
^^
bb

;; nah..so hard to write quickly



;;--------------------------------------------------
;; What next

;; Next read spring physics (gaffer on games) and general topology stuff.

;; The trick for calculating in one step rather than resolving over time will be
;; having a 'thing' to base each step on. So each new line in the language
;; above (not the topology one but mine instead) assumes a default progression
;; down a unit vector; each line is displaced by the vector in the y direction.
;;
;; the vector really is a transform, it could just as easily be a rotation. so:
;;
;; 0*0 is a circle
;;
;; -but-
;;
;; 0*0
;; ***
;;
;; is a cylinder, if the transform is translate,
;; or a sphere, if the transform is a rotation.
;;
;; YAY!
;;
;; Making the faces then is interesting but we do it by ensuring
;; that the normal always faces away from the origin of the transform.
