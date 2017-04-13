(in-package :varjo)

(defclass draw-mode () ())

(defclass points (draw-mode) ())
(defclass lines (draw-mode) ())
(defclass line-loop (draw-mode) ())
(defclass line-strip (draw-mode) ())
(defclass lines-adjacency (draw-mode) ())
(defclass line-strip-adjacency (draw-mode) ())
(defclass triangles (draw-mode) ())
(defclass triangle-fan (draw-mode) ())
(defclass triangle-strip (draw-mode) ())
(defclass triangles-adjacency (draw-mode) ())
(defclass triangle-strip-adjacency (draw-mode) ())
(defclass quads (draw-mode) ())
(defclass patches (draw-mode) ())

;;------------------------------------------------------------



;;------------------------------------------------------------

(defclass geometry-primitive-in () ())

(defclass points (geometry-primitive-in) ())
(defclass lines (geometry-primitive-in) ())
(defclass lines-adjacency (geometry-primitive-in) ())
(defclass triangles (geometry-primitive-in) ())
(defclass triangles-adjacency (geometry-primitive-in) ())


(defclass geometry-primitive-out () ())

(defclass points (geometry-primitive-out) ())
(defclass line-strip (geometry-primitive-out) ())
(defclass triangle-strip (geometry-primitive-out) ())


;;------------------------------------------------------------

;; Tasks

;; I think if we edit #'process-in-args & return we could have
;; instance names on the interface blocks. Not sure how to have
;; arrayed interface blocks though, they are syntactically like structs
;;

;; on geom stages, emit the following
;;
;; layout(points) in;
;; layout(line_strip, max_vertices = 2) out;

;; Fail if we find a tesselation stage when the version is below 4

;; Make stages proper types, make a multistage type which is not allowed in
;; rolling-translate. CEPL will use this for testing.
