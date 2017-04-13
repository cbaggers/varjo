(in-package :varjo)





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
