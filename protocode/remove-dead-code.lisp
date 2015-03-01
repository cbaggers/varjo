;; one quick optimization we can add it to have every function specify if they
;; are pure..then any pure form not in the tail position of a progn gets
;; stripped
