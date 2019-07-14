(in-package :varjo.internals)

;;----------------------------------------------------------------------

(define-vari-type v-single-complex () v-vec2)
(define-vari-type v-double-complex () v-dvec2)
(define-alternate-type-name v-single-complex single-complex)
(define-alternate-type-name v-double-complex double-complex)
(define-alternate-type-name v-single-complex complex)

;; for backwards compatibility
(define-alternate-type-name v-single-complex v-complex)

(define-vari-type v-ratio () v-ivec2)
(define-alternate-type-name v-ratio ratio)

;;----------------------------------------------------------------------
