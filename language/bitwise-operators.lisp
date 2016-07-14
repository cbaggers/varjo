(in-package :varjo-lang)

(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uint varjo:v-uint) varjo:v-uint)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-int varjo:v-int) varjo:v-int)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uvec2 varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uvec3 varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uvec4 varjo:v-uvec4) varjo:v-uvec4)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-ivec2 varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-ivec3 varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-ivec4 varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uint varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uint varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uint varjo:v-uvec4) varjo:v-uvec4)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-int varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-int varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-int varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uvec2 varjo:v-uint) varjo:v-uvec2)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uvec3 varjo:v-uint) varjo:v-uvec3)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-uvec4 varjo:v-uint) varjo:v-uvec4)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-ivec2 varjo:v-int) varjo:v-ivec2)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-ivec3 varjo:v-int) varjo:v-ivec3)
(varjo::v-defun << (x y) "(~a << ~a)" (varjo:v-ivec4 varjo:v-int) varjo:v-ivec4)

(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uint varjo:v-uint) varjo:v-uint)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-int varjo:v-int) varjo:v-int)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uvec2 varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uvec3 varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uvec4 varjo:v-uvec4) varjo:v-uvec4)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-ivec2 varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-ivec3 varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-ivec4 varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uint varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uint varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uint varjo:v-uvec4) varjo:v-uvec4)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-int varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-int varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-int varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uvec2 varjo:v-uint) varjo:v-uvec2)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uvec3 varjo:v-uint) varjo:v-uvec3)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-uvec4 varjo:v-uint) varjo:v-uvec4)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-ivec2 varjo:v-int) varjo:v-ivec2)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-ivec3 varjo:v-int) varjo:v-ivec3)
(varjo::v-defun >> (x y) "(~a >> ~a)" (varjo:v-ivec4 varjo:v-int) varjo:v-ivec4)


(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-int varjo:v-int) varjo:v-int)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-int varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-int varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-int varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-ivec2 varjo:v-int) varjo:v-ivec2)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-ivec2 varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-ivec3 varjo:v-int) varjo:v-ivec3)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-ivec3 varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-ivec4 varjo:v-int) varjo:v-ivec4)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-ivec4 varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uint varjo:v-uint) varjo:v-uint)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uint varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uint varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uint varjo:v-uvec4) varjo:v-uvec4)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uvec2 varjo:v-uint) varjo:v-uvec2)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uvec2 varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uvec3 varjo:v-uint) varjo:v-uvec3)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uvec3 varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uvec4 varjo:v-uint) varjo:v-uvec4)
(varjo::v-defun bit-and (x y) "(~a & ~a)" (varjo:v-uvec4 varjo:v-uvec4) varjo:v-uvec4)

(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-int varjo:v-int) varjo:v-int)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-int varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-int varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-int varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-ivec2 varjo:v-int) varjo:v-ivec2)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-ivec2 varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-ivec3 varjo:v-int) varjo:v-ivec3)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-ivec3 varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-ivec4 varjo:v-int) varjo:v-ivec4)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-ivec4 varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uint varjo:v-uint) varjo:v-uint)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uint varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uint varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uint varjo:v-uvec4) varjo:v-uvec4)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uvec2 varjo:v-uint) varjo:v-uvec2)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uvec2 varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uvec3 varjo:v-uint) varjo:v-uvec3)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uvec3 varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uvec4 varjo:v-uint) varjo:v-uvec4)
(varjo::v-defun bit-ior (x y) "(~a | ~a)" (varjo:v-uvec4 varjo:v-uvec4) varjo:v-uvec4)

(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-int varjo:v-int) varjo:v-int)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-int varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-int varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-int varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-ivec2 varjo:v-int) varjo:v-ivec2)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-ivec2 varjo:v-ivec2) varjo:v-ivec2)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-ivec3 varjo:v-int) varjo:v-ivec3)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-ivec3 varjo:v-ivec3) varjo:v-ivec3)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-ivec4 varjo:v-int) varjo:v-ivec4)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-ivec4 varjo:v-ivec4) varjo:v-ivec4)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uint varjo:v-uint) varjo:v-uint)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uint varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uint varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uint varjo:v-uvec4) varjo:v-uvec4)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uvec2 varjo:v-uint) varjo:v-uvec2)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uvec2 varjo:v-uvec2) varjo:v-uvec2)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uvec3 varjo:v-uint) varjo:v-uvec3)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uvec3 varjo:v-uvec3) varjo:v-uvec3)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uvec4 varjo:v-uint) varjo:v-uvec4)
(varjo::v-defun bit-xor (x y) "(~a ^ ~a)" (varjo:v-uvec4 varjo:v-uvec4) varjo:v-uvec4)
