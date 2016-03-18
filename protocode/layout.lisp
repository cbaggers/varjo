(in-package :varjo)

This is only relevent for 4.1 and up.. so no use for now
look at interface blocks instead

;; --- input location ---
;; allowed on:
;; - input variable declarations
;; - input block declarations
;; - input block member declarations
;;
;; The following have an additional level of arrayness relative to other
;; shader inputs and outputs. This outer array level is removed from the
;; type before considering how many locations the type consumes.
;; - geometry (inputs & outputs)
;; - tessellation-control (inputs & outputs)
;; - tessellation-evaluation (inputs)

;; Vertex Shader Size rules:
;; scalar 1
;; vector 1

;; Other Shader Size rules:
;; scalar 1
;; vector (not dvec3 or dvec4) 1
;; dvec3 2
;; dvec4 2

;; Array Size (after potentially removing an outer array level)
;; (* array-length element-size)

;; Matrix Size
;; n x m matrix is same as array of:
;; - length n
;; - vectors with m components

;; If the declared input is a structure or block, its members will be
;; assigned consecutive locations in their order of declaration

;; For a block, this process applies to the entire block, or until the
;; first member is reached that has a location layout qualifier. When a
;; block member is declared with a location qualifier, its location comes
;; from that qualifier

;; When a block member is declared with a location qualifier, its
;; location comes from that qualifier: The member's location qualifier
;; overrides the block-level declaration

;; If a block has no block-level location layout qualifier, it is
;; required that either all or none of its members have a location layout
;; qualifier, or a compile-time error results.

;; For the purposes of determining if a non-vertex input matches an
;; output from a previous shader stage, the location layout qualifier (if
;; any) must match.


;; --- output ---

;; The usage and rules for using the component qualifier, and applying
;; location qualifier to blocks and structures, are exactly as described
;; in section 4.4.1 "Input Layout Qualifiers"

;; For fragment-shader outputs, the location and index specify the color
;; output number and index receiving the values of the output. For
;; outputs of all other shader stages, the location specifies a vector
;; number that can be used to match against inputs in a subsequent shader
;; stage, even if that shader is in a different program object.

;; Location layout qualifiers may be used on output variables declared as
;; structures, but not on individual members. Location layout qualifiers
;; may not be used on output blocks or output block members.


;; --- interface blocks ---

;; It is a compile-time error to have an input block in a vertex shader
;; or an output block in a fragment shader
