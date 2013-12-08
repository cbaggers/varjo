(in-package :varjo)

;; THis will eventually be where the compiler tests live, but I dont have
;; time for that right now. So thsi will be sparse for a while

;; this currently fails
(defpipline test ((a :int) (b (:float 3)) &context :330)
  (:vertex (sin a)))
