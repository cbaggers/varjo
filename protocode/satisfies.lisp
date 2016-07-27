;; want to make it possible to make adapters for types.

(defstruct-g test
  (pos :vec3)
  (color :vec3))

(defstruct-g foo
  (position :vec3)
  (normal :vec3)
  (color :vec3))

;; clearly foo has enough data to satisfy any function that would take a test
;; but the compiler doesnt know this.

(satisfies test foo
  (pos position)
  (color color))

;; The above tells the compiler how to cast one to the other.

;; The question then is if there is a performace overhead to this.
