(defpackage #:varjo-tests
  (:use #:cl #:varjo #:varjo-lang #:rtg-math #:rtg-math.base-maths #:stefil)
  (:import-from :varjo
		:defshader)
  (:export :test-all))
