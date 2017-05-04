(uiop:define-package #:varjo.import
    (:use #:cl :varjo :optima :named-readtables
          :alexandria :glsl-toolkit)
  (:import-from :varjo :dbind)
  (:export :import-stage
           :import-glsl-function))
