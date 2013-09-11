;; Copyright Mirko Vukovic 2013.
;; Distributed under the Boost Software License, Version 1.0.
;; (See accompanying file ./LICENSE_1_0.txt or copy at
;; http://www.boost.org/LICENSE_1_0.txt)

;;;; convolve-package-def.lisp

(defpackage #:convolve
  (:use #:cl #:gsll #:grid #:map-grid-utils)
  (:export :convolve
	   :convolve-1)
  (:documentation
"Package for calculating convolution of foreign vector"))

