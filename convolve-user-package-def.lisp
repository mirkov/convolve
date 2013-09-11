;; Copyright Mirko Vukovic 2013.
;; Distributed under the Boost Software License, Version 1.0.
;; (See accompanying file ./LICENSE_1_0.txt or copy at
;; http://www.boost.org/LICENSE_1_0.txt)

;;;; convolve-user-package-def.lisp

(defpackage #:convolve-user
  (:use #:cl #:convolve #:mv-gnuplot
	#:map-grid-utils #:sequence-grid)
  (:documentation
"Package for documenting the usage of the CONVOLVE package"))

