;; Copyright Mirko Vukovic 2013.
;; Distributed under the Boost Software License, Version 1.0.
;; (See accompanying file ./LICENSE_1_0.txt or copy at
;; http://www.boost.org/LICENSE_1_0.txt)

;;;; convolve.asd

(asdf:defsystem #:convolve-user
  :serial t
  :description "Examples of use of the convolve library"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :license "Boost Software License"
  :depends-on (#:lisp-unit
               #:antik
               #:gsll
	       #:savitzky-golay
	       #:mv-gnuplot
	       #:sequence-grid
	       #:map-grid-utils)
  :components ((:file "convolve-user-package-def")
               (:file "convolve-user")))

