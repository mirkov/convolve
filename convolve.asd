;; Copyright Mirko Vukovic 2013.
;; Distributed under the Boost Software License, Version 1.0.
;; (See accompanying file ./LICENSE_1_0.txt or copy at
;; http://www.boost.org/LICENSE_1_0.txt)

;;;; convolve.asd

(asdf:defsystem #:convolve
  :serial t
  :description "API for 1D convolution of foreign vectors"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :license "Boost Software License"
  :depends-on (#:lisp-unit
               #:antik
               #:gsll
	       #:map-grid-utils)
  :components ((:file "convolve-package-def")
               (:file "convolve")))

