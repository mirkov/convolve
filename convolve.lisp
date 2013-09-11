;; Copyright Mirko Vukovic 2013.
;; Distributed under the Boost Software License, Version 1.0.
;; (See accompanying file ./LICENSE_1_0.txt or copy at
;; http://www.boost.org/LICENSE_1_0.txt)

;;;; convolve.lisp

(in-package #:convolve)

;;; "convolve" goes here. Hacks and glory await!


;;; convolve-1 is a lower-level generic function whose methods do the
;;; actual computation.  It uses my MAPGRID function.  MAPGRID was
;;; never tested on matrices, use with caution.
(defgeneric convolve-1 (element-type grid1 grid2)
  (:documentation "Perform convolution of two foreign grids of holding elements of
 ELEMENT-TYPE

grid1 and grid2 are vectors of matrices.
element-type is float or complex"))


(defmethod convolve-1 ((element-type (eql 'float)) grid1 grid2)
  (let* ((*default-grid-type* 'foreign-array)
	 (float-type (second (specification grid1)))
	 (*default-element-type* float-type)
	 (dft-product
	  (let ((convol-dft
		 (let ((grid2-dft (unpack (gsll::forward-fourier-transform
					   (copy grid2))
					  :unpack-type 'complex))
		       (grid1-dft (unpack (gsll:forward-fourier-transform
					   (copy grid1))
					  :unpack-type 'complex)))
		   (let ((grid:*default-element-type* '(complex double-float)))
		     (mapgrid #'* grid2-dft grid1-dft)
		     #+can-pass-struct-in-cffi(gsll:axpy 1d0 grid1-dft grid2-dft)))))
		  (gsll:inverse-fourier-transform
		   (grid:copy convol-dft) :half-complex t)))
	 (convolution
	  (mapgrid #'realpart dft-product)))
    convolution))

(defmethod convolve-1 ((element-type (eql 'complex)) grid1 grid2)
  (let* ((*default-grid-type* 'foreign-array)
	 (*default-element-type* '(complex double-float))
	 (dft-product
	  (let* ((convol-dft
		  (let ((grid2-dft (gsll::forward-fourier-transform grid2))
			(grid1-dft (gsll::forward-fourier-transform grid1)))
		    (mapgrid #'* grid2-dft grid1-dft)
		    #+can-pass-struct-in-cffi(gsll:axpy 1d0 grid1-dft grid2-dft))))
	    (inverse-fourier-transform convol-dft))))
    dft-product))



;;; Convolve is the user interface to the convolution facilities
(defgeneric convolve (grid1 grid2)
  (:documentation
"Calculate the convolution of grid1 to grid2 using forward and inverse
FFT

Supported and tested grid types are:
- floats: vector-single-float, vector-double-float
- complex: vector-complex-single-float, vector-complex-double-float

The code is expected to work with matrics, although this has not been
tested"))

(defmethod convolve ((vector1 vector-double-float) (vector2 vector-double-float))
  (destructuring-bind ((array-type dimension1) content-type)
      (specification vector1)
    (declare (ignore array-type content-type))
    (destructuring-bind ((array-type dimension2) content-type)
	(specification vector2)
      (declare (ignore array-type content-type))
      (assert (= dimension1 dimension2) ()
	      (error "Vector dimensions, ~a, ~a are not equal" dimension1 dimension2))
      (convolve-1 'float vector1 vector2))))

(defmethod convolve ((vector1 vector-single-float) (vector2 vector-single-float))
  (destructuring-bind ((array-type dimension1) content-type)
      (specification vector1)
    (declare (ignore array-type content-type))
    (destructuring-bind ((array-type dimension2) content-type)
	(specification vector2)
      (declare (ignore array-type content-type))
      (assert (= dimension1 dimension2) ()
	      (error "Vector dimensions, ~a, ~a are not equal" dimension1 dimension2))
      (convolve-1 'float vector1 vector2))))

(defmethod convolve ((vector1 vector-complex-double-float)
		     (vector2 vector-complex-double-float))
  (destructuring-bind ((array-type dimension1) content-type)
      (specification vector1)
    (declare (ignore array-type content-type))
    (destructuring-bind ((array-type dimension2) content-type)
	(specification vector2)
      (declare (ignore array-type content-type))
      (assert (= dimension1 dimension2) ()
	      (error "Vector dimensions, ~a, ~a are not equal" dimension1 dimension2))
      (convolve-1 'complex vector1 vector2))))

(defmethod convolve ((vector1 vector-complex-single-float)
		     (vector2 vector-complex-single-float))
  (destructuring-bind ((array-type dimension1) content-type)
      (specification vector1)
    (declare (ignore array-type content-type))
    (destructuring-bind ((array-type dimension2) content-type)
	(specification vector2)
      (declare (ignore array-type content-type))
      (assert (= dimension1 dimension2) ()
	      (error "Vector dimensions, ~a, ~a are not equal" dimension1 dimension2))
      (convolve-1 'complex vector1 vector2))))
