;; Copyright Mirko Vukovic 2013.
;; Distributed under the Boost Software License, Version 1.0.
;; (See accompanying file ./LICENSE_1_0.txt or copy at
;; http://www.boost.org/LICENSE_1_0.txt)

;;;; convolve-user.lisp

(in-package :convolve-user)

(defun make-test-sequence (&key
			     (length 1000)
			     (A 8.0)
			     (noise-sigma 0.5))
  "Return a waveform of gaussian noise superimposed on a sequence of six
gaussians of same amplitude and varying widths "
  (let* ((grid:*default-grid-type* 'grid:foreign-array)
	 (grid:*default-element-type* 'double-float)
	 (index (seq-grid:progression :linear 0d0 999d0 :count length))
	 (waveform
	  (let ((rng (gsll:make-random-number-generator gsll:+mt19937+ 0))
		(m '(410.0 660.0 775.0 835.0 890.0 925.0))
		(sigma '(80.0 40.0 20.0 10.0 5.0 2.5)))
	    (mapgrid (lambda (index)
		       (+ (reduce #'+
				  (mapcar (lambda (m sigma)
					    (* A (exp (- (expt (/ (- index m)
								  sigma)
							       2)))))
					  m sigma ))
			  (gsll:sample rng :gaussian :sigma noise-sigma)))
		     index))))
    (values waveform index)))



(defun new-gnuplot-window ()
  (gpi:init-gnuplot)
  (gpi:start-gnuplot)
  (gpi:hello-world))



(defun plot-data+noise ()
  (let* ((grid:*default-grid-type* 'grid:foreign-array)
	 (grid:*default-element-type* 'double-float))
    (multiple-value-bind (data index)
	(make-test-sequence :noise-sigma 0.001)
      (plot-xys index
		`((,data :thick 2 :title "Data + Noise")
		  (,(make-test-sequence)
		    :with :dots :title "Original"))))))


(defun plot-filter-length-effects ()
  "Plot test curve with two filter lengths

The Plot shows the effect of using a wider filter smoothing down wider peaks"
  (let* ((grid:*default-grid-type* 'grid:foreign-array)
	 (grid:*default-element-type* 'double-float)
	 (filter1 (sg:convolution-vector 1000 8 8 2))
	 (filter2 (sg:convolution-vector 1000 16 16 2))
	 (filter3 (sg:convolution-vector 1000 16 16 4)))
    (multiple-value-bind (data index)
	(make-test-sequence :noise-sigma 0.001)
      (let ((convolution1 (convolve data filter1))
	    (convolution2 (convolve data filter2))
	    (convolution3 (convolve data filter3)))
	(set-to ((xrange '(700 nil))
		 (title "Filter length and order effects"))
	(plot-xys index
		  `((,data :thick 2 :title "Smooth")
		    (,convolution1 :title "8+8/order 2")
		    (,convolution2 :title "16+16/order 2")
		    (,convolution3 :title "16+16/order 4"))))))))








