;;;
;;; Quick Plot
;;;
;;; Copyright 2014 Benjamin Silbaugh
;;;
;;; See LICENSE file for redistribution and modification permissions.

(module quick-plot *

(import scheme chicken)
(use abscissa)

;; Samples a function f at points x-vals
(define (sample-function f x-vals)
  (map (lambda (x) (cons x (f x))) x-vals))

;; helper function for building various style xy-plots
(define (xy-plot style-seq xy-pairs)
  (define (labels xy-pairs i)
	(if (null? xy-pairs)
		'()
		(cons (string-append "case " (number->string i))
			  (labels (cdr xy-pairs) (+ i 1)))))
  (window
   ((meta-figure show-legend: #t)
	(apply (meta-cartesian major-grid: '--)
		   (map (lambda (style label xy)
				  (style ((<-meta label: label) xy)))
				style-seq (labels xy-pairs 1) xy-pairs)))))

;; Useful for generating quick-n-dirty line plots of xy data
(define (xy-line-plot #!rest xy-pairs)
  (xy-plot (circular-list 
			(meta-lines color: *red*)
			(meta-lines color: *blue*)
			(meta-lines color: *green*)
			(meta-lines color: *cyan*)
			(meta-lines color: *magenta*))
		   xy-pairs))

;; Useful for generating quick-n-dirty scatter plots of xy data
(define (xy-scatter-plot #!rest xy-pairs)
  (xy-plot (circular-list
			(meta-points style: 'o color: *red*)
			(meta-points style: '+ color: *blue*)
			(meta-points style: 'x color: *green*)
			(meta-points style: 's color: *cyan*)
			(meta-points style: 't color: *magenta*)
			(meta-points style: 'd color: *yellow*))
		   xy-pairs))

); module
