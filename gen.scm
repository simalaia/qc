#! /usr/bin/env chibi-scheme

(import (chibi)
	(srfi 27)
	(scheme case-lambda)
	)

(define nat-upto random-integer)
(define frac random-real)
(define (between b t) (+ b (nat-upto (- (+ t 1) b))) )
(define (sample s) (let ( (v (/ s 2)) ) (between (- v s) v)) )

(define pick
	(case-lambda
		((l) (list-ref l (nat-upto (length l))) )
		(l (pick l) )) )


