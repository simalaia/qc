#! /usr/bin/env chibi-scheme

(import (chibi)
	(srfi 27)
	(scheme case-lambda)
	)

(load "gen.scm")

(define (shrink f) (lambda (n) (if (= n 0) n (f n)) ))

(define half (shrink (lambda (n) ((pick floor ceiling) (/ n 2)) )) )
(define (half-to n b) (+ b (half (- n b))) )

(define step (shrink (lambda (n) (- n 1) )) )
(define (step-to n b) (+ b (step (- n b))) )

(define hfrac (shrink (lambda (n) (/ n 2) )) )
(define (hfrac-to n b) (+ b (hfrac (- n b))) )

