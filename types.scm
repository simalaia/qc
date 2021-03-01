#! /usr/bin/env chibi-scheme

(import (chibi)
	(srfi 27)
	(scheme case-lambda)
	)

(load "gen.scm")
(load "shrink.scm")
(load "helpers.scm")


(define bin (gen
	(lambda () (= (nat-upto 2) 0) )
	(lambda (_) #f )) )

(define byte (gen
	(lambda () (nat-upto 256) )
	half) )

(define pascii (gen
	(lambda () (integer->char (between #x20 #x7E)) )
	(lambda (p) (integer->char (step-to (char->integer p) #x20)) )) )

(define ascii (gen
	(lambda () (integer->char (nat-upto #x80)) )
	(lambda (c) (integer->char (step (char->integer c))) )) )

(define char (gen
	(lambda () (integer->char (nat-upto #x110000)) )
	(lambda (c) (integer->char (step (char->integer c))) )) )

(define uint (gen
	(lambda () (nat-upto #x100000000))
	half) )

(define int (gen
	(lambda () (sample #x100000000))
	half) )

(define float (gen
	(lambda () (+ (sample #x10000000000000000) (frac)))
	hfrac) )
