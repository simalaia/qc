#! /usr/bin/env -S chibi-scheme -r -A ./ -I ${modules}/schemeR7RS

(import (chibi) (chibi match) (chibi io) (chibi time)
	(lib misc)
	)

(load "gen.scm")
(load "shrink.scm")
(load "helpers.scm")
(load "types.scm")

(define (char->string c) (list->string `(,c)) )

(define (dostrings g l)
	(let lop ( (r "") (l l) )
		(if (< 0 l) (lop (string-append (char->string (g 'pick)) r) (- l 1)) r) ))

(define (rstrings g l) (dostrings g (nat-upto l)) )
(define (bstrings g b t) (dostrings g (between b t)) )

(define (mktimer)
	(let ( (c (current-seconds)) )
		(lambda () (- (current-seconds) c) ) ))

(define (main args)
	(let ( (s (bstrings pascii 30 50)) (k (mktimer)) )
		(dspl "---" s "---")
		(if (string=? s (read-line))
			(dspl "correct: "   (string-length s) " in " (k) " seconds")
			(dspl "incorrect: " (string-length s) " in " (k) " seconds") ))
	(dsp ""))

