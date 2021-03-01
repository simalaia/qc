(import (chibi) (chibi match)
	(lib misc) (lib test)
	(srfi 27)
	(only (scheme base) symbol=?)
	)

;; Quick check implementation attempt


;; Object generators
;; Numbers
(define (between n m) (assert (and (number? n) (number? m)))
	(+ n (random-integer (- m n))) )

(define (pick-bounded n m) (lambda () (between n m)) )

(define sint32 (pick-bounded (- (/ (expt 2 32) 2)) (- (/ (expt 2 32)) 1)))
(define uint32 (pick-bounded 0 (- (expt 2 32) 1)))

#|
(define (random-rational n m s)
	(let ( (s (expt 10 s)) )
		(+ (between n m) (/ (floor (* (random-real) s)) s)) ))
|#


;; Booleans
(define (random-bool) (= (random-integer 2) 0) )


;; Characters
(define (random-char . t)
	(match t
		(('ascii) (integer->char (random-integer 128)) )
		(('print) (integer->char (between #x20 #x7E)) )
		((or ('char) ()) (integer->char (random-integer #x110000)) )
		(else (error "random-char hates you") )) )


;; Strings
(define (random-string n . t) (assert (number? n))
	(letrec* (
			(C (lambda (c) (list->string `(,c))))
			(gen (lambda (f n s)
				(if (= n 0) s (gen f (- n 1) (string-append (f) s))))) )
		(match t
			(('ascii)         (gen (lambda () (C (random-char 'ascii))) n "") )
			(('print)         (gen (lambda () (C (random-char 'print))) n "") )
			((or ('char) ())  (gen (lambda () (C (random-char 'char ))) n "") )
			(else (error "random-string hates you") )) ))


;; Items
(define (pick l) (assert (list? l))
	(list-ref l (random-integer (length l))) )


;; Lists
(define (random-list f m)  ;; Creates O(nⁿ) structures
	(let lop ( (m (random-integer (+ m 1))) (r '()) )
		(cond
			((= m 0) r )
			(else (lop (- m 1) (cons (f) r)) )) ))


;; Testing, builds a sexp
(define (random-sexp)
	(let ( (opt (pick `(int str bin lst))) )
		(cond
			((symbol=? `int opt) (uint32) )
			((symbol=? `str opt) (random-string 4 `ascii) )
			((symbol=? `bin opt) (random-bool) )
			((symbol=? `lst opt) (random-list random-sexp 10) )
			(else (error "random-sexp hates you") )) ))


;; Starting to look into a full qc setup
;; I should read the paper
(define (find-closest-to-zero a b) (if (< (abs a) (abs b)) a b))

(define (shrink-bounded min max)
	(let ( (bound-limit (find-closest-to-zero min max)) )
		(lambda (previous)
			(cond
				((or (= previous 0) (= previous bound-limit)) previous)
				((> previous 0) (floor (/ previous 2)) )
				(else (ceiling (/ previous 2)) )) ) ))

(define (pick-uniform size)
	(let ( (v (/ size 2)) )
		(between (- v size) v) ))

(define (shrink p)
	(cond
		((= p 0) p )
		((> p 0) (floor (/ p 2)) )
		(else (ceiling (/ p 2)) )) )


