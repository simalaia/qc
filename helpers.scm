#! /usr/bin/env chibi-scheme

(import (chibi)
	(only (scheme base) symbol=?) (scheme case-lambda)
	)

(define (gen p s)
 (lambda (o . a)
 	(cond
 		((symbol=? o 'pick) (apply p a) )
 		((symbol=? o 'shrink) (apply s a) )
 		(else (error "invalid command") )) ))
