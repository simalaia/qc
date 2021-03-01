(require "scheme-check.scm")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example tests for scheme-check

;; reverse distributes over append
;; with arguments reversed

(define (reverse-append-property l1 l2)
  (equal? (reverse (append l1 l2))
          (append (reverse l2) (reverse l1))))


;; This property is false, and intended to
;; illustrate the finding of counter-examples
(define (reverse-append-bad-property l1 l2)
  (equal? (reverse (append l1 l2))
          (append (reverse l1) (reverse l2))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; showing property combinators

(define (insert el lst)
  (cond
   [(null? lst) (list el)]
   [(< (car lst) el) (cons (car lst)
                           (insert el (cdr lst)))]
   [else (cons el lst)]))

(define (ordered lst)
  (cond
   [(null? lst) #t]
   [(null? (cdr lst)) #t]
   [(<= (car lst) (cadr lst))
    (ordered (cdr lst))]
   [else #f]))

; Use (collect function property) to collect
; information about the test cases

; Use property-if to test conditional properties
(define (insert-keeps-order el lst)
  (collect (length lst)
           (property-if (ordered lst)
                        (ordered (insert el lst)))))

; Use (classify test-predicate "trivial" property) 
; to detect trivial test cases, avoiding a false sense of security
(define (insert-keeps-order-with-trivial el lst)
  (classify (= (length lst) 0) "trivial"
  (property-if (ordered lst)
               (ordered (insert el lst)))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing testable structs

(define-struct point (x y))

;; Use (define-testable-struct struct-name ((field type) ...)) to
;; create custom struct types to be used with Scheme-Check

(define (distance p1 p2)
  (let ([dx (- (point-x p1) (point-x p2))]
        [dy (- (point-y p1) (point-y p2))])
  (sqrt (+ (* dx dx)
           (* dy dy)))))

(define (distance-obeys-triangle-inequality p1 p2 p3)
  (let ([p1p2 (distance p1 p2)]
        [p2p3 (distance p2 p3)]
        [p1p3 (distance p1 p3)])
    (>= (+ p1p2 p2p3) p1p3)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing gen-do syntax

(define natural-arbitrary
  (gen-do (<- i (arbitrary 'integer))
          (gen-return (add1 (abs i)))))

(define natural-coarbitrary
  variant)

(define (natural? v)
  (and (integer? v)
       (> v 0)))

; Some simple natural number properties
(define (natural-closed-under-addition n1 n2)
  (natural? (+ n1 n2)))

(define (natural-addition-distributes-over-sum n1 n2 n3)
  (= (* n1 (+ n2 n3))
     (+ (* n1 n2) (* n1 n3))))

; A false natural number property
(define (natural-closed-under-subtraction n1 n2)
  (natural? (- n1 n2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver

(define (run-tests)
  (let-syntax ((test 
                (lambda (x)
                  (syntax-case x ()
                    [(test prop-name types)
                     (syntax
                      (begin
                        (printf "Testing property ~A:~n" (quote prop-name))
                        (test-property prop-name types)
                        (printf "~n")))]))))
    (with-testable-types ((natural natural-arbitrary
                                   natural-coarbitrary))
      (test reverse-append-property '((list-of integer) (list-of integer)))
      (test reverse-append-bad-property '((list-of integer) (list-of integer)))
      (test insert-keeps-order '(integer (list-of integer)))
      (test insert-keeps-order-with-trivial '(integer (list-of integer)))
      (test natural-closed-under-addition '(natural natural))
      (test natural-addition-distributes-over-sum '(natural natural natural))      
      (test natural-closed-under-subtraction '(natural natural))
      (define-testable-struct point ((x real) (y real)))
      (test distance-obeys-triangle-inequality '(point point point)))))
