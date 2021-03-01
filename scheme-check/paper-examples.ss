(require "scheme-check.scm")
(require (lib "list.ss"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preliminaries

(define (liftm2 f a b)
  (gen-do (<- aa a)
          (<- bb b)
          (gen-return (f aa bb))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Defining Properties

;; (printf "2. Defining Properties~n")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.1 A Simple Example

;; (printf "  2.1 A Simple Example~n")

(define (reverse-unit x)
  (equal? (list x)
          (reverse (list x))))

(test-property reverse-unit
               '(integer))

(define (reverse-append xs ys)
  (equal? (reverse (append xs ys))
          (append (reverse ys) (reverse xs))))

(test-property reverse-append
               '((list-of integer)
                 (list-of integer)))

(define (reverse-reverse xs)
  (equal? xs
          (reverse (reverse xs))))

(test-property reverse-reverse
               '((list-of integer)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2 Functions

;; (printf "  2.2 Functions~n")

(define (extensionally-equal? f1 f2 x)
  (equal? (f1 x)
          (f2 x)))

(define (compose g f)
  (lambda (n)
    (g (f n))))

(define (composition-associative f g h v)
  (extensionally-equal? (compose f
                                 (compose g h))
                        (compose (compose f g)
                                 h)
                        v))

(test-property composition-associative
               '((function integer integer)
                 (function integer integer)
                 (function integer integer)
                 integer))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3 Conditional Laws

;; (printf "  2.3 Conditional Laws~n")

(define (max-less-equal x y)
  (property-if (<= x y)
               (= (max x y) y)))

(test-property max-less-equal '(integer integer))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.4 Monitoring Test Data

;; (printf "  2.4 Monitoring Test Data~n")

(define (ordered? xs)
  (equal? xs (mergesort xs <)))

(define (insert el lst)
  (cond
   [(null? lst) (list el)]
   [(<= el (car lst)) (cons el lst)]
   [else (cons (car lst)
               (insert el (cdr lst)))]))

(define (property-insert x xs)
  (property-if (ordered? xs)
               (classify (null? xs) "trivial"
                         (ordered? (insert x xs)))))

(test-property property-insert
               '(integer
                 (list-of integer)))

(define (property-insert-2 x xs)
  (property-if (ordered? xs)
               (collect (length xs)
                         (ordered? (insert x xs)))))

(test-property property-insert-2
               '(integer
                 (list-of integer)))

(define (accum+ lst)
  (define (scan this rest)
    (cond
     [(null? rest) (list this)]
     [else (cons this
                 (scan (+ this (car rest))
                       (cdr rest)))]))
  (cond
   [(null? lst) ()]
   [else (scan (car lst) (cdr lst))]))

(define ordered-list-generator
  (gen-fmap accum+
            (gen-fmap (lambda (l)
                        (map abs l))
                      (arbitrary '(list-of integer)))))

(test-property-with-generators property-insert
                               (list (arbitrary 'integer)
                                     ordered-list-generator))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.5 Infinite Structures

;; Not sure how to handle these yet :(

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Defining Generators

;; (printf "3. Defining Generators~n")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.1 Arbitrary

;; (printf "  3.1 Arbitrary~n")

(define no-coarb
  (lambda (_)
    (printf "No coarb just yet")))

(define arb-int
  (choose -20 20))
(install-testable-type! 'int arb-int no-coarb)

;;(display (generate-single (arbitrary '(list-of int))))
;;(newline)

(define (arb-pair t1 t2)
  (liftm2 cons (arbitrary t1) (arbitrary t2)))

(install-testable-type! '(pair ? ?) 
                        (lambda (t1 t2)
                          (liftm2 cons 
                                  (arbitrary t1)
                                  (arbitrary t2)))
                        no-coarb)

;;(display (generate-single 
;;          (arbitrary 
;;           '(list-of (pair (pair integer integer) real)))))
;;(newline)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.2 Generators for User-Defined Types

(define red 'red)
(define blue 'blue)
(define green 'green)

(install-testable-type! 'color
                        (oneof (gen-return red)
                               (gen-return green)
                               (gen-return blue))
                        no-coarb)

;;(display (generate-single (arbitrary '(list-of color))))
;;(newline)

(install-testable-type! '(my-list-of ?)
                        (lambda (type)
                          (oneof (gen-return ())
                                 (liftm2 cons 
                                         (arbitrary type)
                                         (arbitrary `(my-list-of ,type)))))
                        no-coarb)
;;(display (generate-single (arbitrary '(my-list-of integer))))
;;(newline)

(install-testable-type! '(my-long-list-of ?)
                        (lambda (type)
                          (frequency 
                           (1 (gen-return ()))
                           (4 (liftm2 
                               cons
                               (arbitrary type)
                               (arbitrary `(my-long-list-of ,type))))))
                        no-coarb)
;;(display (generate-single (arbitrary '(my-long-list-of integer))))
;;(newline)


(define liftm gen-fmap)

(define (leaf v)
  `(leaf ,v))

(define (branch ch1 ch2)
  `(branch ,ch1 ,ch2))

(install-testable-type! '(binary-tree ?)
                        (lambda (type)
                          (frequency
                           (1 (liftm leaf (arbitrary type)))
                           (2 (liftm2 branch 
                                      (arbitrary `(binary-tree ,type))
                                      (arbitrary `(binary-tree ,type))))))
                        no-coarb)

;; Warning, this will quite likely not terminate
;;(display (generate-single (arbitrary '(binary-tree integer))))
;;(newline)

(define (arb-tree type)
  (lambda (n)
    (if (= n 0)
        (liftm leaf (arbitrary type))
        (frequency
         (1 (liftm  leaf (arbitrary type)))
         (4 (liftm2 branch 
                    ((arb-tree type) (quotient n 2))
                    ((arb-tree type) (quotient n 2))))))))

(install-testable-type! '(bounded-binary-tree ?)
                        (lambda (type)
                          (sized (arb-tree type)))
                        no-coarb)
            
;;(display (generate-single (arbitrary '(bounded-binary-tree integer))))
;;(newline)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.3 Generating Functions
