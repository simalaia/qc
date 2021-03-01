[module scheme-check mzscheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme-Check: A lightweight tool for random testing
;;;               of Scheme Programs

;;; blatantly based on QuickCheck [1]

;; By Carlos Eduardo Scheidegger, 2003
;;
;; Copyright (C) 2003 Carlos E. Scheidegger 
;;                    <carlos.scheidegger@terra.com.br>
;;
;; This  library is  free  software; you  can  redistribute it  and/or
;; modify it under the terms  of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2.1 of
;; the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE. See  the GNU
;; Lesser General Public License for more details.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   If you put this to good use, let me know! I'd appreciate it
;;   quite a bit.. Thanks!
;;                                                      - Carlos

  
(require (all-except (lib "etc.ss") identity compose))
(require "split-rng.scm")
(require (all-except (lib "list.ss") foldl foldr))

(provide (rename return gen-return)     ;; A -> gen A
         (rename bind gen-bind)         ;; gen A (A -> gen B) -> gen B
         (rename fmap gen-fmap)         ;; (A -> B) gen A -> gen B
         (rename sequence gen-sequence) ;; (list (gen A) (gen B) ...)
                                        ;;    -> gen (list A B ...)

         sized                          ;; (int -> gen V) -> gen V
         resize-by                      ;; (int -> int) -> gen V -> gen V
         resize                         ;; int -> gen V -> gen V
         rand                           ;; gen V -> struct:s-rng
         promote                        ;; (A -> gen V) -> gen (A -> V)         
         variant                        ;; int -> gen A -> gen A
         ;; variant has time complexity linear on the magnitude of
         ;; the first argument. YHBW.

         generate                       ;; int -> struct:s-rng -> gen A -> A
         generate-single                ;; gen A -> A
         choose                         ;; int int -> gen int
         elements                       ;; (list-of A) -> gen A
         oneof                          ;; (gen A) (gen B) ... -> gen (A/B/...)
         frequency                      ;; (int (gen A)) (int (gen B)) ...
                                        ;;    -> gen (A/B/...)

         vector-of                      ;; type -> int -> gen (list-of type)
         
         arbitrary                      ;; type -> (gen type)
         coarbitrary                    ;; type -> (type -> gen B -> gen B)

         install-testable-type!         ;; type -> (gen type) -> 
                                        ;;    (type -> gen A -> gen A) ->

         testable-types                 ;; -> (list-of type)
         
         property-if                    ;; bool property -> property
         classify                       ;; bool string property -> property
         trivial                        ;; bool property -> property
         collect                        ;; a property -> property

         test-property-general          ;; int int int (A B C ... -> property)
                                        ;;   (list (gen A) (gen B) ...) ->
                                        ;;   result
         ;; the first three parameters are the size to be passed
         ;; to the generators, the maximum number of tests to be passed
         ;; and the maximum number of property condition failures
         ;; the following parameter is the property, and the final parameter
         ;; is a list of the generators for the parameters

         test-property                  ;; (A B C ... -> property)
                                        ;;   (list A B C ...) -> result
         ;; test-property uses default values for size, max-tests and
         ;; max-fails. Moreover, the list to be passed contains the types
         ;; themselves, instead of generators

         test-property-with-generators ;; (A B ... -> property)
                                       ;;   (list (gen A) (gen B) ...) ->
                                       ;;     result
         ;; test-property-with-generators is equivalent to test-property
         ;; but the list to be passed has generators instead of types

         define-testable-struct        ;; struct-name ((field-name type) ...)
         ;; define-testable-struct is a macro that defines all procedures
         ;; necessary and installs them in the dictionaries so that
         ;; the struct can be used in properties (ie, values of the struct
         ;; can be generated randomly)

         with-testable-types           ;; ((type-name arb-def coarb-def) ...)
                                       ;;   body ->
         ;; with-testable-types installs the types, evaluates its body
         ;; and later restores the dictionary states, avoiding unnecessary
         ;; pollution of types in the namespace

         test-suite ;; ->
         ;; internal tests

         gen-do
         ;; Haskell-like do-syntax for the generator monad

         )

;; Generally Applicable Warning:
;; This is ported from haskell code
;; much, much curried madness. you have been warned.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gen value: size -> generator -> value

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General combinators, similar to the Haskell prelude

;; identity: A ... -> A ...
(define (identity . x)
  (apply values x))

;; foldr1: (A A -> A) (list-of A) -> A  
(define (foldr1 f lst)
  (cond
   [(null? (cdr lst)) (car lst)]
   [else
    (f (car lst)
       (foldr1 f (cdr lst)))]))

;; foldr: (A B -> B) B (list-of A) -> B
(define (foldr f zero lst)
  (cond
   [(null? lst) zero]
   [else (f (car lst) 
            (foldr f 
                   zero 
                   (cdr lst)))]))

;; foldl: (A B -> A) A (list-of B) -> A
(define (foldl f zero lst)
  (define (the-fold el lst)
    (cond
     [(null? lst) el]
     [else
      (the-fold (f el (car lst)) (cdr lst))]))
  (the-fold zero lst))

;; foldl1: (A A -> A) (list-of A) -> A
(define (foldl1 f lst)
  (foldl f (car lst) (cdr lst)))

;; compose: (list (Y -> Z) (X -> Y) ... (B -> C) (A -> B)) -> (A -> Z)
(define (compose . lof)
  (define (compose-single g f)
    (lambda (n)
      (g (f n))))
  (foldr1 compose-single lof))

;; const: A -> (B -> A)
(define (const n)
  (lambda _
    n))
      
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generator monad stuff

;; instance monad...
;; constant

;; return: A -> gen A
(define (return a)
  (lambda (n)
    (lambda (r)
      a)))

;; building complex generators
;; bind as in the monad 'bind'

;; bind: gen A (A -> gen B) -> gen B
(define (bind m k)
  (lambda (n)
    (lambda (r0)
      (let-values ([(r1 r2) (s-rng-split r0)])
        (let ([m. (k ((m n) r1))])
          ((m. n) r2))))))
  
;; monad combinators

;; instance functor: this is just liftM, why defining it like this?
;;   (in the haskell source, of course - here we don't have liftM :)
;; fmap: (A -> B) gen A -> gen B
(define (fmap f m)
  (bind m
        (compose return f)))

;; sequence: (list (gen A) (gen B) ...) -> gen (list A B ...)
(define (sequence lst)
  (cond
   [(null? lst) (return ())]
   [else
    (gen-do (<- x (car lst))
            (<- xs (sequence (cdr lst)))
            (return (cons x xs)))]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generator combinators
  
;; sized: (int -> gen V) -> gen V
(define (sized gen)
  (lambda (n)
    (lambda (r)
      (((gen n) n) r))))

;; resize-by: (int -> int) -> gen V -> gen V
(define resize-by
  (lambda (f)
    (lambda (m)
      (lambda (s)
        (lambda (r)
          ((m (f s)) r))))))

;; resize: int -> gen V -> gen V
(define (resize n)
  (resize-by (const n)))

;; rand: gen V -> struct:s-rng
(define rand
  (lambda (s)
    (lambda (r)
      r)))
  
;; promote: (A -> gen V) -> gen (A -> V)
(define (promote f)
  (lambda (s)
    (lambda (r)
      (lambda a
        (((apply f a) s) r)))))
 
;; variant: int -> gen A -> gen A
(define (variant v)

  (define (new-rand gen count)
    (let-values
        ([(left right) (s-rng-split gen)])
      (if (= 0 count)
          left
          (new-rand right (sub1 count)))))

  (lambda (m)
    (lambda (n)
      (lambda (r)
        ((m n) (new-rand r (add1 v)))))))

;; generate: int struct:s-rng (gen A) -> A
(define (generate s r gen)
  ((gen s) r))

(define default-size 10)

;; generate-single: gen A -> A
(define (generate-single gen)
  (let ([s default-size]
        [r (make-s-rng (random 1000000000))])
    ((gen s) r)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; derived generator combinators

;; s-random-range-gen :: struct:s-rng int int -> int
(define (s-random-range-gen gen from to)
  (+ from
     (let-values ([(v _)
                   (s-rng-random gen
                                 (- to from))])
       v)))

;; warning: haskell's range is closed. ours is half-closed
;; choose: int int -> gen int
(define (choose range-from range-half-to)
  (fmap (lambda (r)
          (s-random-range-gen r range-from range-half-to))
        rand))

;; elements: (list-of A) -> gen A
(define (elements xs)
  (fmap (lambda (n)
          (list-ref xs n))
        (choose 0 (length xs))))

;; the-oneof: (list (promise-of (gen A)) (promise-of (gen B)) ...) 
;;               -> gen (A/B/...)

(define (the-oneof gens)
  ;; force-bind: (gen (promise-of A)) (A -> gen B) -> gen B
  (define (force-bind m k)
    (lambda (n)
      (lambda (r0)
        (let-values ([(r1 r2) (s-rng-split r0)])
          (let ([m. (k (force ((m n) r1)))])
            ((m. n) r2))))))

  (force-bind (elements gens)
              identity))

;; oneof: (gen A) (gen B) ... -> gen (A/B/...)
(define-syntax oneof
  (lambda (x)
    (syntax-case x ()
      [(oneof gen ...)
       (syntax
        (the-oneof (list (delay gen) ...)))])))



;; the-frequency: (list (int (promise-of (gen B))) (int (promise-of (gen B))) ...) 
;;                        -> gen (A/B/..)
(define (the-frequency xs)
  (define (pick n lst)
    (let ([k (caar lst)]
          [x (cadar lst)])
      (if (<= n k)
          (force x)
          (pick (- n k) (cdr lst)))))
  (let ([tot (apply + (map (lambda (x) (car x)) xs))])
    (gen-do (<- n (choose 1 (add1 tot)))
            (pick n xs))))

;; frequency: (int (gen A)) (int (gen B)) ... -> gen (A/B/...)
(define-syntax frequency
  (lambda (x)
    (syntax-case x ()
      [(frequency (fr gen) ...)
       (syntax
        (the-frequency 
         (list (list fr 
                     (delay gen)) ...)))])))

;; vector-of :: type -> int -> gen (list-of type)
(define (vector-of type)
  (lambda (n)
    (sequence (build-list n
                          (lambda (_) (arbitrary type))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-defined generators

;; *-arbitrary: gen *
;; *-coarbitrary: * -> gen A -> gen A

(define boolean-arbitrary
  (elements (list #t #f)))

(define boolean-coarbitrary
  (lambda (b)
    (if b
        (variant 0)
        (variant 1))))

(define char-arbitrary
  (fmap integer->char
        (choose 32 126)))

(define char-coarbitrary
  (lambda (n)
    (variant (char->integer n))))



(define int-arbitrary
  (sized (lambda (n)
           (choose (- n) n))))

(define int-coarbitrary
  (lambda (n)
    (variant (if (> n 0)
                 (* 2 n)
                 (add1 (* 2 (- n)))))))


(define rational-arbitrary
  (lambda (s)
    (lambda (r)
      (let ([v (((sequence
                   (list ((resize-by (lambda (x) (* x x))) int-arbitrary)
                         (sized (lambda (n)
                                  (choose 1 n))))) s) r)])
        (/ (car v) (cadr v))))))

(define rational-coarbitrary
  (lambda (rat)
    (compose (variant (numerator rat))
             (variant (denominator rat)))))


; Yeah, kludge
(define real-arbitrary
  (lambda (s)
    (lambda (r)
      (exact->inexact ((rational-arbitrary s) r)))))

; Big YMMV here, you have been warned
(define real-coarbitrary
  (lambda (real)
    (rational-coarbitrary
     (rationalize (inexact->exact real) 1/1000))))


; Maybe this should be parameterized (to get "integer complex", 
; "rational complex" and "real complex" numbers)
(define complex-arbitrary
  (lambda (s)
    (lambda (r)
      (let [(v ((((vector-of 'real) 2) s) r))]
        (+ (car v) (* (cadr v) 0+1i))))))

(define complex-coarbitrary
  (lambda (c)
    (compose (real-coarbitrary (real-part c))
             (real-coarbitrary (imag-part c)))))


(define (list-arbitrary type) ;; dispatched based on type
  (sized (lambda (n)
           (bind (choose 0 n)
                 (vector-of type)))))


(define (list-coarbitrary type)
  (lambda (lst)
    (cond
     [(null? lst) (variant 0)]
     [else 
      (compose (variant 1)
               (compose ((coarbitrary type) (car lst))
                        ((coarbitrary `(list-of ,type)) (cdr lst))))])))


(define string-arbitrary
  (fmap list->string
        (list-arbitrary 'char)))

(define string-coarbitrary
  (compose (list-coarbitrary 'char)
           string->list))
           

(define symbol-arbitrary
  (fmap string->symbol
        string-arbitrary))

(define symbol-coarbitrary
  (compose string-coarbitrary
           symbol->string))
           
;; We'll curry and uncurry this under the hood:
;; (function-arbitrary type-1 type-2 type-3 type-4)
;; ==> (function-arbitrary type-1
;;       (function-arbitrary type-2 
;;          (function-arbitrary type-3 type-4))) <== this is foldr!
;; after that, we foldl the function application inside the monad
;; and put that inside the monad, too. Whew

(define (function-arbitrary . types)

  (define (real-function-arbitrary type-from type-to)
    (promote
     (lambda (n)
       (((coarbitrary type-from) n) (arbitrary type-to)))))

  (cond
   [(< (length types) 2)
    (raise-type-error 'function-arbitrary "Two or more types" types)]
   [(= (length types) 2)
    (real-function-arbitrary (car types) (cadr types))]
   [else
    (let ([domain (car types)]
          [codomain (foldr1 (lambda (x y)
                              (list 'function x y)) 
                            (cdr types))])
      (promote (lambda n
                 (let ([right-arity (sub1 (length types))])
                   (when (not (= right-arity
                                 (length n)))
                     (raise-type-error 'function-arbitrary 
                                       (format "~A values" right-arity)
                                       n)))
                 (foldl (lambda (gen v)
                          (lambda (s)
                            (lambda (r)
                              (((gen s) r) v))))
                        (real-function-arbitrary domain codomain)
                        n))))]))

; Similar currying-uncurrying magic here

(define (function-coarbitrary . types)

  (define (last lst)
    (cond
     [(null? (cdr lst)) (car lst)]
     [else (last (cdr lst))]))

  (define (init lst)
    (cond
     [(null? (cdr lst)) ()]
     [else (cons (car lst) (init (cdr lst)))]))

  (define (curryfier c f)
    (define (worker count lst)
      (cond
       [(= count 0) (apply f (reverse lst))]
       [else (lambda (i)
               (worker (sub1 count) (cons i lst)))]))
    (worker c ()))

  (lambda (f)
    ((foldr (lambda (arb coarb)
              (lambda (f)
                (lambda (gen)
                  (bind arb
                        (lambda (a)
                          ((coarb (f a)) gen))))))
            (coarbitrary (last types))
            (map arbitrary (init types))) (curryfier (sub1 (length types)) f))))

(define (choice-arbitrary . types)
  (the-oneof (map (lambda (t)
                    (delay (arbitrary t)))
                  types)))

(define number-arbitrary
  (oneof int-arbitrary
         rational-arbitrary
         real-arbitrary
         complex-arbitrary))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Poor man Haskell typeclasses
;; a roll-your-own type-based dispatch system

(define (coarb-unimplemented type-name)
  (lambda (_)
    (raise (format "~A coarbitrary unimplemented (for now)" type-name))))

;; entry: (type method)
;; type: symbol
;; method: function

;; dictionaries: (list-of entry)
(define arbitrary-dictionary
  `((boolean ,boolean-arbitrary)
    (integer ,int-arbitrary)
    (rational ,rational-arbitrary)
    (real ,real-arbitrary)
    (complex ,complex-arbitrary)
    (char ,char-arbitrary)
    (string ,string-arbitrary)
    (symbol ,symbol-arbitrary)

    ((list-of ?) ,list-arbitrary)
    ((function *) ,function-arbitrary)

;; There is no general way to do coarbitrary of this unless
;; coarbitrary annotation includes a type-predicate
;; (integer? for integer, etc).
;; Doable, but not for now
    ((choice *) ,choice-arbitrary)
    (number ,number-arbitrary)

    ))

(define coarbitrary-dictionary
  `((boolean ,boolean-coarbitrary)
    (integer ,int-coarbitrary)
    (rational ,rational-coarbitrary)
    (real ,real-coarbitrary)
    (complex ,complex-coarbitrary)
    (char ,char-coarbitrary)
    (string ,string-coarbitrary)
    (symbol ,symbol-coarbitrary)

    ((list-of ?) ,list-coarbitrary)
    ((function *) ,function-coarbitrary)

    ((choice *) ,(coarb-unimplemented "Choice"))
    (number ,(coarb-unimplemented "Number"))

    ))


(define (install-testable-type! name arb-fun coarb-fun)
  (set! arbitrary-dictionary 
        (cons (list name arb-fun)
              arbitrary-dictionary))
  (set! coarbitrary-dictionary
        (cons (list name coarb-fun)
              coarbitrary-dictionary)))

;; installed-types :: -> (list-of type)
(define (testable-types)
  (map car arbitrary-dictionary))

;; atomic? :: type -> bool
(define (atomic? type)
  (symbol? type))

;; type-constructor? :: type -> bool
(define (composite? type)
  (list? type))

;; type-constructor :: composite-type -> symbol
(define (type-constructor type)
  (car type))

;; fixed-arity-constructor? :: composite-type -> bool
(define (fixed-arity-constructor? type) 
  (and (> (length type) 1)
       (andmap (lambda (x) (eq? x '?))
               (cdr type))))

;; variable-arity-constructor? :: composite-type -> bool
(define (variable-arity-constructor? type)
  (and (= (length type) 2)
       (eq? (cadr type) '*)))

;; composing-types :: composite-type -> (list-of type)
(define (composing-types type)
  (cdr type))

;; type-name :: entry -> type
(define (type-name dictionary-entry)
  (car dictionary-entry))

;; method :: entry -> method
(define (method dictionary-entry)
  (cadr dictionary-entry))

;; type: (list-of integer)
;; entry: ((list-of ?) ,list-method)
;; Find the first that matches, no backtrack

; composite types are either of form
;   (fixed-arity-constructor ? ? ? ...) or
;   (variable-arity-constructor *)
;  It would be interesting to have, for example,
;   (fixed-arity-constructor ? some-fixed-type), 
;     but I just want a one-sided-matcher,
;     not a freaking unifier (for now), so I won't implement this

;; match: type -> dict -> boolean
(define (match type dict)
  (cond
   [(atomic? type) (find (lambda (entry)
                           (eq? (type-name entry) type))
                         dict)]
   [(composite? type)
    (and (find (lambda (entry)
                 (let ([name (type-name entry)])
                   (and (composite? name)
                        (eq? (type-constructor type)
                             (type-constructor name))
                        (cond
                         [(fixed-arity-constructor? name)
                          (= (length type) (length name))]
                         [(variable-arity-constructor? name) #t]
                         [else #f]))))
               dict)
         (andmap (lambda (t)
                   (match t dict))
                 (composing-types type)))]
   [else #f]))

;; This assumes a matched type
;; If we have an atomic type, just return the method
;; If we have a composite type, apply the composing methods
;;   to the base type generator to get the composite method

;; build-method: type -> dict -> gen type
(define (build-method type dict)
  (cond
   [(atomic? type) (method (find (lambda (entry)
                                      (eq? (type-name entry) type))
                                    dict))]
   [(composite? type) 
    (apply (method (find (lambda (entry)
                              (and (composite? (type-name entry))
                                   (eq? (type-constructor type)
                                        (type-constructor (type-name entry)))))
                            dict))
           (composing-types type))]
   [else #f]))

(define (find pred? lst)
  (cond
   [(null? lst) #f]
   [(pred? (car lst)) (car lst)]
   [else (find pred? (cdr lst))]))


;; arbitrary :: type -> procedure
(define (arbitrary type)
  (if (match type arbitrary-dictionary)
      (build-method type arbitrary-dictionary)
      #f))

;; coarbitrary :: type -> procedure
(define (coarbitrary type)
  (if (match type coarbitrary-dictionary)
      (build-method type coarbitrary-dictionary)
      #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property combinators

;; Property :: either boolean 'failed-condition (list 'classify key)
;;   key is a string, and the value "trivial" has a special treatment:
;;   the property testers return the ratio of trivial results to real
;;   ones. This way you can programatically check the quality of the
;;   tests performed for a given definition of "trivial"

(define (property? p)
  (or (boolean? p)
      (eq? p 'failed-condition)
      (and (list? p)
           (eq? (car p) 'classify)
           (string? (cadr p)))))

;; property-if: bool property -> property
(define (property-if condition predicate)
  (if condition
      predicate
      'failed-condition))

;; classify: bool string property -> property
(define (classify condition name predicate)
  (cond 
   [(not predicate) #f]
   [condition `(classify ,name)]
   [else predicate]))

;; trivial: bool property -> property
(define (trivial condition predicate)
  (classify condition "trivial" predicate))

;; collect: A property -> property
(define (collect v predicate)
  (cond
   [(not predicate) #f]
   [else `(classify ,(format "~A" v))]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual testing

(define maximum-test-count 100)
(define maximum-fail-count 1000)

;; result: either (list 'ok real) 
;;                (list 'ok-exhausted real) 
;;                (list 'failed-test counter-example-values)

;; test-property-general: int -> int -> int -> (a b c ... -> property)
;;                          -> (list (gen a) (gen b) (gen c) ...) -> result
(define (test-property-general
         size max-tests max-fails 
         property generators)

  (define value-generator
    (sequence generators))

  (define (make-arbitrary-values s-rng)
    (let-values ([(left right)
                  (s-rng-split s-rng)])
      (values left
              ((value-generator size) right))))


  (define (show-histogram the-list count)

    (define hash (make-hash-table 'equal))

    (define (push-to-histogram v)
      (hash-table-safe-get hash v
                           (lambda (value) ; found
                             (hash-table-put! hash v (add1 value)))
                           (lambda () ; not found
                             (hash-table-put! hash v 1))))

    (define (sorted-histogram)
      (mergesort (map (lambda (entry)
                        (list (car entry)
                              (/ (truncate (* 10000
                                              (/ (cadr entry) count)))
                                 100.0)))

                      (hash-table-map hash (lambda (k v) (list k v))))
                 (lambda (e1 e2)
                   (or (> (cadr e1)
                          (cadr e2))
                       (and (= (cadr e1)
                               (cadr e2))
                            (string<? (car e1)
                                      (car e2)))))))

    (define (trivial-ratio)
      (hash-table-safe-get hash "trivial"
                           (lambda (trivial-freq) (- 1.0 
                                                     (/ trivial-freq count)))
                           (lambda () 1.0)))

    (for-each push-to-histogram the-list)
    (for-each (lambda (entry) (printf "~A% ~A.~n" (cadr entry) (car entry)))
              (sorted-histogram))
    (trivial-ratio))

  (define (iterate-through-tests classify-list test-count fail-count s-rng)
    (cond
     [(= fail-count maximum-fail-count)
      (printf "Exhausted tests (failed condition ~A times) after passing ~A tests.~n"
              fail-count test-count)
      (list 'ok-exhausted 
            (show-histogram classify-list test-count))]
     [(= test-count maximum-test-count)
      (printf "Ok, passed ~A tests.~n" test-count)
      (list 'ok
            (show-histogram classify-list test-count))]
     [else
      (let-values 
          ([(next-rng the-values) (make-arbitrary-values s-rng)])
        (let ([the-result (apply property the-values)])
          (cond
           [(eq? the-result 'failed-condition) ; Check failed condition
            (iterate-through-tests classify-list
                                   test-count 
                                   (add1 fail-count)
                                   next-rng)]
           [(and (list? the-result)
                 (eq? (car the-result) 'classify)
                 (= (length the-result) 2)) ; Check classify
            (iterate-through-tests (cons (cadr the-result)
                                         classify-list)
                                   (add1 test-count)
                                   fail-count
                                   next-rng)]
           [(eq? the-result #t) ; Valid
            (iterate-through-tests classify-list
                                   (add1 test-count)
                                   fail-count
                                   next-rng)]
           [(eq? the-result #f) ; Counter-example
            (printf "NOT OK, falsifiable after ~A tests, with arguments ~A~n"
                    test-count
                    the-values)
            (show-histogram classify-list test-count)
            (list 'failed-test the-values)]
           [else
            (printf "Internal error, property did not return valid value: ~A~n"
                    the-result)])))]))

  (iterate-through-tests () 0 0 (make-s-rng (current-seconds))))

;; test-property: (a b c ... -> property) -> 
;;                  (list a b c ...) -> result
(define (test-property property types)
  (test-property-general default-size maximum-test-count maximum-fail-count
                         property (map arbitrary types)))

;; test-property-with-generators:
;;   (a b c ... -> property) -> (list (gen a) (gen b) (gen c) ...) -> result
(define (test-property-with-generators property generators)
  (test-property-general default-size maximum-test-count maximum-fail-count
                         property generators))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful macros

;; This is just hash-table-get with procedures for both
;; finding and not finding the key
(define-syntax hash-table-safe-get
  (lambda (x)
    (syntax-case x ()
      [(_ hash key found not-found)
       (syntax
        (let/ec escape
          (let ([value (hash-table-get hash 
                                       key
                                       (lambda ()
                                         (escape (not-found))))])
            (found value))))])))


;; This allows structs with simple data to be used as arbitrary values
;; For example, consider
;;   (define-struct point (x y))
;; To generate arbitrary points, write
;;   (define-testable-struct point ((x integer) (y integer)))
;; the macro takes care of defining appropriate functions and inserting
;; them in the dictionary tables

(define-syntax define-testable-struct
  (lambda (x)

    (define (make-arb-maker type-name)
      (string->symbol
       (string-append "make-"
                      (symbol->string
                       (syntax-object->datum type-name)))))

    (define (make-record-names type-name lst)
      (map (lambda (el)
             (string->symbol
              (string-append (symbol->string
                              (syntax-object->datum type-name))
                             "-"
                             (symbol->string el))))
           (syntax-object->datum lst)))

    (define (all-ids? ls)
      (or (null? ls)
          (and (identifier? (car ls))
               (all-ids? (cdr ls)))))                           

    (syntax-case x ()
      [(_ name ((v1 t1) ...))
       (cond
        [(not (all-ids? (syntax-e (syntax (v1 ...)))))
         (raise-syntax-error #f "Expected all identifiers"
                             (syntax _) (syntax (v1 ...)))
         ]
        [(not (all-ids? (syntax-e (syntax (t1 ...)))))
         (raise-syntax-error #f "Expected all identifiers"
                             (syntax _) (syntax (t1 ...)))
         ]
        [(not (identifier? (syntax name)))
         (raise-syntax-error #f "Expected identifier" 
                             (syntax _)
                             (syntax name))]
        [else
         (with-syntax 
             ([struct-constructor (make-arb-maker (syntax name))]
              [acessor-list (quote
                             (make-record-names (syntax name)
                                                (syntax (v1 ...))))])
           
           (syntax
            (install-testable-type!
             (quote name)
             ;; arb-function
             (lambda (n)
               (lambda (r)
                 (apply struct-constructor
                        (((sequence (map arbitrary 
                                         (quote (t1 ...)))) n) r))))
             
             ;; coarb-function
             (lambda (p)
               (apply compose
                      (map (lambda (a b)
                             (compose (variant 0)
                                      ((coarbitrary a) (b p))))
                           (quote (t1 ...))
                           accessor-list))))))])])))


;; This allows temporary types to be defined without polluting
;; the dictionary, and as such is ideal to use in internal module
;; tests

(define (install-many-types! loe)
  (for-each (lambda (x)
              (apply install-testable-type! x))
            loe))

(define (set-arbitrary! v)
  (set! arbitrary-dictionary v))

(define (set-coarbitrary! v)
  (set! coarbitrary-dictionary v))

(define-syntax with-testable-types
  (lambda (x)
    (syntax-case x ()
      [(_ ((name arb coarb) ...) rest ...)
       (syntax
        (let [(arb-dict arbitrary-dictionary)
              (coarb-dict coarbitrary-dictionary)]
          (install-many-types! (list (list (quote name) arb coarb) ...))
          rest ...
          (set-arbitrary! arb-dict)
          (set-coarbitrary! coarb-dict)))])))

;; Haskell-like do-syntax for the generator monad
;; Somewhat tricky, arbitrary captures

(define-syntax gen-do
  (lambda (x)
    (syntax-case x ()
      [(gen-do (<- var exp) exp-rest ...)
       (eq? (syntax-object->datum (syntax <-))
            '<-)
       (syntax-case (datum->syntax-object
                     (syntax gen-do)
                     (syntax-object->datum (syntax var))) ()
           [new-var
            (syntax
             (bind exp
                   (lambda (new-var)
                     (gen-do exp-rest ...))))])]
      [(gen-do exp) (syntax exp)]
      [(gen-do exp exp-rest ...)
       (syntax
        (bind exp
              (lambda (unused)
                (gen-do exp-rest ...))))])))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Suite

(define (test-suite)
  (with-testable-types ((s-rng (fmap make-s-rng (choose 0 2147483562))
                               (coarb-unimplemented "S-rng"))

                        (property (oneof (arbitrary 'boolean)
                                         (return 'failed-condition)
                                         (fmap (lambda (x) (list 'classify x))
                                               (arbitrary 'string))
                                         (return (list 'classify "trivial")))
                                  (coarb-unimplemented "Property"))

                        (natural (gen-do (<- n (arbitrary 'integer))
                                         (return (add1 (abs n))))
                                 (coarb-unimplemented "Natural")))

    ; Choose respects ranges
    (test-property (lambda (rng from to)
                     (property-if (> to from)
                                  (let ([v (generate 100 rng (choose from to))])
                                    (and (>= v from)
                                         (< v to)))))
                   '(s-rng integer integer))
    
    ; Refactoring char-arbitrary
    (test-property (lambda (rng size)
                     (eq? (generate size 
                                    rng
                                    char-arbitrary)
                          (generate size
                                    rng 
                                    (fmap integer->char
                                          (choose 32 126)))))
                   '(s-rng integer))

    ; Property combinators are closed under property
    ; property-if
    (test-property (lambda (cnd property)
                     (property? (property-if cnd property)))
                   '(boolean property))
    (test-property (lambda (cnd name predicate)
                     (property? (classify cnd name predicate)))
                   '(boolean string property))
    (test-property (lambda (cnd property)
                     (property? (trivial cnd property)))
                   '(boolean property))
    (test-property (lambda (value property)
                     (property? (collect value property)))
                   '(integer property))
    
    ; Function-arbitrary currying-uncurrying works
    (test-property-with-generators
     (lambda (v)
       (integer? v))

     ;; What we do here is test the under-the-hood uncurrying for test-arbitrary
     ;; The test work by generating a random list of integers, and using
     ;; this list to create a function that takes that many integers and
     ;; applies the list to the function. The result must be an integer.
     (list (gen-do (<- lst (bind (arbitrary 'natural) (vector-of 'integer)))
                   (<- f (arbitrary `(function ,@(build-list
                                                  (add1 (length lst))
                                                  (const 'integer)))))
                   (return (apply f lst)))))

     ))
]

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; References:

;;   [1] Classen, Koen. Hughes, John. QuickCheck: A lightweight tool for 
;; random testing of Haskell programs. Proceedings of the International 
;; Conference on Functional Programming, 2000.
