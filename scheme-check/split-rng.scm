[module split-rng mzscheme

#| Literally ported from the Haskell 98 Library
   We need this for the fancy stuff at scheme-check

   Specifically, we must split random number generators
   for the coarbitrary instances
|#

; (provide (all-defined))

(provide (rename create-s-rng make-s-rng)
         s-rng-next
         s-rng-split
         s-rng-random)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct s-rng (s1 s2))

(define (create-s-rng seed)
  (let* ([s (abs seed)]
         [q (quotient s 2147483562)] ; I'm afraid we're falling into bigint
         [s1 (modulo s  2147483562)] ; zone here.. whatever
         [s2 (modulo q  2147483398)])
    (make-s-rng (add1 s1) (add1 s2))))

(define (s-rng-next generator)
  (let* ([s1 (s-rng-s1 generator)]
         [s2 (s-rng-s2 generator)]
         [k (quotient s1 53668)]
         [s1. (- (* 40014 
                    (- s1
                       (* k 53668)))
                 (* k 12211))]
         [s1.. (if (< s1. 0)
                   (+ s1. 2147483563)
                   s1.)]
         [k. (quotient s2 52774)]
         [s2. (- (* 40692
                    (- s2
                       (* k. 52774)))
                 (* k. 3791))]
         [s2.. (if (< s2. 0)
                   (+ s2. 2147483399)
                   s2.)]
         [z (- s1.. s2..)]
         [z. (if (< z 1)
                 (+ z 2147483562)
                 z)])
    (values z. (make-s-rng s1.. s2..))))

(define (s-rng-split generator)
  (let-values ([(_ next) (s-rng-next generator)])
    (let* ([s1 (s-rng-s1 generator)]
           [s2 (s-rng-s2 generator)]
           [t1 (s-rng-s1 next)]
           [t2 (s-rng-s2 next)]
           [new-s1 (if (= s1 2147483562)
                       1
                       (add1 s1))]
           [new-s2 (if (= s2 1)
                       2147483398
                       (sub1 s2))]
           [left  (make-s-rng new-s1 t2)]
           [right (make-s-rng new-s2 t1)])
      (values left right))))

(define srng (create-s-rng 0))

(define (s-rng-random gen range)
  (let-values ([(value next) (s-rng-next gen)])
    (values (quotient (* value range)
                      2147483562)
            next)))


]
