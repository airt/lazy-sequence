#lang scheme

(require test-engine/racket-tests)
(require "../src/lazy-sequence.ss")

;;; some values and functions ;;;

(define (id x) x)

(define (const x)
  (λ (y) x))

(define natural-numbers
  (lazy-iterate add1 0))

(define primes
  (letrec ((sieve
    (λ (xs) (let ((x (lazy-car xs)))
      (lazy-cons x
        (sieve
          (lazy-filter
            (λ (y)
              (positive? (remainder y x)))
            xs)))))))
    (sieve (lazy-iterate add1 2))))

(define fibonacci
  (lazy-cons 1
    (lazy-cons 1
      (lazy-zip-with + fibonacci (lazy-cdr fibonacci)))))

;;; tests for these values ;;;

; natural-numbers
(check-expect
  (lazy->list (lazy-take 5 (lazy-drop 50 natural-numbers)))
  (range 50 55))

; primes
(check-expect
  (lazy->list (lazy-take 5 (lazy-drop 50 primes)))
  '(233 239 241 251 257))

; fibonacci
(check-expect
  (lazy->list (lazy-take 5 (lazy-drop 15 fibonacci)))
  '(987 1597 2584 4181 6765))

;;; list operations ;;;

; lazy?
(check-expect
  (map lazy?
    (list
      natural-numbers
      (cons 1 2)))
  '(#t #f))

; lazy->list
(check-expect
  (lazy->list
    (lazy-cons 1 (lazy-cons 2 '())))
  '(1 2))

; list->lazy
(check-expect
  (lazy->list
    (list->lazy (range 0 6)))
  (range 0 6))

; lazy-ref
(check-expect
  (lazy-ref 5 natural-numbers)
  5)

; lazy-range
(check-expect
  (lazy->list
    (lazy-range 0 5))
  (range 0 5))

; lazy-append
(check-expect
  (lazy->list
    (lazy-append
      (lazy-range 0 3)
      (lazy-range 3 6)
      (lazy-range 6 9)))
  (range 0 9))

; lazy-map
(check-expect
  (lazy->list
    (lazy-take 5
      (lazy-map
        (λ (x) (add1 (* x 2)))
        natural-numbers)))
  '(1 3 5 7 9))

; lazy-filter
(check-expect
  (lazy->list
    (lazy-take 5
      (lazy-filter
        odd?
        natural-numbers)))
  '(1 3 5 7 9))

;;; special folds ;;;

; lazy-and
(check-expect
  (map lazy-and
    (list
      '()
      (lazy-iterate not #t)))
  '(#t #f))

; lazy-or
(check-expect
  (map lazy-or
    (list
      '()
      (lazy-iterate not #f)))
  '(#f #t))

; lazy-any
(check-expect
  (map
    (λ (xs)
      (lazy-any
        (λ (x) (> x 9))
        xs))
    (list
      '()
      natural-numbers))
  '(#f #t))

; lazy-all
(check-expect
  (map
    (λ (xs)
      (lazy-all
        (λ (x) (< x 9))
        xs))
    (list
      '()
      natural-numbers))
  '(#t #f))

; lazy-concat
(check-expect
  (lazy->list
    (lazy-concat
      (list
        (lazy-range 0 3)
        (lazy-range 3 6)
        (lazy-range 6 9))))
  (range 0 9))

; lazy-concat-map
(check-expect
  (lazy->list
    (lazy-concat-map
      (λ (x) (range x (+ x 3)))
      '(0 3 6)))
  (range 0 9))

;;; infinite lists ;;;

; lazy-iterate
(check-expect
  (lazy-ref 5 (lazy-iterate add1 0))
  5)

; lazy-repeat
(check-expect
  (lazy-ref 5 (lazy-repeat 0))
  0)

; lazy-replicate
(check-expect
  (lazy->list (lazy-replicate 5 5))
  '(5 5 5 5 5))

; lazy-cycle
(check-expect
  (lazy->list
    (lazy-take 5
      (lazy-cycle
        (lazy-range 0 3))))
  '(0 1 2 0 1))

;;; sublists ;;;

; lazy-take
(check-expect
  (lazy->list (lazy-take 6 natural-numbers))
  (range 0 6))

; lazy-drop
(check-expect
  (lazy->list
    (lazy-take 5
      (lazy-drop 5 natural-numbers)))
  (range 5 10))

; lazy-take-while
(check-expect
  (lazy->list
    (lazy-take-while
      (λ (x) (< x 5))
      natural-numbers))
  (range 0 5))

; lazy-drop-while
(check-expect
  (lazy->list
    (lazy-take 5
      (lazy-drop-while
        (λ (x) (< x 5))
        natural-numbers)))
  (range 5 10))

; lazy-split-at
(check-expect
  (map lazy->list
    (lazy-split-at 5
      (lazy-range 0 10)))
  (list
    (range 0 5)
    (range 5 10)))

; lazy-span
(check-expect
  (map lazy->list
    (lazy-span
      (λ (x) (< x 5))
        (lazy-range 0 10)))
  (list
    (range 0 5)
    (range 5 10)))

;;; zipping lists ;;;

; lazy-zip
(check-expect
  (lazy->list
    (lazy-zip
      primes
      (lazy-range 0 3)))
  '((2 0) (3 1) (5 2)))

; lazy-zip-with
(check-expect
  (lazy->list
    (lazy-zip-with +
      primes
      (lazy-range 0 5)))
  '(2 4 7 10 15))

;;; run tests ;;;

(test)
