# lazy-sequence

[![Build Status][build-badge]][build-status]

exercises of lazy evaluation

## usage

```scheme
(define natural-numbers
  (lazy-iterate add1 0))

(lazy->list
  (lazy-take 5
    (lazy-drop 50 natural-numbers)))
; => (range 50 55)

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

(lazy->list
  (lazy-take 5
    (lazy-drop 50 primes)))
; => '(233 239 241 251 257)
```

## api

```scheme
;;; list operations ;;;

lazy?
lazy->list
list->lazy

lazy-cons
lazy-car
lazy-cdr
lazy-caar
lazy-cdar

lazy-ref
lazy-range
lazy-append

lazy-foldr
lazy-filter
lazy-map

;;; special folds ;;;

lazy-and
lazy-or
lazy-any
lazy-all
lazy-concat
lazy-concat-map

;;; infinite lists ;;;

lazy-iterate
lazy-repeat
lazy-replicate
lazy-cycle

;;; sublists ;;;

lazy-take
lazy-drop
lazy-take-while
lazy-drop-while
lazy-split-at
lazy-span

;;; zipping lists ;;;

lazy-zip
lazy-zip-with
```

## license

BSD-3-Clause

[build-badge]: https://img.shields.io/travis/airt/lazy-sequence/master.svg
[build-status]: https://travis-ci.org/airt/lazy-sequence
