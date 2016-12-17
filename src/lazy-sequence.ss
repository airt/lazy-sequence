#lang scheme

(provide

  ;;; call by need ;;;

  need
  defer

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

)

;;; call by need ;;;

; need
(define need force)

; defer
(define-syntax defer
  (syntax-rules ()
    ((_ e ...) (delay e ...))))

;;; list operations ;;;

; lazy?
(define (lazy? xs)
  (and
    (pair? xs)
    (promise? (cdr xs))))

; lazy->list
(define (lazy->list xs)
  (if (pair? xs)
    (cons
      (lazy-car xs)
      (lazy->list (lazy-cdr xs)))
    '()))

; list->lazy
(define (list->lazy xs)
  (if (pair? xs)
    (lazy-cons
      (car xs)
      (list->lazy (cdr xs)))
    '()))

; lazy-cons
(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

; lazy-car
(define lazy-car car)

; lazy-cdr
(define (lazy-cdr xs)
  (force (cdr xs)))

; lazy-caar
(define (lazy-caar xss)
  (lazy-car
    (lazy-car xss)))

; lazy-cdar
(define (lazy-cdar xss)
  (lazy-cdr
    (lazy-car xss)))

; lazy-ref
(define (lazy-ref n xs)
  (cond
    [(null? xs) '()]
    [(not (positive? n)) (lazy-car xs)]
    [else (lazy-ref (sub1 n) (lazy-cdr xs))]))

; lazy-range
(define (lazy-range start end [step 1])
  (if (< start end)
    (lazy-cons
      start
      (lazy-range
        (+ start step)
        end
        step))
    '()))

; lazy-append
(define (lazy-append . xss)
  (lazy-concat xss))

; lazy-foldr
(define (lazy-foldr f z xs)
  (if (pair? xs)
    (f
      (defer (lazy-car xs))
      (defer (lazy-foldr f z (lazy-cdr xs))))
    z))

; lazy-filter
(define (lazy-filter p xs)
  (lazy-foldr
    (λ (x z)
      (if (p (need x))
        (lazy-cons (need x) (need z))
        (need z)))
    '()
    xs))

; lazy-map
(define (lazy-map f xs)
  (lazy-foldr
    (λ (x z)
      (lazy-cons
        (f (need x))
        (need z)))
    '()
    xs))

;;; special folds ;;;

; lazy-and
(define (lazy-and xs)
  (call/cc
    (λ (cc)
      (let loop ((xs xs))
        (cond
          [(null? xs) (cc #t)]
          [(not (lazy-car xs)) (cc #f)]
          [else (loop (lazy-cdr xs))])))))

; lazy-or
(define (lazy-or xs)
  (call/cc
    (λ (cc)
      (let loop ((xs xs))
        (cond
          [(null? xs) (cc #f)]
          [(lazy-car xs) (cc #t)]
          [else (loop (lazy-cdr xs))])))))

; lazy-any
(define (lazy-any f xs)
  (lazy-or
    (lazy-map f xs)))

; lazy-all
(define (lazy-all f xs)
  (lazy-and
    (lazy-map f xs)))

; lazy-concat
(define (lazy-concat xss)
  (cond
    [(null? xss) '()]
    [(null? (lazy-car xss))
      (lazy-concat (lazy-cdr xss))]
    [else
      (lazy-cons
        (lazy-caar xss)
        (lazy-concat
          (lazy-cons
            (lazy-cdar xss)
            (lazy-cdr xss))))]))

; lazy-concat-map
(define (lazy-concat-map f xs)
  (lazy-concat
    (lazy-map f xs)))

;;; infinite lists ;;;

; lazy-iterate
(define (lazy-iterate f x)
  (lazy-cons x (lazy-iterate f (f x))))

; lazy-repeat
(define (lazy-repeat x)
  (lazy-cons x (lazy-repeat x)))

; lazy-replicate
(define (lazy-replicate n x)
  (lazy-take n (lazy-repeat x)))

; lazy-cycle
(define (lazy-cycle xs)
  (lazy-concat
    (lazy-repeat xs)))

;;; sublists ;;;

; lazy-take
(define (lazy-take n xs)
  (if (and (pair? xs) (positive? n))
    (lazy-cons
      (lazy-car xs)
      (lazy-take (sub1 n) (lazy-cdr xs)))
    '()))

; lazy-drop
(define (lazy-drop n xs)
  (cond
    [(null? xs) '()]
    [(not (positive? n)) xs]
    [else (lazy-drop (sub1 n) (lazy-cdr xs))]))

; lazy-take-while
(define (lazy-take-while p xs)
  (if (and (pair? xs) (p (lazy-car xs)))
    (lazy-cons
      (lazy-car xs)
      (lazy-take-while p (lazy-cdr xs)))
    '()))

; lazy-drop-while
(define (lazy-drop-while p xs)
  (cond
    [(null? xs) '()]
    [(not (p (lazy-car xs))) xs]
    [else (lazy-drop-while p (lazy-cdr xs))]))

; lazy-split-at
(define (lazy-split-at n xs)
  (list
    (lazy-take n xs)
    (lazy-drop n xs)))

; lazy-span
(define (lazy-span p xs)
  (lazy-foldr
    (λ (x z)
      (if (p (need x))
        (list
          (lazy-cons (need x) (first (need z)))
          (second (need z)))
        (list
          '()
          (lazy-cons (need x) (second (need z))))))
    '(() ())
    xs))

;;; zipping lists ;;;

; lazy-zip
(define (lazy-zip . xss)
  (if (member '() xss)
    '()
    (lazy-cons
      (map lazy-car xss)
      (apply lazy-zip (map lazy-cdr xss)))))

; lazy-zip-with
(define (lazy-zip-with f . xss)
  (lazy-map
    (λ (ys)
      (apply f ys))
    (apply lazy-zip xss)))
