#lang slideshow

; Exercise 4.1.

(define (attacks? a b) (or
                        (= (car a) (car b))
                        (= (cdr a) (cdr b))
                        (= (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b))))
                        ))

; Exercise 4.2.

(define (attacks-any? q lst) (ormap (λ (x) (attacks? q x)) lst))

; Exercise 4.3.

(define (no-attacks? lst)
  (= (length
    (foldl
      (λ (x cur)
        (cond
          [(attacks-any? x cur) cur]
          [else (cons x cur)]))
      empty lst))
    (length lst)))

; Exercise 4.4.

(define (for-range f l r) (map f (range l r)))

; Exercise 4.5.

(define (naive-four-queens)
  (filter no-attacks? (apply append (for-range (λ (x1)
    (apply append (for-range (λ (x2)
      (apply append (for-range (λ (x3)
        (for-range (λ (x4)
          (list (cons 1 x1) (cons 2 x2) (cons 3 x3) (cons 4 x4)))
        1 5))
      1 5)))
    1 5)))
  1 5)))
  )


(naive-four-queens)

; Exercise 4.6.

; Iterate the positions of another queen except the cells, which are
; captured by some another queen already brought on prefix.

; Exercise 4.7.

(define (add-queen-at lst y n)
  (filter
    (λ (cur) (no-attacks? cur))
    (for-range (λ (x) (cons (cons x y) lst)) 1 (+ n 1))
  ))

(add-queen-at (car (naive-four-queens)) 5 5)

; Exercise 4.8.

(define (eight-queens)
  (define (helper lst pos)
    (cond
      [(= pos 9) lst]
      [else
        (helper (foldl (λ (x cur) (append cur (add-queen-at x pos 8))) empty lst) (+ pos 1))
      ])
    )
  (helper (list empty) 1))

(eight-queens)

; Exercise 4.9.

(define (n-queens n)
  (define (helper lst pos)
    (cond
      [(= pos (+ n 1)) lst]
      [else
        (helper (foldl (λ (x cur) (append cur (add-queen-at x pos n))) empty lst) (+ pos 1))
      ])
    )
  (helper (list empty) 1))

(n-queens 5)

; Exercise 4.10.

(require racket/generator)


(define (for-range-gen f l r)
  (map
    (λ (x) (begin (yield (f x)) (f x)))
  (range l r)))

(define for-range-gen-next (generator () (for-range-gen (λ (x) (* x x)) 3 8)))

(for-range-gen-next)
(for-range-gen-next)
(for-range-gen-next)
(for-range-gen-next)
(for-range-gen-next)
(for-range-gen-next)

; Exercise 4.11.

(define (add-queen-at-gen lst y n)
  (define for-range-gen-here (generator () (for-range-gen (λ (x) (cons (cons x y) lst)) 1 (+ n 1))))
  (filter
    (λ (cur) (no-attacks? cur))
    (map (λ (x) (for-range-gen-here)) (range 1 (+ n 1)))
  ))

(define (eight-queens-gen)
  (define (helper lst pos)
    (cond
      [(= pos 9) lst]
      [else
        (helper (foldl (λ (x cur) (append cur (add-queen-at-gen x pos 8))) empty lst) (+ pos 1))
      ])
    )
  (map
    (λ (x) (begin (yield x) x))
  (helper (list empty) 1)))

(define eight-queens-gen-next (generator () (eight-queens-gen)))

(eight-queens-gen-next)
(eight-queens-gen-next)
(eight-queens-gen-next)
(eight-queens-gen-next)
