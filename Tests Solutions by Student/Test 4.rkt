#lang slideshow

; 1

(define (replicate n x)
  (cond
    [(= n 0) empty]
    [else (cons x (replicate (- n 1) x))]))

(replicate 10 'a)
(replicate 3 '(1 . 2))

; 2

(define (encode lst)
  (define (helper lst x)
    (cond
      [(equal? (length lst) 1) (list (cons (first lst) 1))]
      [(equal? (first lst) (first (rest lst))) (prerend (helper (rest lst) (first lst)))]
      [else (cons (cons (first lst) 1) (helper (rest lst) (first lst)))]))
  
  (define (prerend x)
    (cons (cons (car (first x)) (+ (cdr (first x)) 1)) (rest x)))

  (cond
    [(empty? lst) empty]
    [else (helper lst '!)]) ; Some symbol, that is not used in input
  )

(encode '(a a b c c c a a a))

; 3

(define (decode lst)
  (foldl (λ (x cur) (foldl (λ (y cur2) (append cur2 (list (car x)))) cur (range 0 (cdr x)))) empty lst))

(decode '((a . 2) (b . 1) (c . 3) (a . 3)))