#lang slideshow

; Exercise 3.1.

(define (len-via-foldl lst) (foldl (λ (x total) (+ total 1)) 0 lst))

(len-via-foldl (list 1 2 3 4 5 6))

; Exercise 3.2.

(define (find-sum lst) (cond[(empty? lst) 0][else (+ (first lst) (find-sum (rest lst)))]))

(define (len-via-map lst) (find-sum (map (λ (x) 1) lst)))

(len-via-map (list 1 2 3 4 5 6))

; Exercise 3.3.

(define (average lst) (/ (find-sum lst) (len-via-map lst)))

(average (list 1 2 3 4 5 6))

; Exercise 3.4.

(define (my-remove-dublicates lst)
  (foldl
   (λ (x cur)
     (cond
       [(empty? (filter (λ (y) (= x y)) cur)) (append cur (list x))]
       [else cur]))
   empty lst))

(my-remove-dublicates (list 1 5 3 1 2 3 4 5 2 4))

; Exercise 3.5.

(define (my-remove-dublicates-with lst f)
  (foldl
   (λ (x cur)
     (cond
       [(empty? (filter (λ (y) (f x y)) cur)) (append cur (list x))]
       [else cur]))
   empty lst))

(my-remove-dublicates-with (list 1 5 3 1 2 3 4 5 2 4) (λ (x y) (= (modulo x 3) (modulo y 3))))

; Exercise 3.6.

; The predicate has to take 2 arguments and be either #t or #f.
; It also has to by symmetric and transitive.

; Exercise 3.7.

(define (for-range f l r) (map f (range l r)))

(for-range (λ (x) (* x x)) 5 10)

; Exercise 3.8.

(define (for-range-2D f l1 r1 l2 r2)
  (map
   (λ (x) (map f (map (λ (y) (cons x y))
               (range l2 r2))))
   (range l1 r1)))

(for-range-2D (λ (x) (* (car x) (cdr x))) 5 10 3 8)

; Exercise 3.9.

(define (render-2D f l1 r1 l2 r2)
  (apply vc-append (map
   (λ (x) (apply hc-append (map
           (λ (x)
             (cond
               [x (filled-rectangle 10 10)]
               [else (rectangle 10 10)]))
           (map f (map (λ (y) (cons x y))
               (range l2 r2))))))
   (range l1 r1))))

(render-2D (λ (x) (odd? (+ (car x) (cdr x)))) 5 10 3 8)

; Exercise 3.10.

(define (alt-map f lst) (foldl (λ (x cur) (append cur (list (f x)))) empty lst))

(alt-map (λ (x) (* x x)) (list 4 5 6 7 8))

; Exercise 3.11.

(define (alt-reverse lst) (foldl (λ (x cur) (cons x cur)) empty lst))

(alt-reverse (list 4 5 6 7 8))


