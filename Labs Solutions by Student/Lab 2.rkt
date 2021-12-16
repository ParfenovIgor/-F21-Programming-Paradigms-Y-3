#lang slideshow

; Introduction

(define (sqr x) (* x x) )

(define (greater x y)
  (cond
    [(<= x y) (* x 2)]
    [else (* x 3)]
   )
 )

(define (getFirst x)
  (cond
    [(empty? x) empty]
    [else (first x)]
   )
 )

(define (nth lst x)
  (cond
    [(= x 0) (first lst)]
    [else (nth (rest lst) (- x 1))]
   )
 )

(define (sumpref lst x)
  (cond
    [(= x 0) (first lst)]
    [else (+ (first lst) (sumpref (rest lst) (- x 1)))]
   )
 )

(define (prefsum a)
  (define (calculate a x)
    (cond
      [(empty? a) (list x)]
      [else (cons x (calculate (rest a) (+ x (first a))))]
     )
   )
  (calculate a 0)
 )

(define (get-element a x)
  (cond
    [(= x 0) (first a)]
    [else (get-element (rest a) (- x 1))]
   )
 )

(define (get-element-2d a x y)
  (cond
    [(= x 0) (get-element (first a) y)]
    [else (get-element-2d (rest a) (- x 1) y)]
   )
 )

; Warm-up exercise: plot data series as a histogram

(define (ex-sin x)
  (abs (+ 100 (* 50 (sin x))))
 )

(define (plot-bars data)
  (cond
    [(empty? data) empty]
    [else (cons (filled-rectangle 10 (first data)) (plot-bars (rest data)))]
   )
 )

; Conway’s Game of Life

(define alive 1)
(define dead 0)

(define conway-example
  (lambda (i j)
    (cond
      [(and (< (abs i) 3)
            (< (abs j) 3)
            (odd? (+ i j)))
       alive]
      [else dead]
     )
   )
 )

(define (count function i j step)
   (+ (step-function function (- i 1) (- j 1) (- step 1))
   (+ (step-function function (- i 1) j (- step 1))
   (+ (step-function function (- i 1) (+ j 1) (- step 1))
   (+ (step-function function i (- j 1) (- step 1))
   (+ (step-function function i (+ j 1) (- step 1))
   (+ (step-function function(+ i 1) (- j 1) (- step 1))
   (+ (step-function function (+ i 1) j (- step 1))
      (step-function function (+ i 1) (+ j 1) (- step 1))
  )))))))
 )

(define step-function
  (lambda (function i j step)
    (cond
      [(= step 0) (function i j)]
      [else
        (cond
          [(= (step-function function i j (- step 1)) alive) (cond
                                  [(and (>= (count function i j step) 2) (<= (count function i j step) 3) 1) alive]
                                  [else dead]
                                 )]
          [(= (step-function function i j (- step 1)) dead) (cond
                                  [(= (count function i j step) 3) alive]
                                  [else dead]
                                 )]
         )
       ]
     )   
   )
 )

(define (square x)
  (cond
    [(= x alive) (vc-append (filled-rectangle 10 10))]
    [(= x dead) (vc-append (rectangle 10 10))]
   )
 )

(define (render-conway function)
  (define (iterateX x)
    (cond
      [(<= x 5) (cons (iterateY x -5) (iterateX (+ x 1)))]
      [else empty]
     )
   )
  (define (iterateY x y)
    (cond
      [(<= y 5) (cons (square (function x y)) (iterateY x (+ y 1)))]
      [else empty]
     )
   )
  (iterateX -5)
 )

(define (render-conway-steps function nsteps)
  (define (iterateX x)
    (cond
      [(<= x 5) (cons (iterateY x -5) (iterateX (+ x 1)))]
      [else empty]
     )
   )
  (define (iterateY x y)
    (cond
      [(<= y 5) (cons (square (step-function function x y (- nsteps 1))) (iterateY x (+ y 1)))]
      [else empty]
     )
   )
  (cond
    [(= nsteps 0) empty]
    [else (cons (render-conway-steps function (- nsteps 1)) (iterateX -5))]
   )
 )

; 1.11

(define (f-1-11-recursive n)
  (cond
    [(< n 3) n]
    [else (+ (f-1-11-recursive (- n 1)) (+ (* 2 (f-1-11-recursive (- n 2))) (* 3 (f-1-11-recursive (- n 3)))))]
   )
 )

(define (f-1-11-iterative-func n n1 n2 n3)
  (cond
    [(= n 0) n3]
    [else (f-1-11-iterative-func (- n 1) n2 n3 (+ n3 (+ (* 2 n2) (* 3 n1))))]
   )
 )

(define (f-1-11-iterative n)
  (cond
    [(< n 3) n]
    [else (f-1-11-iterative-func (- n 2) 0 1 2)]
   )
 )

; 1.16

(define (binary-power-func a b n)
  (cond
    [(= b 0) n]
    [(odd? b) (binary-power-func a (- b 1) (* n a))]
    [(even? b) (binary-power-func (* a a) (/ b 2) n)]
   )
 )

(define (binary-power a b)
  (binary-power-func a b 1)
 )
