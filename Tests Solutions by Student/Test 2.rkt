#lang racket

; Task 1

(define (alternating-sum lst) (cond[(empty? lst) 0][else (- (first lst) (alternating-sum (rest lst)))]))

(alternating-sum (list 6 2 4 1 3 9))

; Task 2

(define (dec n) (- n 1))

(define (f n) (cond[(<= n 2) (- 3 n)][else (+ (f (dec n)) (f (dec (dec n))))]))

(f 3)

; (f 3) ->
; (cond[(<= 3 2) (- 3 2)][else (+ (f (dec 3)) (f (dec (dec 3))))]) ->
; (+ (f (dec 3)) (f (dec (dec 3)))) ->
; (+ (f (- 3 1)) (f (dec (dec 3)))) ->
; (+ (f 2) (f (dec (dec 3)))) ->
; (+ (cond [(<= 2 2) (- 3 2)][else (+ (f (dec 2)) (f (dec (dec 2))))]) (f (dec (dec 3)))) ->
; (+ (- 3 2) (f (dec (dec 3)))) ->
; (+ 1 (f (dec (dec 3)))) ->
; (+ 1 (f (dec 2))) ->
; (+ 1 (f 1)) ->
; (+ 1 (cond [(<= 1 2) (- 3 1)][else (+ (f (dec 1)) (f (dec (dec 1))))])) ->
; (+ 1 (- 3 1)) ->
; (+ 1 2) ->
; 3
