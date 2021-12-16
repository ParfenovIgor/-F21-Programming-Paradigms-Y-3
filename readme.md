# [F21] Programming Paradigms Y-3

## Course outline

* Lambda Calculus
* Racket
* Haskell
* Prolog

[Course Outline](https://docs.google.com/spreadsheets/d/1VPd8rdu_5SfPqgZrTYCrGrIKd2mOKsxOI_J7EIrarHQ/edit#gid=0)

[Lecture Recordings](https://www.youtube.com/playlist?list=PLov3NSwpY86eMWcSLVDi071ertPAA22Dr)

[Final Exam Questions](https://docs.google.com/spreadsheets/d/1mlgcu1c54fJR5M7MFuieSsc7jowtdUI1QCdR1TobOGg/edit#gid=0)

## Lambda Calculus

[Wikipedia](https://en.wikipedia.org/wiki/Lambda_calculus)

[Calculator](https://lambdacalc.io/)

[Bonus Interpreter](https://github.com/ParfenovIgor/LCP)

``` Haskell
c0 := \s. \z. z
c1 := \s. \z. s z
c2 := \s. \z. s (s z)

inc := \n. \s. \z. n s (s z)
sum := \m. \n. \s. \z. m s (n s z)
prd := \m. \n. m (sum n) c0
pow := \m. \n. m (prd n) c1
```

## Racket

[Wikipedia](https://en.wikipedia.org/wiki/Racket_(programming_language))

[Download IDE](https://download.racket-lang.org/)

``` Lisp
#lang slideshow

(define (twice f x) (f (f x)))

(define (apply-twice f lst)
    (cond
        [(empty? lst) '()]
        [else (cons (twice f (first lst)) (apply-twice f (rest lst)))]))

(apply-twice (λ (x) (* x x)) '(1 2 3))
```

## Haskell

[Wikipedia](https://en.wikipedia.org/wiki/Haskell_(programming_language))

[Online IDE](https://code.world/haskell#)

Haskell does not have IDE. Here is what author used:
1. Download [GHC compiler](https://www.haskell.org/ghc/download.html)
2. Download [Visual Studio Code](https://code.visualstudio.com/)
3. Download in Visual Studio Code the **Haskell language support**

``` Haskell
twice :: (t -> t) -> Maybe t -> Maybe t
twice _ Nothing = Nothing
twice f (Just x) = Just (f (f x))

apply_twice :: (t -> t) -> [Maybe t] -> [Maybe t]
apply_twice _ [] = []
apply_twice f (x:xs) = twice f x : apply_twice f xs

main :: IO ()
main = print (apply_twice (\x -> x * x) [Just 1, Nothing, Just 3])
```

## Prolog

[Wikipedia](https://en.wikipedia.org/wiki/Prolog)

[Online IDE](https://swish.swi-prolog.org/)

``` Haskell
swap_pairs([], []).
swap_pairs([A], [A]).
swap_pairs([A,B|C], [B,A|D]) :- swap_pairs(C, D).

nth([A|_], 0, A) :- !.
nth([_|A], N, B) :- M is N - 1, nth(A, M, B).

prefix(_, []).
prefix([A|B], [A|C]) :- prefix(B, C).

subarray([_|A], B) :- subarray(A, B).
subarray(A, B) :- !, prefix(A, B). 
```
