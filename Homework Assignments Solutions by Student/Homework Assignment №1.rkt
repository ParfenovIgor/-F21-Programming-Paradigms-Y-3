; i.parfenov@innopolis.university
; Igor Parfenov B19-SD-01

; The program is very far from good enough and it is very simple to
; find the test, where the simplifications aren't done till the end.
; However, I have attempted to solve all given tasks.

#lang slideshow

; The definitions of base functions.

(define (variable? expr) (symbol? expr))

(define (sum? expr) (and (list? expr) (>= (length expr) 3) (equal? (first expr) '+)))
(define (summand expr n)
  (cond
    [(= n 0) (first expr)]
    [else (summand (rest expr) (- n 1))])
  )

(define (diff? expr) (and (list? expr) (equal? (length expr) 3) (equal? (first expr) '-)))
(define (minuend expr) (first (rest expr)))
(define (subtrahend expr) (first (rest (rest expr))))

(define (product? expr) (and (list? expr) (>= (length expr) 3) (equal? (first expr) '*)))
(define (multiplier expr n)
  (cond
    [(= n 0) (first expr)]
    [else (multiplier (rest expr) (- n 1))])
  )

(define (quotient? expr) (and (list? expr) (equal? (length expr) 3) (equal? (first expr) '/)))
(define (dividend expr) (first (rest expr)))
(define (divisor expr) (first (rest (rest expr))))

(define (expt? expr) (and (list? expr) (equal? (length expr) 3) (equal? (first expr) '^)))
(define (expt-base expr) (first (rest expr)))
(define (expt-degree expr) (first (rest (rest expr))))

(define (sin? expr) (and (list? expr) (equal? (length expr) 2) (equal? (first expr) 'sin)))
(define (cos? expr) (and (list? expr) (equal? (length expr) 2) (equal? (first expr) 'cos)))
(define (tan? expr) (and (list? expr) (equal? (length expr) 2) (equal? (first expr) 'tan)))
(define (log? expr) (and (list? expr) (equal? (length expr) 2) (equal? (first expr) 'log)))

(define (argument expr) (first (rest expr)))

; Finds the derivative of given function with the respect to given argument, but does not simplifies the expression.

(define (derivative expr var)
  (define (helper expr var)
    (cond
      [(number? expr) 0] ; dC/dx = 0
      [(variable? expr)
        (cond
          [(equal? expr var) 1] ; dx/dx = 1
          [else 0])] ; dx/dy = 0
      [(sum? expr) (foldl
                   (λ (x cur) (append cur (list (derivative (summand expr x) var))))
                   '(+) (range 1 (length expr)))] ; d(f+g)/dx = df/dx + dg/dx
      [(diff? expr) (list '-
                         (derivative (minuend expr) var)
                         (derivative (subtrahend expr) var))] ; d(f-g)/dx = df/dx - dg/dx
      [(product? expr) (foldl
                         (λ (x cur) (append cur (list (foldl
                                                      (λ (y cur2)
                                                        (cond
                                                          [(= x y) (append cur2 (list (derivative (multiplier expr y) var)))]
                                                          [else (append cur2 (list (multiplier expr y)))]
                                                      ))
                                                      '(*) (range 1 (length expr))))))
                       '(+) (range 1 (length expr)))] ; d(f*g)/dx = df/dx * g + f * dg/dx
      [(quotient? expr) (list '/
                              (list '-
                                    (list '* (derivative (dividend expr) var) (divisor expr))
                                    (list '* (dividend expr) (derivative (divisor expr) var)))
                              (list '^ (divisor expr) 2))] ; d(f/g)/dx =(df/dx * g + f * dg/dx) / g^2
      [(expt? expr) (list '* expr (list '* (list 'log (expt-base expr)) (derivative (expt-degree expr) var)))] ; d(a^x)/dx = ln a * a^x
      [(sin? expr) (list '* (list 'cos (argument expr)) (derivative (argument expr) var))] ; d(sin x)/dx = cos x
      [(cos? expr) (list '* '-1 (list '* (list 'sin (argument expr)) (derivative (argument expr) var)))] ; d(cos x)/dx = -sin x
      [(tan? expr) (list '* (list '/ '1 (list '^ (list 'cos (argument expr)) '2)) (derivative (argument expr) var))] ; d(tan x)/dx = 1 / (cos x)^2
      [(log? expr) (list '* (list '/ '1 (argument expr)) (derivative (argument expr) var))])) ; d(log x)/dx = 1/x
    (helper expr var))

; Uses primitive simplifucation rules to simplify the primitive level of expression.

(define (simplify-at-root expr)
  ; Finds sum of numbers in sum.
  (define (sum-combine-numbers expr)
    (cond
      [(empty? expr) 0]
      [else (cond
              [(number? (first expr)) (+ (first expr) (sum-combine-numbers (rest expr)))]
              [else (sum-combine-numbers (rest expr))])]))

  ; Removes numbers in expression.
  (define (remove-numbers expr)
    (foldl
     (λ (x cur) (cond
                  [(number? x) cur]
                  [else (append cur (list x))]))
     empty expr))

  ; Adds all number-addends in given sum.
  (define (sum-simplify-numbers expr)
    (define (helper expr)
      (cond
        [(= (sum-combine-numbers expr) 0) (cond
                                           [(= (length (remove-numbers expr)) 1) '0]
                                           [(= (length (remove-numbers expr)) 2) (first (rest (remove-numbers expr)))]
                                           [else (remove-numbers expr)])]
        [else (cond
                [(= (length (remove-numbers expr)) 1) (sum-combine-numbers expr)]
                [else (append (list '+ (sum-combine-numbers expr)) (rest (remove-numbers expr)))])]))
    (helper (map (λ (x) (simplify x)) expr)))

  ; Finds product of numbers in product.
  (define (product-combine-numbers expr)
    (cond
      [(empty? expr) 1]
      [else (cond
              [(number? (first expr)) (* (first expr) (product-combine-numbers (rest expr)))]
              [else (product-combine-numbers (rest expr))])]))

  ; Multiplies all number-multipliers in given product.
  (define (product-simplify-numbers expr)
    (define (helper expr)
       (cond
        [(= (product-combine-numbers expr) 0) '0]
        [(= (product-combine-numbers expr) 1) (cond
                                               [(= (length (remove-numbers expr)) 1) '0]
                                               [(= (length (remove-numbers expr)) 2) (first (rest (remove-numbers expr)))]
                                               [else (remove-numbers expr)])]
        [else (cond
                [(= (length (remove-numbers expr)) 1) (product-combine-numbers expr)]
                [else (append (list '* (product-combine-numbers expr)) (rest (remove-numbers expr)))])]))
    (helper (map (λ (x) (simplify x)) expr)))

  ; Simplification using basic operations properties.
  (cond
    [(sum? expr) (sum-simplify-numbers expr)]
    [(and (diff? expr) (equal? (minuend expr) '0)) (list '* '-1 (subtrahend expr))]
    [(and (diff? expr) (equal? (subtrahend expr) '0)) (minuend expr)]
    [(product? expr) (product-simplify-numbers expr)]
    [(and (product? expr) (foldl (λ (x cur) (cond[(equal? x 0) #t][else cur])) #f expr)) '0]
    [(and (quotient? expr) (equal? (divisor expr) '1)) (dividend expr)]
    [(and (quotient? expr) (equal? (dividend expr) '0) (not (equal? (divisor expr) 0))) '0]
    [(and (expt? expr) (equal? (expt-base expr) '0) (not (equal? (expt-degree expr) 0))) '0]
    [(and (expt? expr) (equal? (expt-degree expr) '0) (not (equal? (expt-degree expr) 0))) '1]
    [(and (expt? expr) (equal? (expt-degree expr) '1)) (expt-base expr)]

    ; If all arguments are numbers, calculate the function.
    [(and (diff? expr) (number? (minuend expr)) (number? (subtrahend expr)))
     (- (minuend expr) (subtrahend expr))]
    [(and (quotient? expr) (number? (dividend expr)) (number? (divisor expr)))
     (/ (dividend expr) (divisor expr))]
    [(and (expt? expr) (number? (expt-base expr)) (number? (expt-degree expr)))
     (expt (expt-base expr) (expt-degree expr))]
    [(and (sin? expr) (number? (argument expr)))
     (sin (argument expr))]
    [(and (cos? expr) (number? (argument expr)))
     (cos (argument expr))]
    [(and (tan? expr) (number? (argument expr)))
     (tan (argument expr))]
    [(and (log? expr) (number? (argument expr)))
     (log (argument expr))]
    
    [else expr]
    ))

; Simplifies given expression.

(define (simplify expr)
  (define (simplify-rem expr simplified-expr)
    (cond[(equal? expr simplified-expr) expr][else (simplify simplified-expr)]))

  (define (simplify-full expr s-expr)
    (cond
      [(equal? expr s-expr)
       (cond
         [(sum? expr) (simplify-rem expr (foldl (λ (x cur) (cond[(equal? x '+) (append cur '(+))][else (append cur (list (simplify x)))])) empty expr))]
         [(diff? expr) (simplify-rem expr (append '(-) (list (simplify (minuend expr))) (list (simplify (subtrahend expr)))))]
         [(product? expr) (simplify-rem expr (foldl (λ (x cur) (cond[(equal? x '*) (append cur '(*))][else (append cur (list (simplify x)))])) empty expr))]
         [(quotient? expr) (simplify-rem expr (append '(/) (list (simplify (dividend expr))) (list (simplify (divisor expr)))))]
         [(expt? expr) (simplify-rem expr (append '(^) (list (simplify (expt-base expr))) (list (simplify (expt-degree expr)))))]
         [else expr])]
      [else (simplify s-expr)]))

  (simplify-full expr (simplify-at-root expr)))

; Sorts list of symbols.

(define (sort-symbols lst)
  (sort lst
    (λ (x y)
    (string<? (symbol->string x) (symbol->string y)))))

; Compares two lists.

(define (expr-less a b)
  (cond
    [(and (integer? a) (integer? b)) (< a b)]
    [(integer? a) a]
    [(integer? b) b]
    [(and (symbol? a) (symbol? b)) (string<? (symbol->string a) (symbol->string b))]
    [(symbol? a) #t]
    [(symbol? b) #f]
    [(not (list? a)) #t]
    [(not (list? b)) #f]
    [(empty? b) #f]
    [(empty? a) #t]
    [(expr-less (first a) (first b)) #t]
    [(expr-less (first b) (first a)) #f]
    [else (expr-less (rest a) (rest b))]
    ))

; Sorts two lists using appropriate comparator.

(define (sort-expr lst)
  (sort lst expr-less))

; Normalizes the expression, uses disributive properties to eliminate addends and multipliers, adds similar addends, but does not simplifies the expression.

(define (normalize expr)  
  ; Expands first bracket with sum in product, given that first multiplier is sum.
  (define (normalize-product expr) (cond
                                     [(sum? (first (rest expr)))
                                      (normalize (foldl (λ (x cur)
                                               (cond
                                                 [(equal? x '+) cur]
                                                 [else (append cur (list (append (list '* x)
                                                                                 (foldl (λ (y cur2) (append cur2 (list y))) empty (rest (rest expr)))
                                                                                 )))]))
                                       '(+) (first (rest expr))))]
                                     [else expr]))

  ; Expands all levels of sums in brackets.
  (define (propagate-sums expr)
    (foldl (λ (x cur) (cond
                        [(equal? x '+) cur]
                        [(sum? (normalize x)) (append cur (foldl (λ (y cur2) (cond
                                                                               [(equal? y '+) cur2]
                                                                               [else (append cur2 (list y))]))
                                                            empty (normalize x)))]
                        [else (append cur (list (normalize x)))])) '(+) expr))

  ; Expands all levels of products in brackets.
  (define (propagate-products expr)
    (foldl (λ (x cur) (cond
                        [(equal? x '*) cur]
                        [(product? (normalize x)) (append cur (foldl (λ (y cur2) (cond
                                                                               [(equal? y '*) cur2]
                                                                               [else (append cur2 (list y))]))
                                                            empty (normalize x)))]
                        [else (append cur (list (normalize x)))])) '(*) expr))

  ; Multiplies all numbers in product and puts the their product to the beginning.
  (define (eliminate-numbers-in-product expr)
    (foldl
     (λ (x cur) (cond[(or (equal? x '*) (integer? x)) cur][else (append cur (list x))]))
     (list '* (foldl (λ (x cur) (cond[(integer? x) (* x cur)][else cur])) 1 expr))
     expr))

  ; Adds similar products in sums given they are sorted by following.
  ; Scans all addends: if there is no such addend in resulting list, adds it to resulting
  ; list after calculating the sum of coefficients of equal addends in input list.
  (define (add-similar-members expr)
    (foldl
     (λ (x cur) (cond[(product? x)
                      (cond
                        [(foldl (λ (y cur2) (cond[(and (product? y) (equal? (rest (rest x)) (rest (rest y)))) #f][else cur2])) #t cur)
                         (append cur (list (append (list '* (foldl (λ (y cur2) (cond[(and (product? y) (equal? (rest (rest x)) (rest (rest y)))) (+ cur2 (first (rest y)))][cur2])) 0 expr)) (rest (rest x)))))]
                        [else cur])]
                     [else (append cur (list x))]))
     empty expr))
  
  (cond
    [(sum? expr) (add-similar-members (foldl (λ (x cur) (cond[(product? x) (append cur (list (eliminate-numbers-in-product (sort-expr x))))][else (append cur (list x))])) empty (propagate-sums expr)))]
    [(diff? expr) (append '(+) (list (normalize (minuend expr))) (list (append '(*) (list (normalize (subtrahend expr))) '(-1))))]
    [(product? expr) (normalize-product
                      (cons '* (foldl (λ (x cur) (cond
                                                   [(equal? x '*) cur]
                                                   [(sum? x) (append (list x) cur)]
                                                   [else (append cur (list x))])) empty (propagate-products expr))))]
    [(and (expt? expr) (integer? (expt-degree expr)) (> (expt-degree expr) 0)) (normalize (foldl (λ (x cur) (append cur (list (expt-base expr)))) '(*) (range 0 (expt-degree expr))))]
    [else expr])
  )

; Rewrites the expression into infix notation, but does not simplifies the expression.

(define (to-infix expr)
  (cond
    [(sum? expr) (rest (foldl (λ (x cur) (cond[(equal? x '+) cur][else (append cur (append '(+) (list (to-infix x))))])) empty expr))]
    [(diff? expr) (list (to-infix (minuend expr)) '- (to-infix (subtrahend expr)))]
    [(product? expr) (rest (foldl (λ (x cur) (cond[(equal? x '*) cur][else (append cur (append '(*) (list (to-infix x))))])) empty expr))]
    [(quotient? expr) (list (to-infix (dividend expr)) '/ (to-infix (divisor expr)))]
    [(expt? expr) (list (to-infix (expt-base expr)) '^ (to-infix (expt-degree expr)))]
    [(sin? expr) (list 'sin (to-infix (first (rest expr))))]
    [(cos? expr) (list 'cos (to-infix (first (rest expr))))]
    [(tan? expr) (list 'tan (to-infix (first (rest expr))))]
    [(log? expr) (list 'log (to-infix (first (rest expr))))]
    [else expr]))

; Finds all variables in expression.

(define (variables-of expr)
  (define (helper expr)
    (cond
      [(variable? expr) (list expr)]
      [(sum? expr) (foldl (λ (x cur) (cond[(equal? x '+) cur][else (append cur (helper x))])) empty expr)]
      [(diff? expr) (append (helper (minuend expr)) (helper (subtrahend expr)))]
      [(product? expr) (foldl (λ (x cur) (cond[(equal? x '*) cur][else (append cur (helper x))])) empty expr)]
      [(quotient? expr) (append (helper (dividend expr)) (helper (divisor expr)))]
      [(expt? expr) (append (helper (expt-base expr)) (helper (expt-degree expr)))]
      [(sin? expr) (helper (argument expr))]
      [(cos? expr) (helper (argument expr))]
      [(tan? expr) (helper (argument expr))]
      [(log? expr) (helper (argument expr))]
      [else empty]))
  (sort-symbols (foldl (λ (x cur) (cond
                                    [(ormap (λ (y) (equal? x y)) cur) cur]
                                    [else (cons x cur)])) empty (helper expr)))
  )

; Calculate the gradient of function with the respect to given variables and simplifies the expression.

(define (gradient expr var)
  (foldl (λ (x cur) (append cur (list (simplify (derivative expr x))))) empty var))
