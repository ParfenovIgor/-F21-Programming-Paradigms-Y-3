#lang slideshow

(define employees
  '(("John" "Malkovich" . 29)
    ("Anna" "Petrova" . 22)
    ("Ivan" "Ivanov" . 23)
    ("Anna" "Karenina" . 40)))

; 1

(define (fullname x) (cons (car x) (car (cdr x))))

(fullname '("John" "Malkovich" . 29))

; 2

(define (filter-by-name name lst) (filter (λ (x) (equal? (car x) name)) lst))

(filter-by-name "Anna" employees)

; 3

(define (employees-over-25 lst) (map (λ (x) (fullname x)) (filter (λ (x) (> (cdr (cdr x)) 25)) lst)))

(employees-over-25 employees)
