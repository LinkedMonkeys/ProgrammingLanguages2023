#lang racket

; Is (if e1 e2 e3) a function?

(define (my_if e1 e2 e3)
  (if e1
      e2
      e3))

;(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) ; 1

;(define (fact n) (if (= n 0) '(1) ((* n (fact (- n 1))))) ; 2

;(define (fact n) (if = n 0 1 (* n (fact (- n 1))))) ; 3
;
;(define fact (n) (if (= n 0) 1 (* n (fact (- n 1))))) ; 4
;
;(define (fact n) (if (= n 0) 1 (* n fact (- n 1)))) ; 5
;
;(define (fact n) (if (= n 0) 1 (* n ((fact) (- n 1))))) ; 6
;
;(define (fact n) (if (= n 0) 1 (n * (fact (- n 1))))) ; 7

(define my_list '(1 2 (3 4) () (5 6 (7 (8 9) 10)) 11))

(define (sum xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum (cdr xs)))
          (+ (sum (car xs)) (sum (cdr xs))))))

(define (sum2 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum2 (cdr xs)))]
        [#t (+ (sum2 (car xs)) (sum2 (cdr xs)))]))


(define (flatten xs)
  (cond [(null? xs) null]
        [(number? (car xs)) (cons (car xs) (flatten (cdr xs)))]
        [#t (append (flatten (car xs)) (flatten (cdr xs)))]))


(flatten my_list)







