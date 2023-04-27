#lang racket

 

(list 1 2 3) ; Create a list

 

`(1 2 3) ; Also create a list

 

; (1 2 3) ; Call the 1 function on parameters 2 and 3 (NO!)

 

(define x 42)

 

(define cube1

  (lambda (x)

    (* x (* x x))))

 

(define cube2

  (lambda (x)

    (* x x x)))

 

(define pow

  (λ (base exponent)

    (if (= exponent 0)

        1

        (* base (pow base (- exponent 1))))))

 

(define (cube3 x)

  (* x x x))

 

(define (pow2 base exponent)

  (if (= exponent 0)

      1

      (* base (pow2 base (- 1 exponent)))))

 

(define pow3

  (λ (base)

    (λ (exponent)

      (if (= exponent 0)

          1

          (* base ((pow3 base) (- exponent 1)))))))

 

;(define power_of_two (pow3 2))

 

(define (sum xs)

  (if (null? xs)

      0

  (+ (car xs) (sum (cdr xs)))))

 

(define (append xs ys)

  (if (null? xs)

      ys

  (cons (car xs) (append ((cdr xs) ys)))))

 

(define (map f xs)

  (if (null? xs)

      null

  (cons (f (car xs) (map f (cdr xs))))))