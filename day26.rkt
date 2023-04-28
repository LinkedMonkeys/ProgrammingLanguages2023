#lang racket

(define my_tree '(((() 4 ()) 2 (() 5 ())) 1 (() 3 (() 6 ()))) )

(define (tree_sum t)
  (cond [(null? t) 0]
        [#t (+ (tree_sum (car t)) (cadr t) (tree_sum (caddr t)))]))

(tree_sum my_tree)

(define (max xs)
  (cond [(null? xs) 99999999]
        [(null? (cdr xs)) (car xs)]
        [(> (car xs) (max (cdr xs))) (car xs)]
        [#t (max (cdr xs))]))

(max '(1 42 99 19 8 4 2 92))

; Assuming xs has at least one value.
;(define (max_tail xs acc)
;  (cond [(null? xs) acc]
;        [(> (car xs) acc) (max_tail (cdr xs) (car xs))]
;        [#t (max_tail (cdr xs) acc)]))
;(define my_list '(1 42 99 19 8 4 2 92))
;(max_tail (cdr my_list) (car my_list))

;(define (good_max xs)

(define (f x)
  (let ([y (+ 1 41)]
        [z 99])
    (+ x y z)))

(f 10)


(define (g x)
  (let ([increment (lambda (z) (+ z 1))])
    (increment x)))

(g 42)

(define (good_max xs)
  (letrec ([max_tail (lambda (xs acc)
                       (cond [(null? xs) acc]
                             [(> (car xs) acc) (max_tail (cdr xs) (car xs))]
                             [#t (max_tail (cdr xs) acc)]))])
    (max_tail (cdr xs) (car xs))))
(define my_list '(1 42 99 19 8 4 2 92))
(good_max my_list)