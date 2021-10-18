#lang racket

;; What is a REPL?
(+ 3 5)

;; Simplest FN ever
(define (plus n m)
  (+ n m ))


;; Exercise 1:
;; Writ an fn - compare-two that takes two arguments
(define (compare-two? n m)
  (> n m ))


;; What is a predicate?
;;(compare-two? 30 6)


;; Exercise 2:
;; in-order? - 3 arguments, true if all 3 are in descending order.

(define (in-order? n m v)
  (> n m v))

;;(in-order? 7 4 2)

;;(+ 1 2 3 4 5 6 )


;; Conditionals
(if #t
    5
    3)

;; Just keep using cond, it's whatever.
(cond
 [#t 5]
 [else 3])


;; Exercise 3:
;; Write factorial.
(define (fact n)
  (cond
   [(= n 1) 1]
   [else (* n (fact (- n 1)))]))

;;(fact 5)

;; Exercise 4
;; Fibonacci
(define (fib n)
  (cond
   [(= n 1) 1]
   [(= n 2) 1]
   [else (+ (fib (- n 1)) (fib (- n 2)))]))


;;(fib 17)


;; Exercise 5
;; Reverse digits reeverse-digits 4321 -> 1234
(define (reverse-digits n)
  (define (reverse-digits-helper acc n)
    (cond
     [(= n 0) acc]
     [else (reverse-digits-helper (+ (* acc 10) (remainder n 10))
                                  (quotient n 10))]))
  (reverse-digits-helper 0 n))

;;(reverse-digits 4321)


;; Remainder & Quotient
(remainder 10 3)
(quotient 4321 10)
(quotient 7 10)
