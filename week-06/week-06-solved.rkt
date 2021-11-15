#lang racket



;;


;; Exercise 1: - Write a function that accepts a list of booleans and returns true only if all of them are true.
;; Don't write this recursively.


;;(all-true? '(#t #t #t #t)) => #t
;;(all-true? '(#t #f #t)) => #f

;; Works
(and #t #t #t)

;; Doesn't work.
;;(apply and '(#t #t #t))
;;
(andmap identity '(#t #t #t))
(ormap identity '(#f #f #f))


;; Exercise 2: Write a function that returns true only if ALL elements in a collection are the same. Norecursion.

;; (all-equal? '(1 1 1)) => #t
;; (all-equal? '(1 2 1)) => #f

;; Works
(equal? "a" "a")

;;Doesn't
;;(equal? "a" "a" "a")

;; Does not work
;;(define (all-equal? xs)
;; (apply equal? xs))

;; What was curry?
;; From a function of N arguments, transform into a function of N - M arguments.
(define (all-equal? xs)
  (andmap (curry equal? (car xs)) xs))

;; What is a matrix - pravougulna tablica ot chisla.
;; Spisuk ot spisuci

;; Exercsie 3: Create a function that verifies if a nested list is a matrix.

(matrix? (list '(1 2)
               '(2 1)))

(matrix? (list '(1 2)
               '(2 1 3)))


(define (matrix? xss)
  (all-equal? (map length xss)))

;; Exercise 4: Transpose a matrix -
(define (transpose xss)
  (apply map list xss))


;; Exercise 5: Receive a number, reorder it into all it's digits in ascending order.
;; (in-order-digits 4423321) => 1223344


(define (in-order-digits n)
  (define (helper current-num xs)
    (cond
     [(zero? current-num) xs]
     [else (helper (quotient current-num 10) (cons (remainder current-num 10) xs))]))
  (sort (helper n '()) <))




;; Exercise Next:
;; Let's write a function of two arguments - the first is a linear graph in the form of y-axis coordinates,
;; the second is a specific y axis coordinate. The question is: How many times does the graph cross the dot?
;;

;;(count-crosses '(0 2 4 1 5) 3) => 3

|                      5
|            4
|
|--------------------------
|      2
|                 1
||0



(define (count-crosses xss n)
  (define (helper current-list count)
    (cond [(= 1 (length current-list)) count]
          [(or (and (< n )))])))


;; Exercise Next: Create a function that accepts a number N and returns a new function also accepting a number M.
;; The result of the created function is a new number X, containing all digits of M *not present*
;; in N. (I guess in order)



((digit-filter 123) 4427651) => 44765

(define (digit-filter n)
  (define (n->list current-num xs)
    (cond
     [(zero? current-num) xs]
     [else (n->list (quotient current-num 10) (cons (remainder current-num 10) xs))]))
  (define n-digits (n->list n '()))
  (define (helper current result)
    (cond
     [(null? current) (reverse result)]
     [(member (car current) n-digits) (helper (cdr current) result)]
     [else (helper (cdr current) (cons (car current) result))]))
  (lambda (subject)
    (helper (n->list subject '()) '())))

((digit-filter 1243) 44255634)
