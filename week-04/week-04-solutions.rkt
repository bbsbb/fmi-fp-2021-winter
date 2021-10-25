#lang racket

;; What is let??

;; let ............

;; Define sequential unconnected variables
(let ([a 5])
  (+ a 3))


;; Define sequential dependant variables
(let* ([a 5])
  (+ a b))

;; Allows to define mutually recursive dependants
(letrec)

;;; So, let --- not very interesting.


;;; Let's talk about collections.

(list 1 2 3)

'(1 2 3)

(equal? '(1 2 3) (list 1 2 3))


(equal? (list 1 2 3) (cons 1 (cons 2 3)))

(cons 1 (cons 2 3))

;; Empty list is null
(eq? null '())

;; FN's to maniluate lists
(define our-l '(1 2 3))

;;(car our-l) => 1
;;(cdr our-l) => '(2 3)
;;(cddr our-l) => '(3)
;;(caaadr our-l) => ...please don't use

(length our-l)
;;(empty? '())

;; Is our list empty? Then it's null.
;; Spoilers: We probably stop recursions like that....
(null? '())

;; wtf, can't add to beginning of list?!!?
(append '(1) '(2 3))
;; wtf, can't add to beginning of list?!!?
;;(append '(1) 2)


(cons 1 '(2))

;; Let's write a length function that returns the lenght of a list.

(define (our-length l)
  (cond
   [(null? l) 0]
   [else (+ 1 (our-length (cdr l)))]))

(our-length '(1 2 3 4))
(our-length '())

;; Exercise 2
;; Write a function that accepts a 1-parameter-fn and a list and returns
;; a new list with the fn applied to all arguments in the original parameters
;; Example: increment + '(1 2 3) => '(2 3 4)
;; Spoilers: It's a higher order function.
(define (apply-to-list fn l)
  (cond [(null? l)]
        [else (cons (fn (car l)) (apply-to-list fn (cdr l)))]))

(apply-to-list (lambda (x) x))


;; Lambdas can be defined ANYWHERE
(let* ([a 1]
       [fn (lambda (n) (+ a n))])
  (fn 5))


;; Exercise 3
;; Let's write a fn of two parameters - a value + a list that returns the number of times
;; the value occurred in the list.
;; e.g. 5 '(1 2 4 5 3 5 3) => 2

(define (count-v v l)
  (cond [(null? l) 0]
        [(= (car l) v) (+ 1 (count-v v (cdr l)))]
        [else (count-v v (cdr l))]))

(count-v 5 '(1 2 3 5))


;;(count-v 5 '(1 2 3 4 5 5)) => 2


(and #t 5)

(or #f 7)


;; Exercise 4:
;; Create a function that accepts a collection and dedupes all items within it.
;; e.g. (dedup-all '(1 2 3 4 "wut" 3 3 7 4)) => '(1 2 3 4 "wut" 7)

(define (member? v l)
  (and (member v l) #t))

(define (dedup-all l)
  (define (dedup-helper acc rest)
    (cond
     [(null? rest) (reverse acc)]
     [(not (member? (car rest) acc)) (dedup-helper (cons (car rest) acc) (cdr rest))]
     [else (dedup-helper acc (cdr rest))]))
  (dedup-helper null l))


(dedup-all '(1 2 3 3 3 3 4 5))


;;(cons 1 '(2 3))
;;(cons '(2 3) (list 1))
