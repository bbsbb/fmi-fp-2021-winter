#lang racket


;; Fromat last time, Vasilena solves is-prime in perfect English
(define (is-prime? n)
  (define (does-not-divider? m)
    (cond
     [(> (* m m) n) #t]
     [(zero? (remainder n m)) #f]
     [else (does-not-divider? (+ m 1))]))
  (does-not-divider? 2))

;; What does it do?
;;(= 0.000000000000000000000000000001 0)


;; What are lambads...
(define my-inc-2 (lambda (n) (+ 1 n)))

;; Silly incremetor
(define (our-inc-by n)
  (lambda (m) (+ m n)))

;;((our-inc-by 3) 5)

;;(define inc-by-six (our-inc-by 6))
;;(define inc-by-four (our-inc-by 4))


;; Exercise 2
;; Write an fn that checks if the digits of a ynumber are ordered.
;;(ordered? 11235567) => #t
;;(ordered? 1132) => #f

(define (ordered? n)
  (cond
   [(< n 10) #t]
   [(>= (remainder n 10)
       (remainder (quotient n 10) 10)) (ordered? (quotient n 10))]
   [else #f]))

;;(ordered? 112344567)


;; Write a the same function of above but checking if the digits confrom
;; to a given predicate.
(define (ordered-by? pred n)
  (cond
   [(< n 10) #t]
   [(pred (remainder n 10)
       (remainder (quotient n 10) 10)) (ordered-by? pred (quotient n 10))]
   [else #f]))

;;(ordered-by? >= 1123)
;; How do we use it with a lambda?
(define ordered-with-lambda?
  (lambda (n) (ordered-by? >= n)))

(ordered-with-lambda? 1123)

;; What is the difference with above? Spoilers, this returns an fn
(define (ordered-with-lambda-2?)
  (lambda (n) (ordered-by? >= n)))

;; Briefly on currying and partial, more in haskell
(define moar-ordered-by? (curry ordered-by? >=))

(moar-ordered-by? 1123)

;; Automorphic numbers, 376*376 = 141376
(define (automorphic? n)
  (define (suffix m k)
    (cond
     [(zero? m) #t]
     [(= (remainder m 10) (remainder k 10)) (suffix (quotient m 10)
                                                    (quotient k 10))]
     [else #f]))
  (suffix n (* n n)))


;; Narcissistic numbvers
;; => 153 = 1^3 + 5^3 + 3^3

(define (narcissistic? n)
  (define (count-digits k m)
    (cond
     [(= 0 k) m]
     [else (count-digits (quotient k 10) (+ m 1))]))
  (define digits (count-digits n 0))
  (define (narc-helper? nums acc)
    (cond
     [(= nums 0) (= acc n)]
     [else (narc-helper? (quotient nums 10)
                         (+ acc (expt (remainder nums 10) digits)))]))
  (narc-helper? n 0))


;;(narcissistic? 9474)
