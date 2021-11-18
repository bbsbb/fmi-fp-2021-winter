#lang racket

;; Create a function of two arguments - a predicate and a list.
;; Output is a nested list of two elements - all values for which
;; the predicate was true/false respectively.


;;(split-list string? '(1 "a" 3 "c")) -> '('(1 3) '("a" "c"))

;; Input - List of 4 elements. Output. List of Not 4 elements.
;; Spoilers: You can never solve it with map.

;; map, filter, apply

;;
(define (split-list f? xs)
  (list (filter f? xs)
        (filter (compose not f?) xs)))

;; Read it at some point in your life.
;; 1. https://wiki.haskell.org/Fold
;; 2. https://wiki.haskell.org/Foldr_Foldl_Foldl'


;; Fold SGUVA.
;; foldl, reduce.
;; Higher order fold:
;; Arguments: A function that receives an FN, an INITIAL value to use as 2nd argument and a collection.

(foldl cons '() '(1 2 3))

;;
;;
(foldl (lambda (x initial)
         initial)
       '() '(#t #t #t))

;; First-to-n-ocdcurences. Create a function taht receives a list and a counter and returns the
;; the first element of the list to be encountered COUNT times.
;; Maybe we change the
;;(first-to-n '(1 3 1 3 1 1 1 4) 2) => 1

;; Go over the elements in order of appearence.
;; (1 2 3 2 1) -> SHoul dbe 2, you return 1.

;;
;;'(2 2 1 2 2) - What we don't have?
;; We can do instead
;;'(1 2 3 2 1)
;;'(4 3 0 3 4) - Transform this into a list of the index of when an element reached the count 2.

;;(list-ref '(1 2) 1)


(+ 1 2 3)
(apply + '(1 2 3))

(define (reaches-count-at-index xs n desired-count current-index current-count)
  ;; xs - The list we are traversing
  ;; n - The element we are counting
  ;; desired-count - The count we are going to reach.
  ;; current-index - Where we currently are in traversing xs
  ;; current-count - How many times have we seen the element so far.
  (cond
   ;; Could be sus if passing 0 as desired count?
   [(= current-count desired-count) (- current-index 1)]
   [(null? xs) current-index]
   [else (reaches-count-at-index (cdr xs)
                                 n
                                 desired-count
                                 (+ 1 current-index)
                                 (cond
                                  [(= n (car xs)) (+ current-count 1)]
                                  [else current-count]))]))


;;'(1 2 3 2 1)
;;'(4 3 0 3 4) - Transform this into a list of the index of when an element reached the count 2.
(define (first-to-n-occurences xs n)
  (let* ([occurence-indices (map (lambda (x) (reaches-count-at-index xs x n 0 0))
                                 xs)]
         [first-index (apply min occurence-indices)])
    (list-ref xs first-index)))



;; I symiratite


;;(first-to-n-occurences '(1 2 3 5 1) 2)

;;(reaches-count-at-index '(1 2 3 2 1) 2 2 0 0)

;;(reaches-count-at-index )

(define (first-to-n xs n)
  (define (helper ys cnt)
    (cond [(null? ys) "Error: No element reaches the required count"]
          [(= cnt n) (car ys)]
          ;; This is sus.
          [(= (car ys) (cadr ys)) (helper (cdr ys) (+ 1 cnt))]
          [else (helper (cdr ys) 1)]))
  (helper xs 1))



;; Exercise: We receive a special digit as argument. We return a function that receives a
;;           matrix and sets all ROWS containing the SPEICLA digit to only have values of the digit.

;;((special-matrix 3) (list '(1 2 3)
;;                           (0 0 1)
;;                           (4 0 1))
;; Result:

(define (special-matrix special-number)
  (lambda (matrix)
    (map (lambda (row)
           (cond
            [(list? (member special-number row)) (map (lambda (_) special-number) row)]
            [else row])) matrix)))
