#lang racket


;; Last time we wrote - apply to list.
;; Accept tow arguments - first one is an fn, 2nd one is a collection, return a new
;; collection with fn applied to all individual items.


;;(apply-to-list (lambda (x) (+ 1 x) '(1 2 3))) => '(2 3 4)

(define (apply-to-list f c)
  (cond [(null? c) c]
        [else (cons (f (car c)) (apply-to-list f (cdr c)))]))

;;(apply-to-list (lambda (x) (+ 1 x) '(1 2 3))) => '(2 3 4)
;; map ->

(let [[am-i-list "maybe"]]
  ;;(length am-i-list)
  (car am-i-list))


;; List <-> String conversion
;;(list->string (string->list "am-i-list"))

;; Create a function that returns the longer of two lists(where longer = contains more elements)
;; e.g. (longer-list '(1 2) '(2 3 4)) => '(2 3 4)

(define (longer-list l1 l2)
  (cond [(>= (length l1) (length l2)) l1]
        [else l2]))

;;(longer-list '(1 2) '(1 2 3))


;; Create a function that returns true if a character is a break character - e.g. empty space or punctuation.
;; "I am a <-break character cat."
(define (break-char? c)
  (or (char-whitespace? c) (char-punctuation? c)))


;;(break-char? #\space)
;;(break-char? #\c)

;; Actual Exercise 1:
;; We are going to receive a phrase and return the longest word in it.
;; e.g. (longest-word "I am definitely not a large cat.")  => "definitely"
;; If two or more with the same length - idgaf.

;; Steps:
;; You have a phrase.
;; - Decompose into words  -- with the functions above, do you need this?
;; - For each word, compare to the previous one, pick longer, retain.


;; Character decision.
(define (longest-word phrase)
  ;; What arguments?
  (define (longest-word-helper rest-of-phrase current-word longest-word)
    (cond [(null? rest-of-phrase) (longer-list current-word longest-word)]
          [break-char? (car rest-of-phrase) (longest-word-helper (cdr rest-of-phrase)
                                                                 null
                                                                 (longer-list current-word longest-word))]
          [else (longest-word-helper (cdr rest-of-phrase)
                                     (cons (car rest-of-phrase) current-word)
                                     longest-word)]))
  (longest-word-helper (string->list phrase) null null))



;; Library decision.
(define (longest-word-two phrase)
  (define (longest-word-helper rest-of-phrase longest)
    (cond [(null? rest-of-phrase) longest]
          [(< (string-length longest) (string-length (car rest-of-phrase))) (longest-word-helper (cdr rest-of-phrase)
                                                                                                 (car rest-of-phrase))]
          [else (longest-word-helper (cdr rest-of-phrase) longest)]))
  (longest-word-helper (cdr (string-split phrase)) (car (string-split phrase))))


;;(longest-word-two "I am also working hard")






;; Higher order functions.
;;(apply-to-list)


(define (my-increment x) (+ 1 x))

;;(my-increment 5)

;; My receivess 2(?) arguments
;; a function and a collection
;; Output: Collection of the same # of elements as the input one
;;         Results are in order.
;;         Elements may be transformed.

(map my-increment '(1 2 3))

(define (my-sum n m)
   (+ n m))


(map my-sum '(1 2 3) '(3 2 ))


(map + '(1 2 3) '(3 2 1))

(map + '(1 2 3))


;; What about filter?
;; It receives:
;; A predicate + a collection
;; Output:
;; A collection with at most as many arguments as the input one.
;; Can't transform an element. Useful ONLY for subsets.
;; What does it do?

(filter odd? '(1 2 3 4))

(filter even? '(1 2 3 4))

(filter identity '(#t #t #f))

;; What about apply?
;; Collection: '(4 4 4) or ERROR
;;
;;(apply + '(1 2 3) '(3 2 1))
;;(apply + 10 '(1 2 3 4 5))


;; Exercise Something:
;; '(1 2 3 4 5 6) - Sum of the elements / Count of elements
(define (my-average xs)
  (/ (apply + xs) (length xs)))

;; Idiomatic collection names.
;; xs, coll, l


;; Exercise: Something something
;; Createa a function that accepts an element and returns a collection of N of the same element
;; (all-of-xs 6 "wut") =>  '("wut" "wut" "wut" "wut" "wut" "wut")

(define (all-of-xs n x)
  (map (lambda (_) x) (range 0 n)))

;; inconsolata <-- you can setup in any editor.
;; Idiomatic
;; ( _ _ c ) <-- For stupid languages this is a syntax error.
;;(all-of-xs 6 "wut")


;; Transform a number to a collection of length equal to number's value.
;;(range 0 5)


;; Exercise - Sum all odd numbers in a collection

(define (sum-odd xs)
  (apply + (filter odd? xs)))


;; Exercise: Find the highest VALUE word in a list of strings.
;; Where value = sum of ascii values of the characters.
;; No recursion, doesn't exist.

;;(highest-value-word ("I" "am" "DOGGOOOO")) => Return max summ

;;(char->integer #\a)
;;(char->integer #\A)
;;(max 3 5)
;;(max 3 5 7)


(define (highest-value-word xs)
  (apply max (map (lambda (word)
                    (apply + (map char->integer (string->list word))))
                  xs)))



;;(highest-value-word '("cat" "dog")) => "cat"
