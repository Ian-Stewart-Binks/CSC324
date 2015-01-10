#lang racket
; Exercise 1: Write a function to determine the length of a list.
(define (len lst) (if (empty? lst) 0 (+ 1 (len (list-tail lst 1)))))

; Exercise 2: Write a function to determine if a given element appears in a list.
(define (appears lst item) (> (length (filter (lambda (x) (equal? x item)) lst)) 0))

; Exercise 3: Write a function to determine the number of duplicates in a list.
(define (num_duplicates lst) (- (length lst) (length (remove-duplicates lst))))

; Exercise 4: Write a function to remove all duplicates from a list.
(define (remove_duplicates lst) (foldl (lambda (x y) (if (member x (remove x y)) (remove x y) y)) lst lst))

; Exercise 5 a: Given two lists, output the items that appear in both lists (intersection).
(define (intersection lst1 lst2) (let ([lst3 (filter (lambda (x) (member x lst2)) lst1)])
                                   (append lst3 (filter (lambda (x) (and (member x lst1) (not (member x lst3)))) lst2))))

; Exercise 5 b: Then, output the items that appear in at least one of the two lists.
(define (union lst1 lst2) (remove-duplicates (append lst1 lst2)))

; Exercise 6: Write a function which takes a list of lists, and returns the list which contains the largest item.
(define (get-largest-sublist lsts) (filter (lambda (x) (if (empty? x) #f
                                                           (= (first (max-list x)) (first (max-list (get-all-max-vals lsts))))))
                                           lsts))

(define (get-all-max-vals lsts) (map (lambda (y) (first (max-list y))) (filter (lambda (x) (not (empty? x))) lsts)))

(define (max-list lst) (filter (lambda (x) (empty? (filter (lambda (y) (> y x)) lst))) lst))