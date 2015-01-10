#lang racket
; Exercise 1: Write a function to determine the length of a list.
(define (len lst) (if (empty? lst) 0 (+ 1 (len (list-tail lst 1)))))

; Exercise 2: Write a function to determine if a given element appears in a list.
(define (appears lst item) (> (length (filter (lambda (x) (equal? x item)) lst)) 0))