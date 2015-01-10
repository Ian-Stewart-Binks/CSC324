#lang racket
; Exercise 1
(define (len lst) (if (empty? lst) 0 (+ 1 (len (list-tail lst 1)))))