#lang racket
; returns the length of the list
(define len
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (len (cdr l))))))

; factorial function
(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; counts the occurrences of an atom in a list of atoms
(define countNum
  (lambda(a l)
    (cond
      ((null? l) 0)
      ((eq? a (car l)) (+ 1 (countNum a (cdr l))))
      (else (countNum a (cdr l))))))

; sums the numbers in a list of atoms
(define sumnumbers
  (lambda(l)
    (cond
      ((null? l) 0)
      ((number? (car l)) (+ (car l) (sumnumbers (cdr l))))
      (else (sumnumbers (cdr l))))))
