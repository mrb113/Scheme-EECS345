#lang racket

; sum all the numbers in a list of numbers and non-numbers
(define sumnumbers
  (lambda (l)
    (cond
      ((null? l) 0)
      ((number? (car l)) (+ (car l) (sumnumbers (cdr l))))
      (else (sumnumbers (cdr l))))))

; sum all the numbers in a list of lists
; Two functions we can use to determine if something is a list: 
; 1. list? <- returns true if parameter is a list
; 2. pair? <- returns true if the parameter has a car and a cdr
(define sumnumbers*
  (lambda (l)
    (cond
      ((null? l) 0)
      ((number? (car l)) (+ (car l)(sumnumbers*(cdr l))))
      ((list? (car l)) (+ (sumnumbers* (car l)) (sumnumbers*(cdr l))))
      (else (sumnumbers* (cdr l))))))

; replace all atoms old with the atom new in a list of lists
(define replaceall*
  (lambda (old new l)
    (cond
      ((null? l) '())
      ((list? (car l))(cons (replaceall* old new (car l))(replaceall* old new (cdr l))))
      ((eq? old (car l)) (cons new (replaceall* old new (cdr l))))
      (else (cons (car l) (replaceall* old new (cdr l)))))))

; given a list of lists, remove all numbers in the list
(define noNumbers
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l))(cons (noNumbers (car l))(noNumbers (cdr l))))
      ((number?  (car l)) (noNumbers (cdr l)))
      (else (cons (car l)(noNumbers(cdr l)))))))

; member: return #t if x is in a list of lists
(define member*? 
  (lambda (x l)
  (cond
    ((null? l) #f)
    ((eq? (car l) x) #t)
    ((list? (car l)) (or (member*? x (car l)) (member*? x (cdr l))))
    (else (member*? x (cdr l))))))

; given a list of lists, remove everything from the list
; and leave nothing but the parentheses
(define emptyAll
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l))(cons (emptyAll (car l))(emptyAll (cdr l))))
      (else (emptyAll(cdr l))))))

; remove all parentheses in a list
(define flatten
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (append (flatten(car l))(flatten(cdr l))))
      (else (cons (car l)(flatten (cdr l)))))))