#lang racket

(define multiply
  (lambda (l)
    (cond
      ((null? l) 1)
      (else (* (car l) (multiply cdr l))))))

(define multiply-cps
  (lambda (l return)
    (cond
      ((null? l) (return 1))
      ((zero? (car l)) 0)
      (else (multiply-cps (cdr l) (lambda (v) (return (* v (car l)))))))))

(define mult
  (lambda (l)
    (letrec ((loop (lambda (l2 return break) ;this l is different than the first l
                     (cond
                       ((null? l2) (return 1))
                       ((zero? (car l2)) (break 0))
                       (else (loop (cdr l2) (lambda (v) (return (* (car l2) v)))
             break))))))
      (loop l (lambda (v) v ) (lambda (v) v )))))

