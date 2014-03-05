#lang racket

(define fibonacci
  (lambda (n)
    (cond
      ((zero? n) 0)
      ((= 1 n) 1)
      (else (+ (fibonacci (- n 1)) (fibonacci (- n 2 )))))))

; Continuation passing style
(define fib
  (lambda (n)
    (letrec ((loop (lambda (n return)
                     (cond
                       ((zero? n) (return 0 0))
                       (( = n 1) (return 0 1))
                       (else (loop (- n 1) (lambda (f0 f1) (return f1 (+ f1 f0)))))))))
    (loop n (lambda (f0 f1) f1)))))