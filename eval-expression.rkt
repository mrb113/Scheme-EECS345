#lang racket

; Evaluates an expression of atoms in java-style in Scheme
; This does not yet work.. get ready for next time! 
(define eval-expression
  (lambda (expression format)
      (let (( operator (lambda (s) (car s)))
            (left-operand (lambda (s) (car (cdr s))))
            (right-operand (lambda (s) (car (cdr (cdr s))))))
        (cond 
          ((not (list? expression)) expression)
          ((eq? '+ (operator expression)) (+ (eval-expression (left-operand expression) environment) (eval-expression (right-operand expression) environment)))
          ((eq? '- (operator expression)) (- (eval-expression (left-operand expression) environment) (eval-expression (right-operand expression) environment))) 
          ((eq? '* (operator expression)) (* (eval-expression (left-operand expression) environment) (eval-expression (right-operand expression) environment)))
          ((eq? '/ (operator expression)) (/ (eval-expression (left-operand expression) environment) (eval-expression (right-operand expression) environment)))
          ((eq? '% (operator expression)) (modulo (eval-expression (left-operand expression) environment) (eval-expression (right-operand expression) environment)))
          (else (error 'undefined-operator))))))

(define operator
  (lambda (s)
    (car (cdr s))))

(define left-operand
  (lambda (s)
    (car s)))

(define right-operand
  (lambda (s)
    (car (cdr (cdr s)))))
