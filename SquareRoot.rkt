#lang racket

; Slow square root function
(define squareroot
  (lambda (value it)
    (if (zero? it)
        value
        ; new = old - ((old*old) - value)/(2*old)
        ; This is Newton's Method
        (- (squareroot value (- it 1)) (/ (- (* (squareroot value ( - it 1))
        (squareroot value (- it 1))) value) (* 2 (squareroot value (- it 1 ))))))))

; Faster square root function, but not functional style
(define squareroot2
  (lambda (value it)
    (if (zero? it)
        value
        (let ((old (squareroot2 value (- it 1))))
          (- old (/ (- (* old old ) value) (* 2 old )))))))

; Faster functional square root function
(define squareroot3
  (lambda (value it)
    (if (zero? it)
        value
        ((lambda (old) (- old (/ (- (* old old ) value) (* 2 old)))) (squareroot3 value (- it 1))))))

                                               