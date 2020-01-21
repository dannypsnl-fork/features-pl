#lang racket

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define first 1)
(define second 2)

(display "before swap\n")
(pretty-print first)
(pretty-print second)

(swap first second)

(display "after swap\n")
(pretty-print first)
(pretty-print second)
