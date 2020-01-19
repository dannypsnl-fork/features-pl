#lang racket

(require (for-syntax "utils.rkt"))

(provide Class)
(provide Class-id)
(provide Class-parent)
(provide type-upcasting)
(provide class)

(define class-unique-id 0)
(define (get-unique-id)
  (let ()
    (set! class-unique-id (+ 1 class-unique-id))
    class-unique-id))

(struct Class [id parent])

(define-syntax (class stx)
  (syntax-case stx ()
    [(class name)
      (begin
        (check-ids stx #'(name))
        #'(define name (Class (get-unique-id) 'no-parent)))
      ]
    [(class name parent)
      (begin
        (check-ids stx #'(name))
        #'(define name (Class (get-unique-id) parent)))
      ]))

(define (type-upcasting from to)
  (if (and (Class? (Class-parent from)) (eqv? (Class-id (Class-parent from)) (Class-id to)))
    to
    "can not convert"))
