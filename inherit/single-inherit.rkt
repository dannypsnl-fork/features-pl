#lang racket

(define class-unique-id 0)

(struct Class [id parent])

(define-syntax class
  (syntax-rules ()
    [(class name)
      (let ()
        (set! name (Class class-unique-id 'no-parent))
        (set! class-unique-id (+ 1 class-unique-id)))
      ]
    [(class name parent)
      (let ()
        (set! name (Class class-unique-id parent))
        (set! class-unique-id (+ 1 class-unique-id)))
      ]))

(define A 'none)
(class A)
(printf "id of A: ~v\n" (Class-id A))

(define B 'none)
(class B A)
(printf "id of B: ~v\n" (Class-id B))
(printf "parent of B: ~v\n" (Class-id (Class-parent B)))
