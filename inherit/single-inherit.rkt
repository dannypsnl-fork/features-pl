#lang racket

(define class-unique-id 0)

(struct Class [id parent])

(begin-for-syntax
  (define (check-ids stx forms)
    (for-each
     (lambda (form)
       (unless (identifier? form)
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             form)))
     (syntax->list forms))))

(define-syntax (class stx)
  (syntax-case stx ()
    [(class name)
      (begin
        (check-ids stx #'(name))
        #'(let ()
          (set! name (Class class-unique-id 'no-parent))
          (set! class-unique-id (+ 1 class-unique-id))))
      ]
    [(class name parent)
      (begin
        (check-ids stx #'(name))
        #'(let ()
          (set! name (Class class-unique-id parent))
          (set! class-unique-id (+ 1 class-unique-id))))
      ]))

(define A 'none)
(class A)
(printf "id of A: ~v\n" (Class-id A))

(define B 'none)
(class B A)
(printf "id of B: ~v\n" (Class-id B))
(printf "parent of B: ~v\n" (Class-id (Class-parent B)))

(define (type-upcasting from to)
  (if (and (Class? (Class-parent from)) (eqv? (Class-id (Class-parent from)) (Class-id to)))
    to
    "can not convert"))

(define C 'none)
(class C)
(printf "convert B to A: ~v\n" (Class-id (type-upcasting B A)))
(printf "convert C to A: ~v\n" (type-upcasting C A))
