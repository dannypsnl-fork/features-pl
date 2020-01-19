#lang racket

(require "single-inherit.rkt")

(class A)
(printf "id of A: ~v\n" (Class-id A))

(class B A)
(printf "id of B: ~v\n" (Class-id B))
(printf "parent of B: ~v\n" (Class-id (Class-parent B)))

(class C)
(printf "convert B to A: ~v\n" (Class-id (type-upcasting B A)))
(printf "convert C to A: ~v\n" (type-upcasting C A))
