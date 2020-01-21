#lang racket

(require (for-syntax syntax/parse))

(define-struct const-type (name))
(define-struct arrow-type (freetype constraint parameter result))

(define (id->string id)
  (symbol->string (syntax->datum id)))

(define (type->string typ)
  (cond
    [(const-type? typ) (format "~a" (const-type-name typ))]
    [(arrow-type? typ) (format "[~a <: ~a] ~a -> ~a" (arrow-type-freetype typ) (arrow-type-constraint typ) (arrow-type-parameter typ) (arrow-type-result typ))]))

;;; let bind
(define-syntax (let stx)
   (define-syntax-class type
      #:description "type"
      #:datum-literals (<: ->)
      (pattern typ:id
               #:with type #'(const-type (id->string #'typ))
               )
      (pattern (lambda [t:id <: b:id] [pt:id -> rt:id])
               #:with type #'(arrow-type (id->string #'t) (id->string #'b) (id->string #'pt) (id->string #'rt))
               )
       )
  (syntax-parse stx #:datum-literals (:)
    [(let name:id : t:type) #'(define name t.type)]))

;;; Usage
(let a : A)
(let f : (lambda [a <: b] (a -> a)))

(printf "type of a: ~a\n" (type->string a))
(printf "type of f: ~a\n" (type->string f))
