#lang racket

(provide check-ids)

(define (check-ids stx forms)
  (for-each
    (lambda (form)
      (unless (identifier? form)
        (raise-syntax-error #f
                            "not an identifier"
                            stx
                            form)))
  (syntax->list forms)))

