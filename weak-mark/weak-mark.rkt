#lang typed/racket

(struct None ())
(struct (a) Some ([v : a]))
(define-type (Option a) (U None (Some a)))

(struct type (
  [name : String]
  )
  #:transparent
  )
(struct var (
  [typ : type]
  [tags : (Listof type)]
  )
  ; options
  #:mutable)
(struct func-type (
  [arg : type]
  [arg-tag : (Listof type)]
  [ret : type]
  [tag-after-eval : (Option (-> var Void))]
  ))

(: var/add-tag : var type -> Void)
(define (var/add-tag self tag)
  (set-var-tags! self
    ; performance is not the point here
    (remove-duplicates (list* tag (var-tags self)))
  ))
(: var/remove-tag : var type -> Void)
(define (var/remove-tag self tag)
  (set-var-tags! self (remove tag (var-tags self))))
(: tag-ty : type -> var -> Void)
(define tag-ty (lambda (ty) (lambda (arg-var) (var/add-tag arg-var ty))))

(: application : func-type var -> Symbol)
(define (application ft v)
  ;; check type of arg and v-type are the same
  (if (equal? (func-type-arg ft) (var-typ v))
    (let (
      [require-tags (func-type-arg-tag ft)]
      [tags (var-tags v)])
      (for ([tag tags])
        (cond
          ; remove tag that existing in variable but not in requirements
          [(not (member tag require-tags)) (var/remove-tag v tag)]))
      (set! tags (var-tags v))
      ; after align tag, comparing tags and requirements, they should be the same
      (if (equal? (list->set require-tags) (list->set tags))
        (let ()
          (define eval (func-type-tag-after-eval ft))
          (cond
            [(Some? eval) ((Some-v eval) v)]) ; if some eval to do, apply it
        'ok)
        (error (format "tags mismatching, expected: ~s but got: ~s" require-tags tags))))
    (error (format "type mismatching, expected: ~s but got: ~s" (func-type-arg ft) (var-typ v))))
  )

(define List (type "List")) ; (type List)
(define int (type "int")) ; (type int)
(define void (type "void")) ; (type Void)
(define sorted (type "sorted")) ; (type sorted)
(define x (var List '())) ; (var x : List)
(define sort (func-type List '() void (Some (tag-ty sorted)))) ; (func sort : (x:List) -> void, after: tag x sorted)
(printf "(sort x):\n")
(application sort x) ; (sort x)
(define binary-search (func-type List (list sorted) int (None))) ; (func binary-search : (x:List with [sorted]) -> int, after: do-nothing)
(printf "(binary-search x):\n")
(application binary-search x) ; (binary-search x), should be fine
(define e (var int '())) ; (var e : int)
(define y (var List '())) ; (var y : List)
(define modify-list (func-type List '() void (None))) ; (func modify-list : x:List -> void, after: do-nothing)
(printf "(sort y):\n")
(application sort y) ; (sort y)
(printf "(binary-search y):\n")
(application binary-search y) ; (binary-search y), should be fine
(printf "(modify-list y):\n")
;;; FIXME: shouldn't get 'err-tag-mismatching at here
(application modify-list y) ; (modify-list y)
(printf "(binary-search y):\n")
(application binary-search y) ; (binary-search y), should report error