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

(: application : func-type var -> Symbol)
(define (application ft v)
  ;; check type of arg and v-type are the same
  (if (equal? (func-type-arg ft) (var-typ v))
    (if (equal? (list->set (func-type-arg-tag ft)) (list->set (var-tags v)))
       (let ()
         (define eval (func-type-tag-after-eval ft))
         (cond
           [(Some? eval) ((Some-v eval) v)]) ; if some eval to do, apply it
       'ok)
       ; notice the incomplete part
       ; we actually have to remove tags if no arg-tag requirement
       ; but for this little example this is not so important
       'err)
    'err)
  )

(define List (type "List")) ; (type List)
(define int (type "int")) ; (type int)
(define void (type "void")) ; (type Void)
(define sorted (type "sorted")) ; (type sorted)
(define x (var List '())) ; (var x : List)
(: tag-sorted : var -> Void)
(define tag-sorted (lambda (arg-var) (var/add-tag arg-var sorted)))
(define sort (func-type List '() void (Some tag-sorted))) ; (func sort : (x:List) -> Void, after: tag x sorted)
(printf "(sort x):\n")
(application sort x) ; (sort x)
(define binary-search (func-type List (list sorted) int (None))) ; (func binary-search : (x:List with [sorted]) -> int, after: do-nothing)
(printf "(binary-search x):\n")
(application binary-search x) ; (binary-search x), should be fine
(define e (var int '())) ; (var e : int)
(define y (var List '())) ; (var y : List)
(printf "(binary-search y):\n")
(application binary-search y) ; (binary-search y), should report error
