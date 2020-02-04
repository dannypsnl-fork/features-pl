#lang typed/racket

(struct TraitType (
  ; fields
  [name : String]
  )
  ; struct option
  ; #:transparent option is important here because we want to compare TraitType directly 
  #:transparent)
(struct MarkableType (
  ; fields
  [name : String]
  [mark : (Listof TraitType)]
  )
  ; struct option
  #:mutable)

(: new-type (-> String MarkableType))
(define (new-type name)
  (MarkableType name '()))
(: type/add-tag (-> MarkableType TraitType Void))
(define (type/add-tag self tag)
  (set-MarkableType-mark! self
    ; simply remove duplicates, performance is not the point at here
    (remove-duplicates (list* tag (MarkableType-mark self)))
  ))
(: type/remove-tag (-> MarkableType TraitType Void))
(define (type/remove-tag self tag)
  (set-MarkableType-mark! self (remove tag (MarkableType-mark self))))

;;; prepare environment
(define env (make-hash))

(define List (new-type "List")) ; (type List)
(hash-set! env "x" List) ; (var x : List)
; (func sort : List -> (Tag List sorted))
; (sort x)
; (var e : int)
; (push-back x e)
; (binary-search x) ; should report error
; (sort x)
; (binary-search x) ; should be fine
