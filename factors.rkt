#lang racket/base

(require racket/match racket/list
         (for-syntax racket/base
                     syntax/parse))
(provide 
  (rename-out
    (factor? factor?))
  define-factor
  factor-map factor-fold)

(define (factor*? f)
  (or (primitive-factor? f)
      (factor? f)))

(define (primitive-factor? f)
  (match f
    [(list (? factor*?) ...) #t]
    [_ #f]))


(struct factor () #:transparent)

(define-values (prop:constructor
                constructor?
                constructor)
  (make-struct-type-property 'constructor))


(define-syntax define-factor
  (syntax-parser
    [(_ name:id (field:id ...))

     #'(begin
         (struct name factor (field ...) #:transparent
                 #:property prop:constructor (λ (field ...) (name field ...)))
         )]))


(define (factor-map t f)
  (match t
    [(? list?)
     (map f t)]
    [(? factor?)
     (apply (constructor t)
            (map f (rest (vector->list (struct->vector t)))))]))

(define (factor-fold t f acc)
  (match t
    [(or (? list? l)
         (? factor? (app (λ (t) (rest (vector->list (struct->vector t)))) l)))
     (for/fold ((acc acc)) ((t l))
       (f t acc))]))

