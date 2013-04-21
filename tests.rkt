#lang racket/base

(require "structs.rkt" "factors.rkt"
         racket/match
         (for-syntax racket/base syntax/parse))


(define-factor application (fun arg))
(define-factor abstraction (bound-body))


(define (lam x t)
  (abstraction (bind x t)))
(define (app t s)
  (application t s))

(define (single-step t)
  (match t
    [(application (abstraction bound-body)
                  (? abstraction? arg))
     (define-values (name body) (unbind bound-body))
     (substitute arg name body)]))

(equal?
  (lam (free-var 'n) (free-var 'n))
  (lam (free-var 'x) (free-var 'x)))
(equal? 
  (lam (free-var 'x) (lam (free-var 'y) (app (free-var 'x) (free-var 'y))))
  (lam (free-var 'y) (lam (free-var 'x) (app (free-var 'y) (free-var 'x)))))

(single-step (app (lam (free-var 'x) (free-var 'x)) (lam (free-var 'y) (free-var 'y))))
