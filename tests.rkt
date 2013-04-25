#lang racket/base

(require "structs.rkt" "factors.rkt"
         racket/match
         (for-syntax racket/base syntax/parse))


(define-factor application (fun arg))
(define-factor abstraction (bound-body))


(define (lam x t)
  (abstraction (bind x t)))
(define (app s t)
  (application s t))

(define-match-expander app:
  (syntax-parser
    [(_ s t)
     #'(application s t)]))

(define-match-expander lam:
  (syntax-parser
    [(_ name body) 
     #'(abstraction (bind: name body))]))

(define (value? arg)
  (abstraction? arg))
    


(define (single-step t)
  (match t
    [(app: (lam: name body) (? value? arg))
     (substitute arg name body)]))

(define x (free-var 'x))
(define y (free-var 'y))
(define z (free-var 'z))

(equal?
  (lam y y)
  (lam x x))
(equal? 
  (lam x (lam y (app x y)))
  (lam y (lam x (app y x))))

(equal? 
  (single-step (app (lam x x) (lam y y)))
  (lam z z))



