#lang racket/base

(require "structs.rkt" "factors.rkt")

(define (lam x t)
  (abstraction (bind x t)))
(define (app t s)
  (application t s))


(equal? 
  (lam (free-var 'n) (free-var 'n))
  (lam (free-var 'x) (free-var 'x)))
(equal? 
  (lam (free-var 'x) (lam (free-var 'y) (app (free-var 'x) (free-var 'y))))
  (lam (free-var 'y) (lam (free-var 'x) (app (free-var 'y) (free-var 'x)))))
