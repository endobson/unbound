#lang racket/base

(require racket/match)
(provide 
  (struct-out abstraction)
  (struct-out application)
  
  factor? factor-map factor-fold)


(struct factor ())

(struct application factor (fun arg) #:transparent)
(struct abstraction factor (bound-body) #:transparent)


(define (factor-map t f)
  (match t
    [(application fun arg)
     (application (f fun) (f arg))]
    [(abstraction bound-body)
     (abstraction (f bound-body))]))

(define (factor-fold t f acc)
  (match t
    [(application fun arg)
     (f arg (f fun acc))]
    [(abstraction bound-body)
     (f bound-body)]))

