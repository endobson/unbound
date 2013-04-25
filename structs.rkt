#lang racket/base

(require racket/set racket/match "factors.rkt"
         (for-syntax racket/base syntax/parse))

(provide
  free-var
  bound-var
  var?
  bind
  bind:
  (rename-out
    (open unbind))
  embed
  substitute
  
  )


(struct free-var (name) #:extra-constructor-name make-free-var #:transparent)
(struct bound-var (level index) #:extra-constructor-name make-bound-var #:transparent)

(define (var? v)
  (or (free-var? v) (bound-var? v)))

(struct binding (bound-pattern term) #:transparent)
(struct embed (term))
(struct rebinding (binder body))
(struct rec-binding (pattern))


(define-match-expander bind:
  (syntax-parser
    [(_ pattern term)
     #'(app (lambda (b) (call-with-values (lambda () (open b)) list))
            (list pattern term))]))


(define (substitute arg name term)
  (define (recur f)
    (match f
      [(== name) arg]
      [(? free-var?) f]
      [(? factor?)
       (factor-map f recur)]
      [(binding pat term)
       (binding (recur pat) (recur term))]
      [(embed t)
       (embed (recur t))]
      [(rec-binding pat)
       (rec-binding (recur pat))]
      [(rebinding p1 p2)
       (rebinding (recur p1) (recur p2))]
      [(? bound-var?) f]))
  (recur term))


(define (pattern? t)
  (match t
    [(free-var _) #t]
    [(rebinding p1 p2)
     (and (pattern? p1)
          (pattern? p2))]
    [(rec-binding p)
     (pattern? p)]
    [(embed _) #t]
    [else #f]))

(define (bound-pattern? t)
  (match t
    [(bound-var _ _) #t]
    [(rebinding p1 p2)
     (and (bound-pattern? p1)
          (bound-pattern? p2))]
    [(rec-binding p)
     (bound-pattern? p)]
    [(embed _) #t]
    [else #f]))


;; TODO contract pattern with unique name restriction
(define (bind pattern term)
  (define name-map (pattern-name-map pattern))
  (binding (close-pattern pattern name-map)
           (close-term term name-map)))

(define (rebind p1 p2)
  (define name-map (pattern-name-map p1))
  (rebinding p1 (close-pattern p2 name-map)))

(define (rec-bind p)
  (define name-map (pattern-name-map p))
  (rec-binding (close-pattern p name-map)))

(define (unrebind b)
  (match-define (rebinding p1 p2) b)
  (define name-map (pattern-fresh-name-map p1))
  (values p1 (open-pattern p1 name-map)))

(define (unrec-bind b)
  (match-define (rec-binding p) b)
  (define name-map (pattern-fresh-name-map p))
  (values p (open-pattern p name-map)))


(define (pattern-name-map pat)
  (define (recur pat acc)
    (match pat
      [(free-var n) (cons n acc)]
      [(embed _) acc]
      [(rebinding p1 p2) (recur p2 (recur p1 acc))]
      [(rec-binding p)
       (recur p acc)]
      [(? factor?)
       (factor-fold pat recur acc)]))

  (for/hash ((name (recur pat null))
             (index (in-naturals)))
    (values name index)))

(define (pattern-fresh-name-map p)
  (define name-map (pattern-name-map p))
  (for/hash (((k v) name-map))
    (values v (gensym k))))

(define (bound-pattern-fresh-name-map bp)
  (define (bound-pattern-size bp)
    (define (recur bp acc)
      (match bp
        [(bound-var _ _) (add1 acc)]
        [(embed _) acc]
        [(rebinding p1 p2) (recur p2 (recur p1 acc))]
        [(rec-binding p) (recur p acc)]
        [(? factor?)
         (factor-fold bp recur acc)]))
    (recur bp 0))

  (for/hash ((index (bound-pattern-size bp)))
    (values index (free-var (gensym)))))



(define (open b)
  ;(define used-names (free-names binding))
  (match-define (binding bp t) b)
  (define name-map (bound-pattern-fresh-name-map bp))
  (values
    (open-pattern bp name-map)
    (open-term t name-map)))


(define-values (open-term open-pattern)
  (let ()
    (define (replacers name-map)
      (define ((replace k) t)
        (match t
          [(free-var _) t]
          [(bound-var (== k) i) (hash-ref name-map i)]
          [(bound-var _ _) t]
          [(binding bp t)
           (binding 
             ((replace-bound-pattern k) bp)
             ((replace (add1 k)) t))]
          [(? factor?)
           (factor-map t (replace k))]))
      (define ((replace-bound-pattern k) t)
        (match t
          [(bound-var _ _) t]
          [(embed t) (embed ((replace k) t))]
          [(rebinding p1 p2)
           (rebinding
             ((replace-bound-pattern k) p1)
             ((replace-bound-pattern (add1 k)) p2))]
          [(rec-binding p)
           (rec-binding ((replace-bound-pattern (add1 k)) p))]
          [(? factor?)
           (factor-map t (replace-bound-pattern k))]))
      (define ((replace-pattern k) t)
        (match t
          [(bound-var (== k) i) (hash-ref name-map i)]
          [(embed t) (embed ((replace k) t))]
          [(rebinding p1 p2)
           (rebinding
             ((replace-pattern k) p1)
             ((replace-pattern (add1 k)) p2))]
          [(rec-binding p)
           (rec-binding ((replace-pattern (add1 k)) p))]
          [(? factor?)
           (factor-map t (replace-pattern k))]))
      (values
        (replace 0)
        (replace-pattern 0)))

    (values
      (lambda (t name-map)
        (let-values (((replace replace-pattern) (replacers name-map)))
          (replace t)))
      (lambda (bp name-map)
        (let-values (((replace replace-pattern) (replacers name-map)))
          (replace-pattern bp))))))




(define-values (close-term close-pattern)
  (let ()
    (define (replacers name-map)
      (define ((replace-pattern k) bp)
        (match bp
          [(free-var n) (bound-var k (hash-ref name-map n))]
          [(embed t) (embed (replace k t))]
          [(rebinding p1 p2)
           (rebinding
             ((replace-pattern k) p1)
             ((replace-pattern (add1 k)) p2))]
          [(rec-binding p)
           (rec-binding ((replace-pattern (add1 k)) p))]
          [(? factor?)
           (factor-map bp (replace-pattern k))]))
      (define ((replace-bound-pattern k) bp)
        (match bp
          [(bound-var _ _) bp]
          [(embed t) (embed ((replace k) t))]
          [(rebinding p1 p2)
           (rebinding
             ((replace-pattern k) p1)
             ((replace-pattern (add1 k)) p2))]
          [(rec-binding p)
           (rec-binding ((replace-bound-pattern (add1 k)) p))]
          [(? factor?)
           (factor-map bp (replace-bound-pattern k))]))
      (define ((replace k) t)
        (match t
          [(free-var n)
           (cond
            [(hash-ref name-map n #f) => (λ (v) (bound-var k v))]
            [else t])]
          [(bound-var _ _) t]
          [(binding bp t)
           (binding 
             ((replace-bound-pattern k) bp)
             ((replace (add1 k)) t))]
          [(? factor?)
           (factor-map t (replace k))]))
      (values replace replace-pattern))
    (values
      (lambda (t name-map)
        (let-values (((replace replace-pattern) (replacers name-map)))
          ((replace 0) t)))
      (lambda (p name-map)
        (let-values (((replace replace-pattern) (replacers name-map)))
          ((replace-pattern 0) p))))))
