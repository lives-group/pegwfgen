#lang racket

(require rackcheck)
(provide (all-defined-out))

(define (Γ-val Γ v)
        (if (null? Γ)
            null
            (if (eq? (car (car Γ)) v)
                (car Γ)
                (Γ-val (cdr Γ) v) )
            )
 ) 

;Creates a contiguos list of numerical values from l to u, icremented by 1 unit.
(define (list-from-to l u)
  (if (> l u)
      null
      (cons l (list-from-to (+ 1 l) u))
      )
  )

; Repeat the generator g by n times collecting the results in a list.
(define (gen:repeat g n)
  (if (eq? n 0)
      (gen:const null)
      (gen:bind (gen:repeat g (- n 1) )
                (lambda (xs) (gen:bind g
                                       (lambda (x) (gen:const (cons x xs)) ) ))))
  )

; Generates a list of naturals 
(define (gen:listNat n k)
  (if (eq? n 0) (gen:bind (gen:integer-in 0 k) (lambda (e) (gen:const (list e))) )
      (gen:bind (gen:listNat (- n 1) k)
                (lambda (xs) (gen:bind ( gen:integer-in 0 k)
                                      (lambda (x) (gen:const (list* x xs)) ) ))))
  )

(define (gen:var n)
  (gen:map (gen:listNat n 23) (lambda (s) (list->string (map (lambda (z) (integer->char (+ z 65))) s ))  ) )
  )

(define (gen:symbolVar n)
  (gen:map (gen:listNat n 23) (lambda (s) (string->symbol (list->string (map (lambda (z) (integer->char (+ z 65))) s )))  ) )
  )


(define (zipWith func xs ys)
  (cond
       [(null? xs) null]
       [(null? ys) null ]
       [#t (cons (func (car xs) (car ys) ) (zipWith func (cdr xs) (cdr ys) ) )]
  )
)