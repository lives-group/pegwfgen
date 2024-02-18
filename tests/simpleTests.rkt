#lang racket

(require cover)
(require "../peg-gen.rkt"
         "../gen-utils.rkt"
         "../peg-gen-types.rkt"
         "verf-well-formed.rkt"
         rackcheck
         rackunit)

(provide (all-defined-out))

(define (elem? x xs)
    (and (not (null? xs)) (or (eq? x (car xs)) (elem? x (cdr xs)) ) )
 )

(define (sum xs)
  (match xs
     [null 0]
     [(cons x xs) (+ x (sum xs))]
  ))

(define (circled? x ys Γ)
  (if (null? (headSet (cdr (Γ-val Γ x) )))
      #f
      (or (elem?  x ys)
          (ormap (lambda (z) (circled? z (cons x ys) Γ)) (headSet (cdr (Γ-val Γ x) ))) 
      ) )
  )

(define (sampleList list)
  (if (null? list)
      null
      (if (>= (random 0 99) 50) (cons (car list) (sampleList (cdr list))) (sampleList (cdr list))  ))
  )

  
(define-property no-left-recursion ([peg  (gen:peg 3 5 2)])
    (check-equal? (ormap (lambda (x) (circled? (car x) '() (last peg))) (last peg)) #f)
  )

(define-property obey-constraint ( [Γ (gen:Γ 3)]
                                   [Δ  (gen:const (sampleList (map car Γ ) ) ) ]
                                   [peg  (gen:expr Γ Δ '(0 1 2) #f 2 )])
    (check-equal? (foldr (lambda (e rb) (and (not (elem? e Δ)) rb) ) #t (headSet (cdr peg))) #t)
  )
                             
(define-property genVars ([n (gen:integer-in 0 10)]
                          [ln  (gen:var n)])
    (check-equal?  (string-length ln) (+ n 1))
  )

(define-property zipSz ([xs (gen:list gen:natural)]
                        [ys (gen:list gen:natural)])
    (check-equal?  (length (zipWith + xs ys)) (min (length xs) (length ys)))
  )

(define-property zipCons ([xs (gen:list gen:natural)]
                          [ys (gen:list gen:natural)])
    (check-equal?  (let ([m (min  (length xs)  (length ys))])
                        (+ (sum (take xs m)) (sum (take ys m)))) 
                   (sum (zipWith + xs ys))
    )
)


(define (height peg)
   (match peg
     [(list • e1 e2) (+ 1 (max (height e1) (height e2))) ]
     [(list / e1 e2) (+ 1 (max (height e1) (height e2))) ]
     [(list ! e1) (+ 1 (height e1) ) ]
     [(list * e1) (+ 1 (height e1) ) ]
     [_ 0]
     )
  )

(define (mkpeg k n)
  (map (lambda (x) (height (cadr x)))  (sample (gen:peg 5 5 k) n) ) ) 

(define-property pegDepth   ([n (gen:integer-in 0 6)]
                             [peg (gen:peg 10 10 n)])
    (check-equal?  (<= (height (cadr peg)) n) #t)
    )

(define-property pegDepthDist ([peg (gen:peg 10 10 4)])
    (label!
       (case (height (cadr peg))
         [(0) "zero"]
         [(1) "um"]
         [(2) "dois"]
         [(3) "tres"]
         [(4) "quatro"]
         [else "outro"]
    ))
    (check-equal?  (<= (height (cadr peg)) 4) #t)

    )

;(check-property genVars)
;(check-property zipSz)
;(check-property zipCons)
;(check-property no-left-recursion)
;(check-property obey-constraint)
;(check-property (make-config #:tests 100) pegDepth)
;(check-property pegDepthDist)


;(check-property  (make-config #:tests 1000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) pegDepth)
