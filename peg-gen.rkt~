#lang racket

(require racket/set
         rackcheck
         rackunit
         "gen-utils.rkt")



(provide gen:expr
         gen:grm
         gen:Γ
         gen:peg
         gen:var
         gen:symbolVar
         initΔ
         Γ-val
         )

(define myGen (make-pseudo-random-generator))

(define (h n) (- n 1) )

(define (gen:expr Γ Δ Σ b p) 
       (cond
             [(equal? p 0)  (gen:one-of (append (mkListVar Γ Δ b) 
                                                (if b
                                                    (list (list 'ε #t '()) )
                                                    (map (lambda (x) (list x #f '())) Σ) )
                                         )) ]
             [(and (> p 0) b)    (gen:choice (gen:bind (gen:expr Γ Δ Σ b (h p))
                                                       (lambda (t)  (gen:bind (gen:expr Γ Δ Σ b (h p)) (lambda (s) (gen:const  (mkSeq t s) ) ) ) ) )
                                             (gen:bind (gen:tuple (gen:expr Γ Δ Σ #t (h p)) gen:boolean)
                                                       (lambda (t)  (gen:bind (gen:expr Γ Δ Σ (cadr t) (h p))
                                                                              (lambda (s) (gen:const  (mkAlt (car t) s) )) ) ) )
                                             (gen:bind (gen:expr Γ Δ Σ #f (h p))
                                                       (lambda (t)  (gen:bind (gen:expr Γ Δ Σ #t (h p)) (lambda (s) (gen:const  (mkAlt t s) )) ) ) )
                                             (gen:bind gen:boolean
                                                       (lambda (rb) (gen:bind (gen:expr Γ Δ Σ rb (h p))
                                                                              (lambda (t) (gen:const (mkNot t) ) ) ) ) )
                                             (gen:bind (gen:expr Γ Δ Σ #f (h p))

                                                       (lambda (t) (gen:const (mkKle t) ) ))
                                  )]
             [(and (> p 0) (not b))  (gen:choice (gen:bind (gen:expr Γ Δ Σ #t (h p))
                                                       (lambda (t)  (gen:bind (gen:expr Γ Δ Σ #f (h p)) (lambda (s) (gen:const  (mkSeq t s) ) ) ) ) )
                                                 (gen:bind (gen:expr Γ Δ Σ #f (h p))
                                                           (lambda (t)  (gen:bind (gen:expr Γ Δ Σ (car (sample gen:boolean 1 myGen)) (h p))
                                                                                  (lambda (s) (gen:const  (mkSeq t s) ) ) ) ) )
                                                 (gen:bind (gen:expr Γ Δ Σ #f (h p))
                                                           (lambda (t)  (gen:bind (gen:expr Γ Δ Σ #f (h p)) (lambda (s) (gen:const  (mkAlt t s) )) ) ) )
                                  )]
           )  
)




(define (mkListVar Γ Δ b)
     ;(display "mkListVar: GAMMA = ")
     ;(display Γ)
     ;(display "\nmkListVar: DELTA = ")
     ;(display Δ)
     ;(display "\n")
     (map (lambda (y) (list (car y) (cadr y) (set-union (caddr y) (list (car y)) ) ))
          (filter (lambda (x) (and (eq? (cadr x) b) (not (member (car x) Δ))) ) Γ)
     )
 )

(define (mkSeq e1 e2)
        (list `(• ,(car e1) ,(car e2)) (and (cadr e1) (cadr e2)) ( if (cadr e1)
                                                                      (set-union (caddr e1) (caddr e2))
                                                                      (caddr e1)
                                                                      ) )
)

(define (mkAlt e1 e2)
        (list `(/ ,(car e1) ,(car e2)) (or (cadr e1) (cadr e2)) (set-union (caddr e1) (caddr e2) ) )
)

(define (mkKle e1)
        (list `(* ,(car e1) ) #t (caddr e1) )
)

(define (mkNot e1 )
        (list `(! ,(car e1) ) #t ( caddr e1 ) )
)

; (gen:grm (list (car x) (car t) G)  (Γ-up Γ (car x) (cadr t) (caddr t))  (Δ-up-old Δ x t) Σ (+ n 1) pmax))
(define (gen:grm G Γ Δ Σ n pmax)
       (if (>= n (length Γ))
              (gen:const (list G Γ))
              (gen:let ([x   (gen:const (list-ref Γ n))]
                        [Δ_x (gen:const (hash-ref Δ (car x)) )]
                        [t   (gen:expr Γ Δ_x Σ (cadr x) pmax) ])            
                       (gen:grm (list (car x) (car t) G) (Γ-up Γ (car x) (cadr t) (caddr t)) (batch-update Δ Γ (car x) (caddr t)) Σ (+ n 1) pmax)
              )
        )
 )

(define (Γ-item-up x y)
        (list (car x) (cadr x) (set-union (caddr x) y ))
  )

(define (Γ-up xs y b ty)
        (cond [(null? xs) null]
              [(eq? (car (car xs)) y) (cond [(eq? (cadr (car xs)) b)
                                                  (cons (list y b (set-union ty (caddr (car xs))))
                                                  (Γ-up (rest xs) y b ty))]
                                            [#t  (print "Inconsistency attempt to update Γ: xs=")
                                                 (print xs)
                                                 (print "  y  = ")
                                                 (print (list y b ty))
                                                 (print " attempt to update head nullable field with ")
                                                 (println b)
                                                 (rest xs)
                                             ] 
                                      )] 
              [(member y (caddr (car xs)) )  (cons (Γ-item-up (car xs) ty) (Γ-up (rest xs) y b ty))]
              [#t (cons (car xs) (Γ-up (rest xs) y b ty)) ]
        )
  )

(define (elem? x xs)
    (and (not (null? xs)) (or (eq? x (car xs)) (elem? x (cdr xs)) ) )
 )

; Key -> Var
; var in headSet(key) 
(define (Δ-up Δ key Γ var) 
   (foldr (lambda (Γval Δ)
                  (hash-update Δ key (lambda (hval) (set-union (list (car Γval) var) hval)) null) ) 
         ( hash-update Δ key (lambda (hv) (set-union hv (list var) ) ) )
         (filter (lambda (ΓEntry) (elem? var (caddr ΓEntry) ) ) Γ) ) )


(define (initΔ Γ)
  (foldr (lambda (t Δ) (batch-update Δ Γ (car t) (caddr t) )) (initΔ-1 (map car Γ))  Γ)  
 ) 

(define (batch-update Δ Γ var list-var)
  (foldr (lambda (lvar Δ) (Δ-up Δ lvar Γ var)) Δ list-var )
  )

(define (initΔ-1 x)
      (make-immutable-hash (map (lambda (t) [list t t] ) x) ) 
  )


(define (gen:Γ  maxVars [varSize 0])
  (gen:let ([vs (gen:list (gen:symbolVar varSize) #:max-length maxVars )]
            [ts (gen:repeat gen:boolean (length vs))])
           (gen:const (zipWith (lambda (v b) (list v b null) ) (remove-duplicates vs) ts ) ))
  )

(define (gen:peg maxVars maxLits maxDepth)
  (gen:let ([Γ (gen:Γ maxVars)]
            [n (gen:integer-in 0 maxLits) ]
            [Σ (gen:const (list-from-to 0 n))]
            [p (gen:integer-in 0 maxDepth)]
            [GΓ (gen:grm '∅ Γ (initΔ Γ) Σ 0 p)]
            [b gen:boolean ]
            [e0 (gen:expr (cadr GΓ) null Σ b p)])
           (gen:const (list (car GΓ) (car e0) (cadr GΓ)) )
           )
  )



#;(define Γ-test-0 '( (A #t ())
                    (B #f ())
                    (C #t ())
                    (D #t ()) ) )
#;(define Δ-test-0 (initΔ-1 '(A B C D) ) ) 
#;(define g '((A #t ()) (B #t (A C)) (C #t (A))) )

#;(define g2 '( (A #f ())  (B #f ()) (C #t ()) ) )
#;(define g2-1 '((A #f (B C)) (B #f ()) (C #t ())) )
#;(define d2-1 '#hash((A . (A)) (B . (A B)) (C . (A C))) )

#;(define g1 '((A #t (B)) (B #t (A C)) (C #t (A))) )


 
;(check-property genVars)
;(check-property zipSz)
;(check-property zipCons)
;(check-property  (make-config #:tests 1000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) pegDepth)
