#lang racket

(require racket/set)
(require rackcheck)
(require rackunit)
(require algorithms)


(provide gen:expr
         gen:grm
         gen:Γ
         gen:peg
         gen:var
         gen:symbolVar
         initΔ
         no-left-recursion
         obey-constraint
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


#;(define (Γ-val Γ v)
        (if (null? Γ)
            null
            (if (eq? (car (car Γ)) v)
                (car Γ)
                (Γ-val (cdr Γ) v) )
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
              [(eq? (car (car xs)) y) (cond [(eq? (cadr (car xs)) b)  (cons (list y b (set-union ty (caddr (car xs)))) (Γ-up (rest xs) y b ty))]
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


(define (list-from-to l u)
  (if (> l u)
      null
      (cons l (list-from-to (+ 1 l) u))
      )
  )

(define (gen:listNat n k)
  (if (eq? n 0) (gen:bind (gen:integer-in 0 k) (lambda (e) (gen:const (list e))) )
      (gen:bind (gen:listNat (- n 1) k)
                (lambda (xs) (gen:bind ( gen:integer-in 0 k)
                                      (lambda (x) (gen:const (list* x xs)) ) ))))
  )

(define (gen:repeat g n)
  (if (eq? n 0)
      (gen:const null)
      (gen:bind (gen:repeat g (- n 1) )
                (lambda (xs) (gen:bind g
                                       (lambda (x) (gen:const (cons x xs)) ) ))))
  )


(define (gen:var n)
  (gen:map (gen:listNat n 23) (lambda (s) (list->string (map (lambda (z) (integer->char (+ z 65))) s ))  ) )
  )

(define (gen:symbolVar n)
  (gen:map (gen:listNat n 23) (lambda (s) (string->symbol (list->string (map (lambda (z) (integer->char (+ z 65))) s )))  ) )
  )


#;(define (zip xs ys)
  (cond
       [(null? xs) null]
       [(null? ys) null]
       [#t (cons (list (car xs) (car ys) ) (zip (cdr xs) (cdr ys) ) )]
  )
)

(define (zipWith func xs ys)
  (cond
       [(null? xs) null]
       [(null? ys) null ]
       [#t (cons (func (car xs) (car ys) ) (zipWith func (cdr xs) (cdr ys) ) )]
  )
)



;(sample (gen:expr null '() '(0 1)  #f 3) 10)
;(define (up2 n) (if (<= n 0) (list 0) (cons 0 (up2 (- n 1)) ) ))


#;(define (circle? xs ys Γ)
  (if (null? xs)
      #f
      (or (elem? (car xs) ys)
          (circle? (append (cdr xs) (caddr (Γ-val Γ (car xs) ))) (cons (car xs) ys) Γ)
      ) )
  )

(define (circled? x ys Γ)
  (if (null? (caddr (Γ-val Γ x) ))
      #f
      (or (elem?  x ys)
          (ormap (lambda (z) (circled? z (cons x ys) Γ)) (caddr (Γ-val Γ x))) 
      ) )
  )



#;(define (naive-test vars Σ p n)
     (for ([k n])
          (let ( [ws (last (randPEG vars Σ p)) ] )
            (if (ormap (lambda (x) (circled? (car x) '() ws))  ws )
                (begin (display ws) (display "\n"))
                (display "ok\n")
                )
            )
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
    (check-equal? (foldr (lambda (e rb) (and (not (elem? e Δ)) rb) ) #t (last peg)) #t)
  )
                             
(define-property genNats ([n (gen:integer-in 0 10)]
                          [k (gen:integer-in 0 5)]
                          [ln  (gen:listNat n k)])
    (check-equal?  (length ln) (+ n 1))
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

(define-property pegDepth ([peg (gen:peg 10 10 4)])
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


;(check-property genNats)
;(check-property genVars)
;(check-property zipSz)
;(check-property zipCons)
;(check-property  (make-config #:tests 1000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) pegDepth)
