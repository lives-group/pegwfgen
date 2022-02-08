#lang racket
;(require redex)
(require  racket/set)
(require "./peg.rkt")
(require "./WFverf.rkt")
(require rackcheck)

; Processo de test de GitHub, ver como ele faz isso sozinho
; 
;

(define myGen (make-pseudo-random-generator))

(define (h n)
        (- n 1)
  )


(define (genPegExpr Γ Δ Σ b p)
       (cond
             [(equal? p 0)  (gen:one-of (append (mkListVar Γ Δ b)
                                                (if b
                                                    (list (list 'ε #t '()) )  ; Falta possibilidades de termianis AQUI -- Não falta não !
                                                    (map (lambda (x) (list x #f '())) Σ) )
                                         )) ]
             [(and (> p 0) b)    (gen:choice (gen:bind (genPegExpr Γ Δ Σ b (h p))
                                                       (lambda (t)  (gen:bind (genPegExpr Γ Δ Σ b (h p)) (lambda (s) (gen:const  (mkSeq t s) ) ) ) ) )
                                             (gen:bind (genPegExpr Γ Δ Σ #t (h p))
                                                       (lambda (t)  (gen:bind (genPegExpr Γ Δ Σ (car (sample gen:boolean 1 myGen)) (h p))
                                                                              (lambda (s) (gen:const  (mkAlt t s) )) ) ) )
                                             (gen:bind (genPegExpr Γ Δ Σ #f (h p))
                                                       (lambda (t)  (gen:bind (genPegExpr Γ Δ Σ #t (h p)) (lambda (s) (gen:const  (mkAlt t s) )) ) ) )
                                             (gen:bind (genPegExpr Γ Δ Σ #f (h p))
                                                       (lambda (t) (gen:const (mkNot t) ) ))
                                             (gen:bind (genPegExpr Γ Δ Σ #f (h p))
                                                       (lambda (t) (gen:const (mkKle t) ) ))
                                  )]
             [(and (> p 0) (not b))  (gen:choice (gen:bind (genPegExpr Γ Δ Σ #t (h p))
                                                       (lambda (t)  (gen:bind (genPegExpr Γ Δ Σ #f (h p)) (lambda (s) (gen:const  (mkSeq t s) ) ) ) ) )
                                                 (gen:bind (genPegExpr Γ Δ Σ #f (h p))
                                                           (lambda (t)  (gen:bind (genPegExpr Γ Δ Σ (car (sample gen:boolean 1 myGen)) (h p))
                                                                                  (lambda (s) (gen:const  (mkSeq t s) ) ) ) ) )
                                                 (gen:bind (genPegExpr Γ Δ Σ #f (h p))
                                                           (lambda (t)  (gen:bind (genPegExpr Γ Δ Σ #f (h p)) (lambda (s) (gen:const  (mkAlt t s) )) ) ) )
                                  )]
           )  
)


(define (Γ-val Γ v)
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
     (map (lambda (y) (list (car y) (cadr y) (append (caddr y) (list (car y)) ) ))
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

(define (genGrammar G Γ Δ Σ n pmax)
       (if (>= n (length Γ))
           [begin
             (display "\n and Last Δ = ")
             (display Δ)
             (display "\n")
             (list G Γ) ]
           (let* ([x (list-ref Γ n)]
                  [Δ_x (hash-ref Δ (car x)) ]
                  [t   (car (sample (genPegExpr Γ Δ_x Σ (cadr x) pmax) 1) ) ]
                  [Δ1  (foldr (lambda (z Δ2) (hash-update Δ2 z (lambda (l) (set-union l (list (car x))  )  ) ) ) Δ (caddr t) )]
                  [Γ1  (Γ-up Γ (car x) (cadr t) (caddr t)) ])

                  ;(display "Body ")
                  ;(display t)
                  ;(display " Generated for " )
                  ;(display x)
                  ;(display "\n With Γ = ")
                  ;(display Γ)
                  ;(display "\n and Δ = ")
                  ;(display Δ)
                  ;(display "\n and Δ_x = ")
                  ;(display Δ_x)
                  ;(display "\n")
             
                  (genGrammar (list (car x) (car t) G) Γ1 Δ1 Σ (+ n 1) pmax)
           )
        )
 )

(define (Γ-item-up x y)
        (list (car x) (cadr x) (set-union (caddr x) y ))

  )

(define (Γ-up xs y b ty)
        (cond [(null? xs) null]
              [(eq? (car (car xs)) y) (cond [(eq? (cadr (car xs)) b)  (cons (list y b (set-union ty (caddr (car xs)))) (Γ-up (rest xs) y b ty))]
                                            [#t  (print (car xs))
                                                 (print " attempt to update with ")
                                                 (println b)]
                                      )] 
              [(member y (caddr (car xs)) )  (cons (Γ-item-up (car xs) ty) (Γ-up (rest xs) y b ty))]
              [#t (cons (car xs) (Γ-up (rest xs) y b ty)) ]
        )
  )

(define (elem? x xs)
    (and (not (null? xs)) (or (eq? x (car xs)) (elem? x (cdr xs)) ) )
      )

; Still worng
#;(define (Δ-up Δ key Γ var) 
  (foldr (lambda (Γval Δ)  (hash-update Δ key (lambda (hval) (set-union (list (car Γval) var) hval)) null) ) 
         Δ
         (filter (lambda (ΓEntry) (elem? var (caddr ΓEntry) ) ) Γ) )
    )

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


  

(define (randPEG vars Σi p)
        (let* [(ts  (sample gen:boolean (length vars) myGen))
               (bol  (car (sample gen:boolean (length vars) myGen)))
               (Γi (zipWith (lambda (v b) (list v b null)  ) vars ts))
               (Δi (initΔ Γi))
               (GΓ (genGrammar '∅ Γi Δi Σi 0 p) )
               (e0  (car (sample (genPegExpr (cadr GΓ) null Σi bol p) 1)) )]
              (list (car GΓ) (car e0) (cadr GΓ) ))
  )

(define (randPEG-ERR Σi p)
        (let* [(vars (list 'X0 'X1 'X2) )
               (ts   (list #t #t #t ))
               (bol  #t)
               (Γi (zipWith (lambda (v b) (list v b null)  ) vars ts))
               (Δi (initΔ Γi))
               (GΓ (genGrammar '∅ Γi Δi Σi 0 p) )
               (e0  (car (sample (genPegExpr (cadr GΓ) null Σi bol p) 1)) )]
              (list (car GΓ) (car e0) (cadr GΓ) ))
  )

(define (zip xs ys)
  (cond
       [(null? xs) null]
       [(null? ys) null]
       [#t (cons (list (car xs) (car ys) ) (zip (cdr xs) (cdr ys) ) )]
  )
)

(define (zipWith func xs ys)
  (cond
       [(null? xs) null]
       [(null? ys) null]
       [#t (cons (func (car xs) (car ys) ) (zipWith func (cdr xs) (cdr ys) ) )]
  )
)

(define Γ0 '( (A #f (B C)) (B #t (C)) (C #t ()) ))
(define Γ1 '( (A #f ()) ) )
(define Γ2 '( (A #f ()) (B #f ()) ) )
(define Γ3 '( (A #f (B C)) (B #t ()) (C #t ()) (D #t (C)) ))


;(sample (genPegExpr null '() '(0 1)  #f 3) 10)
(define (up2 n) (if (<= n 0) (list 0) (cons 0 (up2 (- n 1)) ) ))

(define (runTest v Σ p n)
        (for ([x n])
             
             (let ([pg (randPEG-ERR Σ p) ])
                  (println pg)
                  (print "Grammar: ")
                  (println (car pg))
                  (display "\n")
              )
        )
  )

(define Γ-test-0 '( (A #t ())
                    (B #f ())
                    (C #t ())
                    (D #t ()) ) )  
(define Δ-test-0 (initΔ-1 '(A B C D) ) )

; Loopinf is-WF.
;
; '((B (• (* 0) (/ A ε)) (A (• (• ε 1) (• B ε)) ∅)) (• (/ 1 A) (! 1)) ((A #f ()) (B #t (A))))
; '(0)'(1 ⊥)'(1). Interactions disabled; out of memory
;
; A → ε1Bε      
; B → 0*(A / ε)
;
;(1 / A) !1
;
;(is-WF '(B (• (* 0) (/ A ε)) (A (• (• ε 1) (• B ε)) ∅)) '(• (/ 1 A) (! 1)) '() )
;
;'((D (! (• (• 0 1) (/ 0 0)))
;  (C (* (• (• 0 1) (• ε 0)))
;  (B (• (/ (• C 0) (* 0)) (• (• C ε) (/ 1 0)))
;  (A (/ (/ (• B 0) (/ 1 D)) (• (/ B 0) (• C D))) ∅))))
;
; A → B0
;   / (1/ D)
;   / (B/0)CD
; B → (C0 / *0) C ε (1 / 0)
; C → (01ε0)*
; D → !(01(0 / 0))
;(* (• (/ B B) (/ B 0)))
;
;
;((A #t (C D B)) (B #f (C)) (C #t ()) (D #t ())))
;
;(randPEG '(A B) '(0 1) 1)
;'((B (• ε 0) (A (• 0 ε) ∅)) (• 0 ε) ((A #f ()) (B #f ())))
;pegar o corpo de B e verificar se ele é = (B #f ())

;(randPEG '(A B) '(0 1) 2)
;
;
;((X2 (/ (• ε 2) (• X0 ε))
; (X1 (/ (/ 2 X2) (/ 3 X2))
; (X0 (/ (* 2) (/ 3 X1)) ∅)))
;
; (/ (* 0) (/ ε X0))
;
; ((X0 #t (X0 X2 X1)) (X1 #t (X1 X0 X2)) (X2 #t (X0 X1 X2)))
;
