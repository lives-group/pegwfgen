#lang racket
(require racket/match)
(provide (all-defined-out))

;; Helpers

(define (⇀0? xs)
  (member 0 xs))

(define (⇀1? xs)
  (member 1 xs))

(define (⇀f? xs)
  (member 'f xs))

(define (⇀s? xs)
  (or (member 0 xs) (member 1 xs)))

(define (=> c s)
  (if c
      s
      '()))


(define (nt-list grm)
  (match grm
    ['∅ null]
    [(list s e g) (cons s (nt-list g))]
    [_ null])
  )
;; Analysis of Grammars
 ;; e ⇀ 1 then e might succeed while consuming at least one terminal
 ;; e ⇀ 0 then e might succeed on some input string while consuming no input
 ;; e ⇀ f then e might fail on some input
(define (⇀ grammar e [nts '()])
  (match e
    ['ϵ '(0)]
    [(list '/ e1 e2) (let* ([r1 (⇀ grammar e1 nts)]
                            [r2 (⇀ grammar e2 nts)])
                       (if (⇀f? r1) 
                           (set-union r2 (remove 'f r1))
                           r1))]
    [(list '• e1 e2) (let* ([r1 (⇀ grammar e1 nts)]
                            [r2 (⇀ grammar e2 nts)])
                       (set-union (=> (and (⇀0? r1) (⇀0? r2)) '(0))
                                  (=> (and (⇀1? r1) (⇀s? r2)) '(1))
                                  (=> (and (⇀s? r1) (⇀1? r2)) '(1))
                                  (=> (⇀f? r1) '(f))
                                  (=> (and (⇀s? r1) (⇀f? r2)) '(f))))] 
    [(list '* e1) (let* ([r1 (⇀ grammar e1 nts)])
                    (set-union (=> (⇀1? r1) '(1))
                               (=> (⇀f? r1) '(0))
                               ))] 

    [(list '! e1) (let* ([r1 (⇀ grammar e1 nts)])
                    (set-union (=> (⇀s? r1) '(f))
                               (=> (⇀f? r1) '(0))))] 

    [(? number?) '(1 f)]
    [(? symbol?)  (if (member e nts)
                      '()
                      (⇀ grammar (lookup-nt grammar e) (cons e nts)))]
    )
  )


;; Look up for the expression which corresponds the grammar

(define (lookup-nt grammar snt)
  (if (eq? grammar '∅)
      #f
      (let ([nt (car grammar)]
            [exp-nt (second grammar)]
            ) 
        (if (eq? nt snt)
            exp-nt
            (lookup-nt (third grammar) snt))
        )
      )
  )

;; Well-Formed Grammars

#;(define (is-WF grammar e non-terminal) ;; (grammar expression non-terminal)
  (cond
    [(list? e) (let ([id (car e)])
                    (cond [(eq? id '/)  (and (is-WF grammar (cadr e) non-terminal) (is-WF grammar (caddr e) non-terminal))] 
                          [(eq? id '•)  (and (is-WF grammar (cadr e) non-terminal)
                                             (or (not (member '0 (⇀ grammar (cadr e))))
                                                 (is-WF grammar (caddr e) non-terminal))
                                 )]
                          [(eq? id '!)  (is-WF grammar (cadr e) non-terminal)]
                          [(eq? id '*)  (and (not (member '0 (⇀ grammar (cadr e)))) 
                                        (is-WF grammar (cadr e) non-terminal))]
                          [else   #f] 
              )
        )]
       [(number? e)    #t]
       [(equal? e 'ϵ)  #t]
       [(not (equal? grammar '∅)) (if (member e non-terminal)
                                        #f
                                        (is-WF grammar (lookup-nt grammar e) (cons e non-terminal)))] 
       [else  #f]
            )
      )
 
#;(define (is-WF-exp grammar e non-terminal) ;; (grammar expression non-terminal)
  (match e
    ['ϵ  #t]
    [(list '/ l r)  (and (is-WF-exp grammar l non-terminal) (is-WF-exp grammar r non-terminal))] 
    [(list '• l r)  (and (is-WF-exp grammar l non-terminal)
                           (or (not (member '0 (⇀ grammar l)))
                               (is-WF-exp grammar r non-terminal)
                               ))]
    [(list '! x)    (is-WF-exp grammar x non-terminal)]
    [(list '* x)    (and (not (member '0 (⇀ grammar  x))) 
                         (is-WF-exp grammar x non-terminal))]          
    [(? number? _)    #t]
    
    [(? symbol? x) (if (member x non-terminal)
                       #f
                       (is-WF-exp grammar (lookup-nt grammar x) (cons x non-terminal)))] 
    [_  #f]
  )
)

(define (behead x)  (if (null? x) x (cdr x)) )
(define (slide xs ys)
   (if (null? ys) xs (cons (car ys) xs)) )
(define (is-WF-exp grammar e wflist non-terminal) ;; (grammar expression non-terminal)
  (match e
    ['ϵ  #t]
    [(list '/ l r)  (and (is-WF-exp grammar l wflist non-terminal)
                         (is-WF-exp grammar r wflist non-terminal))] 
    [(list '• l r)  (and (is-WF-exp grammar l wflist non-terminal)
                           (if (not (member '0 (⇀ grammar l)))
                               (is-WF-exp grammar r (slide wflist non-terminal) null)
                               (is-WF-exp grammar r wflist non-terminal)
                               ))]
    [(list '! x)    (is-WF-exp grammar x wflist non-terminal)]
    [(list '* x)    (and (not (member '0 (⇀ grammar  x))) 
                         (is-WF-exp grammar x wflist non-terminal))]          
    [(? number? _)    #t]
    
    [(? symbol? x) (cond
                    [(member x wflist)       #t]
                    [(member x non-terminal) #f]
                    [else (is-WF-exp grammar (lookup-nt grammar x) wflist (cons x non-terminal))])] 
    [_  #f]
  )
)

(define (is-WF-g grammar) ;; (grammar expression non-terminal)
     (let ([xs (nt-list grammar)])
         (foldr (lambda (x y) (and x y) ) #t (map (lambda (z) (is-WF-exp grammar z null null)) xs))
       )
)

(define (is-WF grammar e) ;; (grammar expression non-terminal)
   (and (is-WF-exp grammar e null null)
        (is-WF-g grammar))
)
