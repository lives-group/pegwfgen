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

;; Analysis of Grammars
 ;; e ⇀ 1 then e might succeed while consuming at least one terminal
 ;; e ⇀ 0 then e might succeed on some input string while consuming no input
 ;; e ⇀ f then e might fail on some input
(define (⇀ grammar e [nts '()])
  (match e
    ['ε '(0)]
    [(list '/ e1 e2) (let* ([r1 (⇀ grammar e1 nts)]
                            [r2 (⇀ grammar e2 nts)])
                       (if (⇀f? r1) 
                           (append r2 (remove 'f r1))
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

(define (is-WF grammar e non-terminal) ;; (grammar expression non-terminal)
  (if (list? e)
      (let ((id (car e)))
        (cond [(eq? id '/)  (and (is-WF grammar (cadr e) non-terminal) (is-WF grammar (caddr e) non-terminal))] 
              [(eq? id '•)  (and (is-WF grammar (cadr e) non-terminal)
                                 (or (not (member '0 (⇀ grammar (cadr e))))
                                     (is-WF grammar (caddr e) non-terminal))
                                 )]
              [(eq? id '!)  (is-WF grammar (cadr e) non-terminal)]
              [(eq? id '*)  (and (not (member '0 (⇀ grammar (cadr e)))) 
                                 (is-WF grammar (cadr e) non-terminal))]
              [else  #f] 
              )
        )
      (cond [(number? e) #t]
            [(eq? e 'ε)  #t]
            [(not (eq? grammar '∅)) (if (member e non-terminal)
                                        #f
                                        (is-WF grammar (lookup-nt grammar e) (cons e non-terminal)))] 
            [else  #f]
            )
      )
 
  )

