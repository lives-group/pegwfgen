#lang racket

(require "peg-gen-syntax-factory.rkt")

(provide
    (struct-out Eps)
    (struct-out Lit)
    (struct-out Var)
    (struct-out Alt)
    (struct-out Seq)
    (struct-out Not)
    (struct-out Kle)
    PEG
    peg->string
    pexp->string
    PStructF)


(struct PEG (nt start gamma)  #:transparent)
(struct Eps ()                #:transparent)
(struct Lit (chr)        #:transparent)
(struct Var (var)        #:transparent)
(struct Alt (left right) #:transparent)
(struct Seq (left right) #:transparent)
(struct Not (exp)        #:transparent)
(struct Kle (exp)        #:transparent)

(define (nt s peg)
   (hash-ref (PEG-nt peg) s)
  )

(define (type-of s peg)
    (cond
      (hash-ref (PEG-gamma peg) s)
      )
  )

(define (mkUntypedPEG nt-list start-exp)
     (PEG (make-immutable-hash nt-list) start-exp (make-immutable-hash))
  )

(define (mktypedPEG nt-list start-exp ty-list)
     (PEG (make-immutable-hash nt-list) start-exp (make-immutable-hash ty-list))
  )

(define (add-nt p nt exp)
     (PEG (hash-set (PEG-nt p) nt exp) (PEG-start p) (PEG-gamma p))
  )

(define (parens b  s) 
     (match b
           [#f   s]
           [else  (string-append "(" s ")")]
  )
)

;Primary     5
;Kle         4
;Not         3
;Sequence    2 Left
;alternative 1 Left
(define (expr-prec->string n e ) 
    (match e
        [(struct Eps ())    "ϵ"]
        [(struct Lit (c))    (string-append "'" (string c) "'")]
        [(struct Var (s))   (string-append "\"" s "\"")]
        [(struct Alt (l r)) (parens (> n 2) (string-append (expr-prec->string 2 l)
                                                           (expr-prec->string 2 r)))]
        [(struct Seq (l r)) (parens (> n 1) (string-append (expr-prec->string 1 l) "/"
                                                           (expr-prec->string 1 r)))]
        [(struct Kle (p))   (parens (> n 4) (string-append (expr-prec->string 4 p) "*"))  ]
        [(struct Not (p))   (parens (> n 3) (string-append "!" (expr-prec->string 3 p) )) ]
     )
  )
  

(define (nt->str xs) (map (lambda (x) (string-append (car x) (expr-prec->string 0 (cdr x)) ) )) )

(define (pexp->string e ) 
    (expr-prec->string 0 e))

(define (peg->string e ) 
    (append (hash-map (PEG-nt e)
                      (lambda (s exp) (string-append s "<-" (expr-prec->string 0 exp) "\n")) )
            (list (expr-prec->string 0 (PEG-start e)))
    )
)

(define PStructF
    (PEGFSyn
      (lambda ()  (Eps) )         ;mkEps
      (lambda (x) (Lit x ))       ;mkLit
      (lambda (x) (Var x ))       ;mkVar
      (lambda (e) (Not e ))       ;mkNot
      (lambda (e) (Kle e ))       ;mkKle
      (lambda (x y) (Seq x y)  ) ;mkSeq
      (lambda (x y) (Alt x y)  ) ;mkAlt
      (lambda (g nt e) (hash-set g nt e)  ) ;addRule
      (lambda () (make-immutable-hash) )           ;mkEmptyGrm
      (lambda (g start gamma) (PEG g start gamma) )  ;mkPEG
     )
  )
