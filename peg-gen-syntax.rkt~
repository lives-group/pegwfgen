#lang racket

(require "peg-gen-syntax-factory.rkt")

(provide
    (struct-out GEps)
    (struct-out GLit)
    (struct-out GVar)
    (struct-out GAlt)
    (struct-out GSeq)
    (struct-out GNot)
    (struct-out GKle)
    (struct-out GPEG)
    peg->string
    pexp->string
    PEGStructF)


(struct GPEG (nt start gamma)  #:transparent)
(struct GEps ()                #:transparent)
(struct GLit (chr)        #:transparent)
(struct GVar (var)        #:transparent)
(struct GAlt (left right) #:transparent)
(struct GSeq (left right) #:transparent)
(struct GNot (exp)        #:transparent)
(struct GKle (exp)        #:transparent)

(define (nt s peg)
   (hash-ref (GPEG-nt peg) s)
  )

(define (type-of s peg)
    (cond
      (hash-ref (GPEG-gamma peg) s)
      )
  )

(define (mkUntypedPEG nt-list start-exp)
     (GPEG (make-immutable-hash nt-list) start-exp (make-immutable-hash))
  )

(define (mktypedPEG nt-list start-exp ty-list)
     (GPEG (make-immutable-hash nt-list) start-exp (make-immutable-hash ty-list))
  )

(define (add-nt p nt exp)
     (GPEG (hash-set (GPEG-nt p) nt exp) (GPEG-start p) (GPEG-gamma p))
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
        [(struct GEps ())    "ϵ"]
        [(struct GLit (c))    (string-append "'" (string c) "'")]
        [(struct GVar (s))   (string-append "\"" s "\"")]
        [(struct GAlt (l r)) (parens (> n 2) (string-append (expr-prec->string 2 l)
                                                           (expr-prec->string 2 r)))]
        [(struct GSeq (l r)) (parens (> n 1) (string-append (expr-prec->string 1 l) "/"
                                                           (expr-prec->string 1 r)))]
        [(struct GKle (p))   (parens (> n 4) (string-append (expr-prec->string 4 p) "*"))  ]
        [(struct GNot (p))   (parens (> n 3) (string-append "!" (expr-prec->string 3 p) )) ]
     )
  )
  

(define (nt->str xs) (map (lambda (x) (string-append (car x) (expr-prec->string 0 (cdr x)) ) )) )

(define (pexp->string e ) 
    (expr-prec->string 0 e))

(define (peg->string e ) 
    (append (hash-map (GPEG-nt e)
                      (lambda (s exp) (string-append s "<-" (expr-prec->string 0 exp) "\n")) )
            (list (expr-prec->string 0 (GPEG-start e)))
    )
)

(define PEGStructF
    (PEGFSyn
      (lambda ()  (GEps) )         ;mkEps
      (lambda (x) (GLit x ))       ;mkLit
      (lambda (x) (GVar x ))       ;mkVar
      (lambda (e) (GNot e ))       ;mkNot
      (lambda (e) (GKle e ))       ;mkKle
      (lambda (x y) (GSeq x y)  ) ;mkSeq
      (lambda (x y) (GAlt x y)  ) ;mkAlt
      (lambda (g nt e) (hash-set g nt e)  ) ;addRule
      (lambda () (make-immutable-hash) )           ;mkEmptyGrm
      (lambda (g start gamma) (GPEG g start gamma) )  ;mkPEG
     )
  )
