#lang racket
(require racket/contract)

(provide (struct-out PEGFSyn)
         
         defaultFactory)

(struct PEGFSyn
        ( mkEps
          mkLit
          mkVar
          mkNot
          mkKle
          mkSeq
          mkAlt
          addRule
          mkEmptyGrm
          mkPEG
        )
        #:guard (struct-guard/c
                          (-> any/c)
                          (-> any/c any/c)
                          (-> any/c any/c)
                          (-> any/c any/c)
                          (-> any/c any/c)
                          (-> any/c any/c any/c)
                          (-> any/c any/c any/c)
                          (-> any/c any/c any/c any/c)
                          (-> any/c)
                          (-> any/c any/c (listof (cons/c any/c any/c)) any/c)
                  )
        #:transparent)


(define defaultFactory
     (PEGFSyn
      (lambda ()  'ϵ       )     ;mkEps
      (lambda (x) x)             ;mkLit
      (lambda (x) x)             ;mkVar
      (lambda (e) (list '! e  )) ;mkNot
      (lambda (e) (list '* e  )) ;mkKle
      (lambda (x y) (list '• x y)  ) ;mkSeq
      (lambda (x y) (list '/ x y)  ) ;mkAlt
      (lambda (g nt e) (list nt e g)  ) ;addRule
      (lambda () '∅ )                        ;mkEmptyGrm
      (lambda (g start gamma) (list g start gamma) )  ;mkPEG
     )
 )
         