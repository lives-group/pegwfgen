#lang racket

(provide TyPEG
         tySeq
         tyAlt
         tyKle
         tyNot
         tyVar
         tyEps
         tyLit
         customTy
         insNT
         tyPEG->string
         union-headset
         in-headset?
         nullable?
         headSet
         )

(struct TyPEG
        ( nul headset)
        #:guard (struct-guard/c boolean? list?)
        #:transparent)


(define (tySeq tyl tyr)
    (TyPEG (and (TyPEG-nul tyl) (TyPEG-nul tyr))
           (cond
             [(TyPEG-nul tyl) (set-union (TyPEG-headset tyl) (TyPEG-headset tyr)) ]
             [else            (TyPEG-headset tyl) ]))
  )

(define (tyAlt tyl tyr)
    (TyPEG (or (TyPEG-nul tyl) (TyPEG-nul tyr))
           (set-union (TyPEG-headset tyl) (TyPEG-headset tyr)))
  )

(define (tyKle typ)
    (TyPEG #t
           (TyPEG-headset typ))
  )

(define (tyNot typ)
    (TyPEG #t
           (TyPEG-headset typ) )
  )

(define (tyVar v)  (TyPEG #t (list v)) )
(define (tyEps)  (TyPEG #t null) )
(define (tyLit) (TyPEG #f null))
(define (customTy nul hs)
   (TyPEG nul hs))

(define (insNT ty v)  (TyPEG (TyPEG-nul ty) (set-union (TyPEG-headset ty) (list v)) ))

(define (union-headset ty headset)
  (TyPEG (TyPEG-nul ty) (set-union (TyPEG-headset ty) headset) ))

(define (elem? x xs)
   (cond
     [(null? xs) #f]
     [else (or (equal? x (car xs)) (elem? x (cdr xs)))]
  ))

(define (in-headset? ty x)
  (elem? x (TyPEG-headset ty) ))

(define (nullable? ty)
  (TyPEG-nul ty))

(define (headSet ty)
  (TyPEG-headset ty))

(define (bool->string b)
   (cond [b "true"] [else "false"])
  )


(define (tyPEG->string ty)
  (string-append "<"
                 (bool->string (TyPEG-nul ty))
                 ", "
                 (~v (TyPEG-headset ty))
                 ">")
)