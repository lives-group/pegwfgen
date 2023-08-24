#lang racket

(require cover)
(require "../peg-gen.rkt"
         "../tests/verf-well-formed.rkt"
         rackcheck
         rackunit)

(provide (all-defined-out))

 
(define (getGrammar e)
  (car e)
  )

(define (getExpression e)
  (car (cdr e))
  )


;(check-property wellformed-ford)
;(check-property (make-config #:tests 20) wellformed-ford)
(define-property wellformed-ford ([peg  (gen:peg 10 10 4)])
    (check-equal?  (is-WF (getGrammar peg) (getExpression peg) '()) #t)
  )


  
; For experimetns uncomment line below !

;(check-property (make-config #:tests 100 #:deadline (+ (current-inexact-milliseconds) 3600000)) wellformed-ford)

;(check-property (make-config #:tests 1000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) wellformed-ford)
;(check-property (make-config #:tests 20000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) wellformed-ford)
;(check-property (make-config #:tests 30000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) wellformed-ford)
;(check-property (make-config #:tests 40000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) wellformed-ford)
;(check-property (make-config #:tests 50000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) wellformed-ford)
;(check-property (make-config #:tests 100000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) wellformed-ford)
;(check-property (make-config #:tests 200000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) wellformed-ford)

