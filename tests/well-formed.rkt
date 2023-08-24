#lang racket
(require "verf-well-formed.rkt")
(require rackunit)
(require rackcheck)
(require "../peg-gen.rkt")

;; Testing if the generated PEG is Well-Formed

;; Helpers functions to get Grammar and the Expression from gen:peg

(define (getGrammar e)
  (car e)
  )

(define (getExpression e)
  (car (cdr e))
  )


;(check-property wellformed-ford)
;(check-property (make-config #:tests 20) wellformed-ford)
(define-property wellformed-ford ([peg  (gen:peg 3 5 2)])
    (check-equal?  (is-WF (getGrammar peg) (getExpression peg) '()) #t)
  )

(make-config #:tests 10)

#;(define (allTypesMatch g g1 )
   (andmap (lambda (t) (matchTypes t (assoc (car t) g) )) g1)
  )

#;(define-property type-checks([peg  (gen:peg 3 5 2)])
    (check-equal?  (testgen peg) #t)
  )

#;(define-property type-contexts-match([peg  (gen:peg 3 5 2)])
    (check-equal?  (allTypesMatch (solution2context (infer (peg2struct peg))) (last peg)) #t)
  )


