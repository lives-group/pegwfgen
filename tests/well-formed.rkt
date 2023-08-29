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

(define-property wellformed-ford ([peg  (gen:peg 3 5 2)])
    ;(println peg)
    (check-equal?  (is-WF (getGrammar peg) (getExpression peg)) #t)
  )

(define-property null-ill-formed ([peg  (gen:ill-expr null null '(0 1 2) #t 2 )])
    (check-equal? (is-WF '∅ (car peg) ) #f)
  )

(define-property not-null-ill-formed ([peg  (gen:ill-expr null null '(0 1 2) #f 2 )])
    (check-equal? (is-WF '∅ (car peg) ) #f)
  )

(define-property ill-formed-ford ([peg  (gen:ill-peg 3 5 2)])
    (check-equal?  (is-WF (getGrammar peg) (getExpression peg)) #f)
  )

;(make-config #:tests 10)

#;(define (allTypesMatch g g1 )
   (andmap (lambda (t) (matchTypes t (assoc (car t) g) )) g1)
  )

#;(define-property type-checks([peg  (gen:peg 3 5 2)])
    (check-equal?  (testgen peg) #t)
  )

#;(define-property type-contexts-match([peg  (gen:peg 3 5 2)])
    (check-equal?  (allTypesMatch (solution2context (infer (peg2struct peg))) (last peg)) #t)
  )


