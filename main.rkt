#lang racket/base

(require "./peg-gen.rkt"
         "./gen-utils.rkt"
         "./tests/simpleTests.rkt"
         "./tests/well-formed.rkt"
         rackcheck
         rackunit)

(provide (all-from-out "./peg-gen.rkt" ))

(module+ test
   (require rackunit
            rackcheck
            "./tests/simpleTests.rkt")
 
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (begin (check-property genVars)
         (check-property zipSz)
         (check-property zipCons)
         (check-property no-left-recursion)
         (check-property obey-constraint)
         (check-property pegDepth)
         (check-property wellformed-ford)
         (check-property ill-formed-ford)
         (check-property pegDepthDist))
  )

(module+ main
    (require "peg-gen.rkt")
    (provide (all-from-out "peg-gen.rkt" ))  
)
