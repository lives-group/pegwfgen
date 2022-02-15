#lang racket/base

(require "./peg-gen.rkt")
(require rackcheck)
(require rackunit)
(provide (all-from-out "./peg-gen.rkt" ))

(module+ test
  
(module+ test
   (require rackunit)
   (require rackcheck))
 
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (begin (check-property no-left-recursion)
         (check-property obey-constraint))
  )

(module+ main
    (require "peg-gen.rkt")
    (provide (all-from-out "peg-gen.rkt" ))  
)
