#lang racket

(require cover)
(require "../peg-gen.rkt"
         "../tests/verf-well-formed.rkt"
         "experiment-wellFormed.rkt"
         rackcheck
         rackunit)
         
(check-property (make-config #:tests 100000 #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24)) wellformed-ford)
