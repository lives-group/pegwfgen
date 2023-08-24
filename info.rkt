#lang info
(define collection "peg-gen")
(define deps '("algorithms"
               "base" "rackcheck" "rackunit"))
(define build-deps '("cover-lib"
                     "algorithms"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/peg-gen.scrbl" ())))
(define pkg-desc "Type safe (and thus well formed) random peg generator")
(define version "0.1")
(define pkg-authors '(EltonMC RodrigoGR LeonardoVSR ))
