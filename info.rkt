#lang info
(define collection "peg-gen")
(define deps '("base" "rackcheck" "rackunit"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/peg-gen.scrbl" ())))
(define pkg-desc "Type safe (and thus well formed) random peg generator")
(define version "0.0")
(define pkg-authors '(EltonMC RodrigoGR LeonardoVSR ))
