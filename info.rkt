#lang info
(define collection "peg-gen")
<<<<<<< HEAD
(define deps '("base" "algorithms" "rackcheck" "rackunit"))
(define build-deps '("algorithms" "rackcheck" "scribble-lib" "racket-doc" "rackunit-lib"))
=======
(define deps '("algorithms"
               "base" "rackcheck" "rackunit"))
(define build-deps '("cover-lib"
                     "typed-peg"
                     "algorithms"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
>>>>>>> 7194d2b44f229c11ff60ae9837e3561c9d8b4988
(define scribblings '(("scribblings/peg-gen.scrbl" ())))
(define pkg-desc "Type safe (and thus well formed) random peg generator")
(define version "0.0")
(define pkg-authors '(EltonMC RodrigoGR LeonardoVSR ))
