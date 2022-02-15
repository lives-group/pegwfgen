#lang scribble/manual
@require[ scribble/example
          @for-label["../peggen.rkt"
                     racket/base
                     racket/contract
                     racket/string
                     rackcheck
                     rackunit]]


@title{peg-gen}
@author{E. M. Cardoso}

@defmodule[peg-gen/doc]

@(begin
  (define ev (make-base-eval))
  (ev '(require rackcheck racket/list racket/stream rackunit "../pegwfgen/peggen.rkt"))
  (define-syntax-rule (ex body ...)
    (begin
     (random-seed 1337)
     (examples
      #:eval ev
      #:label #f
      body ...))))

@section{Overview}

This module define a collection of  PEG rackcheck generatos. 
In order to generate well formed PEGs, each term is always generate as a
tern (peg, nullable, headset). This approach allows for incremental type correct
construction of a term. 

Whenever a grammar is generated the context of all possible varibales (Γ) is ketp and
upadated accordiling. An aditional context called Δ is used to avoid generated calls
to non terminals that would result in left recursion. 

The general use case for this library is the function gen:peg whose
arguments are the maximum number of varibales, the maximun number of
terminal symbols, and maximun depth of a peg. 

@ex[ (sample (gen:peg 3 2 1) 3) ]

The output of the generator is a list of triple (G, e, Context)
@itemlist[
  @item{ G (Grammar) : A list of variables followe by a peg. The list ends with ∅ (empty grammar)}
  @item{ e: A peg expression}
  @item{ Context : The type context for gamma}]

@section{Generators reference}

@defproc[(gen:Γ    [maxVars exact-positive-integer?] 
                   [#:varSize varSize exact-positive-integer? 0])
                   gen?]{
   Creates a generator that generates a list of variable names (as symbols) with no more than maxVars elements. The varSize is number of letters in the varibale (0 means one letter, 1 means two letters and so on).
}

@defproc[(initΔ  [Γ (list/c symbol? boolean? (listof symbol?))])
                   (hash/c symbol? (listof symbol?))]{
   Constructs an initial correct Δ table from the given Γ.
}


@defproc[(gen:peg [maxVars  exact-positive-integer?] 
                  [maxLits  exact-positive-integer?]
                  [maxDepth exact-positive-integer?])
                   gen?]{
                   
   Creates a PEG generator that generates a PEG grammar, an start expressio and
   the type context for each variable in grammar.
   
   @itemlist[@item{maxVars : The maximim number of variables to be present in the grammamr}
             @item{maxVars : The maximim number of lierals to be present in the grammamr}
             @item{maxDepth : The maximum higth of a PEG AST (all leafs have higth 0)}
             ]
}

@defproc[(gen:grm  [G (list/c symbol? peg G)] 
                   [Γ (list/c symbol? boolean? (listof symbol?))] 
                   [Δ (hash/c symbol? (listof symbol?))] 
                   [Σ (listof natural?)] 
                   [n exact-positive-integer?] 
                   [pmax exact-positive-integer?])
                   gen?]{
                   
   Construc a generator for a grammar from a context Γ. The Δ is a hashtable that maps
   each variable to its respective forbidem variables list. The G parameter is accumulative and will containg the generated grammar at the end. the Σ is the alphabet.The n parameter is from wich variable the generation should start and 
   pmax is the depth. 
  
}

@defproc[(gen:expr [Γ (list/c symbol? boolean? (listof symbol?))] 
                   [Δ (listof symbol?)] 
                   [Σ (listof natural?)] 
                   [b boolean?] 
                   [p exact-positive-integer?])
                   gen?]{
   @itemlist[@item{ Γ : is a a list of terns : @italic{v} is a varibale name;
                    @italic{nullable} is value that determines whether or not that varibale is nullable; italic{headset} is a list of possible varibales starting the body of v )}
             @item{Δ : is a list of constraints - forbiden variables}
             @item{Σ : is an alphabet}
             @item{b : Should be #t whenever the generated expression must be nullable.}
             @item{b : Should be #t whenever the generated expression must be nullable.}
             @item{ p : Depth of the expression. If p is 0, only generate terminals or
                    allowed varibales. Samples @racket[n] values from @racket[g].}]
   
  
}


@defproc[(gen:var  [varSize exact-positive-integer?])
                   gen?]{
   Creates a generator for a variable (as an string) the varSize is the maximum length of
   the string.
}

@defproc[(gen:symbolVar  [varSize exact-positive-integer?])
                   gen?]{
   Creates a generator for a variable (as an symbol) the varSize is the maximum length of
   the symbol.
}


 



