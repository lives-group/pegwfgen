#lang scribble/manual

(@require[ scribble/example
          peg-gen
          @for-label[racket/base
                     racket/contract
                     racket/string
                     rackcheck
                     rackunit]])

@title{peggen}
@author{Elton M. Cardoso, Rodrigo G. Ribeiro, Leonardo V. S. Reis}

@defmodule[peg-gen]


@(begin
  (define ev (make-base-eval))
  (ev '(require rackcheck racket/list racket/stream rackunit peg-gen))
  (define-syntax-rule (ex body ...)
    (begin
     (random-seed 1337)
     (examples
      #:eval ev
      #:label #f
      body ...))))

@section{Overview}


This module defines a collection of  PEG rackcheck generators. 
To generate well-formed PEGs, each term is always generated as a
tern (peg, nullable, headset). This approach allows for incremental type correct construction of the term. 

When generating a grammar, the context of all possible variables (Γ) is kept and updated accordingly. An additional context called Δ is used to avoid generated calls to non-terminals that would result in left recursion. 

The general use case for this library is the function gen:peg whose
arguments are the maximum number of variables, the maximum number of terminal symbols, and the maximum depth of a peg. 

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
  Creates a generator that generates a list of variable names (as symbols) with no more than maxVars elements. The varSize is the number of letters in the variable (0 means one letter, 1 means two letters, and so on).
}

@defproc[(initΔ  [Γ (list/c symbol? boolean? (listof symbol?))])
                   (hash/c symbol? (listof symbol?))]{
   Constructs an initial correct Δ table from the given Γ.
}


@defproc[(gen:peg [maxVars  exact-positive-integer?] 
                  [maxLits  exact-positive-integer?]
                  [maxDepth exact-positive-integer?])
                   gen?]{
                   
   Creates a PEG generator that generates a PEG grammar, a start expression, and the type context for each variable in grammar.
   
   @itemlist[@item{maxVars : The maximum number of variables to be present in the grammamr}
             @item{maxVars : The maximum number of literals to be present in the grammamr}
             @item{maxDepth : The maximum height of a PEG AST (all leaves have high 0)}]
}

@defproc[(gen:grm  [G (list/c symbol? peg G)] 
                   [Γ (list/c symbol? boolean? (listof symbol?))] 
                   [Δ (hash/c symbol? (listof symbol?))] 
                   [Σ (listof natural?)] 
                   [n exact-positive-integer?] 
                   [pmax exact-positive-integer?])
                   gen?]{
                   
     Construc a generator for grammar from a context Γ. The Δ is a hashtable that maps each variable to its respective forbidden variables list. The G parameter is accumulative and will contain the generated grammar at the end. the Σ is the alphabet. The n parameter is from which variable the generation should start and pmax is the depth.  
  
}

@defproc[(gen:expr [Γ (list/c symbol? boolean? (listof symbol?))] 
                   [Δ (listof symbol?)] 
                   [Σ (listof natural?)] 
                   [b boolean?] 
                   [p exact-positive-integer?])
                   gen?]{
   @itemlist[@item{Γ: is a list of terns: @italic{v} is a variable name;  @italic{nullable} is a value that determines whether or not that variable is nullable; italic{headset} is a list of possible variables starting the body of v.}
             @item{Δ: is a list of constraints - forbidden variables}
             @item{Σ: is an alphabet}
             @item{b: Should be #t whenever the generated expression must be nullable.}
             @item{p: Depth of the expression. If p is 0, only generate terminals or allowed variables. Samples @racket[n] values from @racket[g].}]
}


@defproc[(gen:var  [varSize exact-positive-integer?])
                   gen?]{
      Creates a generator for a variable (as a string) the varSize is the maximum length of the string.
}

@defproc[(gen:symbolVar  [varSize exact-positive-integer?])
                   gen?]{
  Creates a generator for a variable (as a symbol) the varSize is the maximum length of the symbol.
}


 



