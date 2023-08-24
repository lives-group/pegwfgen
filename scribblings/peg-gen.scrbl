#lang scribble/manual

(@require[ scribble/example
          peg-gen
          peg-gen/peg-gen-syntax
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

@defproc[(gen:peg-s [maxVars  exact-positive-integer?] 
                    [maxLits  exact-positive-integer?]
                    [maxDepth exact-positive-integer?]
                    [b boolean?])
                    gen?]{
                   
   Creates a PEG generator that generates a PEG grammar, a start expression that is nullable if b is true, and the type context for each variable in grammar.
   
   @itemlist[@item{maxVars : The maximum number of variables to be present in the grammamr}
             @item{maxVars : The maximum number of literals to be present in the grammamr}
             @item{maxDepth : The maximum height of a PEG AST (all leaves have high 0)}
             @item{b: The start expression must be nullable ?}]
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


@section{ Changing the output structure of the generator}

In some cases, it may be helpful to have the generator produce an
output format more suitable for other applications, such as creating
a proper source code or testing a specific library. 

The generator uses a structure (referred to as a factory) that contains
the constructors for all the parsing expressions and the grammar, and this
structure can be set with @racket[setSynFactory].
The module @racket[peg-gen-syntax] contains an alternative
structure called @racket[PEGStructF]. 

@ex[ (require peg-gen
              peg-gen/peg-gen-syntax)
     (setSynFactory PEGStructF)
     (sample (gen:peg 3 2 1) 3)]

The structure is defined as follows:
@racketblock[
(struct PEGFSyn
        ( mkEps
          mkLit
          mkVar
          mkNot
          mkKle
          mkSeq
          mkAlt
          addRule
          mkEmptyGrm
          mkPEG
        )
)
]

where:
@itemlist[
   @item{mkEps : A function with no parameter that builds an empty PEG symbol.}
   @item{mkLit : A function that takes one parameter (from Σ) and builds a terminal symbol.}
   @item{mkVar : A function that takes one parameter (a racket symbol from the set of non-terminals)
                   and builds a no-terminal (or a variable) symbol.} 
  @item{mkSeq and mkAlt : Takes two parameters and builds a sequence and an alternative constructions
                           respectively.}
  @item{mkNot and mkKle : Takes one parameter and builds a not predicate and a repetition construction
                           , respectively.}
  @item{addRule : Takes three parameters: A representation of the Grammar (G), a representation of non-terminal
                   name (NT) and a parsing expression (E) and produces a new grammar adding the rule NT <- E to
                   it.}
  @item{mkEmptyGrm : Takes no parameters and returns a representation of the empty grammar.}
  @item{mKPEG : Takes a representation of grammar, one parsing expression (as the start parsing expression)
  and a type context (as a list of pairs) and builds a structure for the PEG.}
]

As an example consider the follwing definition for the default factory used by peg-gen: 

@racketblock[
(define defaultFactory
     (PEGFSyn
      (lambda () 'ϵ)            ;mkEps
      (lambda (x) x)             ;mkLit
      (lambda (x) x)             ;mkVar
      (lambda (e) (list '! e  )) ;mkNot
      (lambda (e) (list '* e  )) ;mkKle
      (lambda (x y) (list '• x y)  )    ;mkSeq
      (lambda (x y) (list '/ x y)  )    ;mkAlt
      (lambda (g nt e) (list nt e g) )  ;addRule
      (lambda () '∅ )                                ;mkEmptyGrm
      (lambda (g start gamma) (list g start gamma) )  ;mkPEG
     )
 )
]

Note that in this syntax, we represent a PEG as a recursive tuple of
a Non-terminal, its right-hand side followed by the rest of the grammar.
The symbol ∅ represents the empty grammar. Notice that the constructors
only expect to build PEGs without types. The generation algorithm produces
the types. The type context is given to the constructor mkPEG at the end
of the generation process.

The peg-gen-syntax module contains the definition of an alternative structure
that can be used as the output of the generator: 
@racketblock[
(struct GPEG (nt start gamma)  #:transparent)
(struct GEps ()                #:transparent)
(struct GLit (chr)        #:transparent)
(struct GVar (var)        #:transparent)
(struct GAlt (left right) #:transparent)
(struct GSeq (left right) #:transparent)
(struct GNot (exp)        #:transparent)
(struct GKle (exp)        #:transparent)
]

As an second example, here is the definition of the factory for that structure:
@racketblock[
(define PEGStructF
    (PEGFSyn
      (lambda ()  (GEps) )         ;mkEps
      (lambda (x) (GLit x ))       ;mkLit
      (lambda (x) (GVar x ))       ;mkVar
      (lambda (e) (GNot e ))       ;mkNot
      (lambda (e) (GKle e ))       ;mkKle
      (lambda (x y) (GSeq x y)  ) ;mkSeq
      (lambda (x y) (GAlt x y)  ) ;mkAlt
      (lambda (g nt e) (hash-set g nt e)  ) ;addRule
      (lambda () (make-immutable-hash) )           ;mkEmptyGrm
      (lambda (g start gamma) (GPEG g start gamma) )  ;mkPEG
     )
  )

]

The module that defines this data type also define some pretty print functions:

@defproc[( peg->string  [GPEG GPEG?])
                   String?]{
      Converts a PEG into a string. 
}


@defproc[(pexp->string  [exp any?])
                   String?]{
      Converts a parse expression into a string. 
}
    

@section{Generating Ill-typed PEGs}

As an experimental feature, we add the two new generators
that generate PEGs that are ill-typed (not well-formed according to Ford's definition).


@defproc[(gen:ill-peg [maxVars  exact-positive-integer?] 
                  [maxLits  exact-positive-integer?]
                  [maxDepth exact-positive-integer?])
                   gen?]{
                   
It has the same parameters as gen:peg but generates an ill-typed PEG instead.
The gamma context this generator returns will have explicit annotations on the
intended ill-typed non-terminals. However, note that the start expression does
not have a type annotation. 
  
}

@defproc[(gen:ill-peg-s [maxVars  exact-positive-integer?] 
                    [maxLits  exact-positive-integer?]
                    [maxDepth exact-positive-integer?]
                    [b boolean?])
                    gen?]{

It has the same parameters as gen:peg-s but generates an ill-typed PEG instead.
The gamma context this generator returns will have explicit annotations on the
intended ill-typed non-terminals. However, note that the start expression does
not have a type annotation. 
}

