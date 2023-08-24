peg-gen
==============
This library aims to provide a well typed (and thus well formed) PEG generator. 

 

![example workflow](https://github.com/lives-group/pegwfgen/actions/workflows/main.yml/badge.svg)
![example workflow](https://github.com/lives-group/pegwfgen/actions/workflows/test.yml/badge.svg)



# Installing

The easyest way to install this library is trough the raco pkg command. 

```racket
   raco pkg install peg-gen
```
You can also use DrRacket library manager to install this library. 

# Main usage

The function gen:peg generates a PEG grammar together with the initial expression and the type environment. It takes the maximum number of variables, literal symbols and depth of expression as arguments.  This library depends on  [rackcheck](https://docs.racket-lang.org/rackcheck/index.html) and it assumed that you have some familiarity with it. 

As an example lets use rackcheck function `sample` to generate one simple PEG grammar.

```racket
> (sample (gen:peg 2 2 3) 4)
 '((∅ (* 1) ())
  (∅ (/ 0 0) ())
  ((X (* (• 0 1)) (G (! (/ 0 1)) ∅)) (• (• 0 X) (• X ε)) ((G #t ()) (X #t ())))
  ((F ε (H 1 ∅)) ε ((H #f ()) (F #t ()))))
```

Here 4 grammars were genertated by the `sample` function.  The expression `(gen:peg 2 2 4)` is our generator parametrized to genreate PEGs with a maximum of 2 varibales, 2 literals and depth 3. The result is a list of  terns  `(G e Γ)`  where `G` is a  grammar  `e` is a simple PEG expression and `Γ` is the type evironment .


### PEG syntax

* Symbol `ε` represents  the empty string
* Symbol `•` represents the binary operator for concatenation
* Symbol `/` represents the binary operator for choice
* Symbol `!` represents the unary negation operator.
* Symbol `*` represents the unary repetition operator. 

For reasons of simplicity we use naturals for representing terminal symbos. 

### PEG Grammar

A grammar is a recursive structure with the following syntax: 

* `∅` :  The empty grammar.
* `(V e G)` : Where `V` is a nonterminal (a varibale)  `e` is a PEG expression and  `G` is a grammar structure.

### Type environment

The type environment is a list of triples where the first element is a nonterminal, the
second element is a boolean which indicates the nullability of the nonterminal and
the third element is a list of all nonterminals that firstly appears on the left side of
the expression associated wiht the non-terminal (head set). 


### Customizing the output

The end user now can provide an structure (called a factory) that defines how the PEG should be constructed. As example of this 
the module peg-gen-syntax.rkt contains a racket structure and its corresponding factory called PEGStructF to build PEGs.  
This example can be set by requiring the mentioned module and then overriding the generator default factory using the .

( **NOTE:** The module  peg-gen-syntax.rkt is already imported by the peg-gen module. )

```racket
> (require peg-gen/peg-gen-syntax)
> (setSynFactory PEGStructF)
> (sample (gen:peg 2 2 2) 3)
(list
 (PEG '#hash() (Not (Seq (Lit 0) (Eps))) '())
 (PEG '#hash() (Alt (Seq (Eps) (Lit 1)) (Not (Eps))) '())
 (PEG '#hash() (Kle (Lit 0)) '()))
```

To define a custom output, the user can require the module peg-gen/peg-gen-syntax-factory
and fill PEGFSyn structure with the appropriated constructors. Consider the definition of
the previous used output structure as an example: 

```racket
(require "peg-gen-syntax-factory.rkt")

(struct GPEG (nt start gamma)  #:transparent)
(struct GEps ()                #:transparent)
(struct GLit (chr)        #:transparent)
(struct GVar (var)        #:transparent)
(struct GAlt (left right) #:transparent)
(struct GSeq (left right) #:transparent)
(struct GNot (exp)        #:transparent)
(struct GKle (exp)        #:transparent)

(define (add-nt p nt exp)
     (GPEG (hash-set (GPEG-nt p) nt exp) (GPEG-start p) (GPEG-gamma p))
  )

(define PEGStructF
    (PEGFSyn
      (lambda ()  (GEps) )         ;mkEps
      (lambda (x) (GLit x ))       ;mkLit
      (lambda (x) (GVar x ))       ;mkVar
      (lambda (e) (GNot e ))       ;mkNot
      (lambda (e) (GKle e ))       ;mkKle
      (lambda (x y) (GSeq x y)  ) ;mkSeq
      (lambda (x y) (GAlt x y)  ) ;mkAlt
      (lambda (g nt e) (hash-set g nt e)  )          ;addRule
      (lambda () (make-immutable-hash) )             ;mkEmptyGrm
      (lambda (g start gamma) (GPEG g start gamma) ) ;mkPEG
     )
  )

 ```

### Generating ill-typed PEGs (Experimental)

This version contains tow new main functions to generate ill-typed PEGs (PEGs that may loop)
Some non-terminals may be intentionally made ill-type, those have their respective types 
define as the symbol 'ill-typed instead of having an actual type. 
 
```racket
> (sample (gen:ill-peg 2 2 2) 3)
(list
 (PEG '#hash() (Seq (Kle (Eps)) (Lit 2)) '())
 (PEG (hash 'X (Lit 1)) (Kle (Eps)) (list (cons 'X (TyPEG #f '()))))
 (PEG
  (hash 'C (Seq (Lit 0) (Var 'T)) 'T (Var 'T))
  (Seq (Eps) (Lit 1))
  (list '(T . ill-typed) (cons 'C (TyPEG #f '())))))
```
The two first PEGs are ill-typed because of (Kle (Eps)). The last the Non-terminal T is left-recursive. 

# Tests

This library has its own raccheck tests that are executed by the raco test command line. 
There is some special test-cases designed to test the geenration process more rigorously. Those teste are more computational intensive and can take many hours to complete. 





