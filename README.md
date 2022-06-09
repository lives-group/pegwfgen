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


# Tests

This library has its own raccheck tests that are executed by the raco test command line. 
There is some special test-cases designed to test the geenration process more rigorously. Those teste are more computational intensive and can take many hours to complete. 





