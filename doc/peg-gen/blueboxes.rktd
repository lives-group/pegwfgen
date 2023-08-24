641
((3) 0 () 1 ((q lib "peg-gen/main.rkt")) () (h ! (equal) ((c def c (c (? . 0) q gen:Γ)) q (0 . 4)) ((c def c (c (? . 0) q gen:grm)) q (651 . 8)) ((c def c (c (? . 0) q pexp->string)) q (1393 . 3)) ((c def c (c (? . 0) q gen:var)) q (1167 . 3)) ((c def c (c (? . 0) q gen:symbolVar)) q (1246 . 3)) ((c def c (c (? . 0) q gen:ill-peg-s)) q (1636 . 6)) ((c def c (c (? . 0) q gen:ill-peg)) q (1453 . 5)) ((c def c (c (? . 0) q gen:peg-s)) q (448 . 6)) ((c def c (c (? . 0) q gen:peg)) q (269 . 5)) ((c def c (c (? . 0) q initΔ)) q (146 . 3)) ((c def c (c (? . 0) q gen:expr)) q (948 . 7)) ((c def c (c (? . 0) q peg->string)) q (1331 . 3))))
procedure
(gen:Γ maxVars [#:varSize varSize]) -> gen?
  maxVars : exact-positive-integer?
  varSize : exact-positive-integer? = 0
procedure
(initΔ Γ) -> (hash/c symbol? (listof symbol?))
  Γ : (list/c symbol? boolean? (listof symbol?))
procedure
(gen:peg maxVars maxLits maxDepth) -> gen?
  maxVars : exact-positive-integer?
  maxLits : exact-positive-integer?
  maxDepth : exact-positive-integer?
procedure
(gen:peg-s maxVars maxLits maxDepth b) -> gen?
  maxVars : exact-positive-integer?
  maxLits : exact-positive-integer?
  maxDepth : exact-positive-integer?
  b : boolean?
procedure
(gen:grm G Γ Δ Σ n pmax) -> gen?
  G : (list/c symbol? peg G)
  Γ : (list/c symbol? boolean? (listof symbol?))
  Δ : (hash/c symbol? (listof symbol?))
  Σ : (listof natural?)
  n : exact-positive-integer?
  pmax : exact-positive-integer?
procedure
(gen:expr Γ Δ Σ b p) -> gen?
  Γ : (list/c symbol? boolean? (listof symbol?))
  Δ : (listof symbol?)
  Σ : (listof natural?)
  b : boolean?
  p : exact-positive-integer?
procedure
(gen:var varSize) -> gen?
  varSize : exact-positive-integer?
procedure
(gen:symbolVar varSize) -> gen?
  varSize : exact-positive-integer?
procedure
(peg->string GPEG) -> String?
  GPEG : GPEG?
procedure
(pexp->string exp) -> String?
  exp : any?
procedure
(gen:ill-peg maxVars maxLits maxDepth) -> gen?
  maxVars : exact-positive-integer?
  maxLits : exact-positive-integer?
  maxDepth : exact-positive-integer?
procedure
(gen:ill-peg-s maxVars maxLits maxDepth b) -> gen?
  maxVars : exact-positive-integer?
  maxLits : exact-positive-integer?
  maxDepth : exact-positive-integer?
  b : boolean?
