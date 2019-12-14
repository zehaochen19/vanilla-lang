# SystemF

Brilliant PL people implement their own dependently typed languages.
I can only implement a System F.

For simplicity, this programming language only supports type checking and evaluation on closed terms.

## Grammar

```
Types           A, B, C   ::= Unit | Bool | Nat | α | ∀α.A | A → B
Monotypes       τ, σ      ::= Unit | Bool | Nat | α | τ → σ

Expressions     e         ::=   x                                     -- variable
                              | ()                                    -- unit
                              | True                                  -- boolean constant true
                              | False                                 -- boolean constant false
                              | 0                                     -- natural number zero
                              | S e                                   -- natural number successor
                              | natcase n {0 → e1, S x → e2}          -- natural number elimination
                              | (e1, e2)                              -- product
                              | e.1                                   -- projection first
                              | e.2                                   -- projection second
                              | Inj1 e                                -- injection1
                              | Inj2 e1                               -- injection2
                              | sumcase e {Inj1 x → e1, Inj2 y → e2}  -- sum elimination
                              | λx.e                                  -- implicit λ
                              | λx : A.e                              -- annotated λ
                              | e1 e2                                 -- application
                              | e : A                                 -- annotation
                              | let x = e1 in e2                      -- let binding
                              | let x : A = e1 in e2                  -- annotated let binding
                              | if e then e1 else e2                  -- if-else
                              | fix e                                 -- fixpoint
```

## Usage

First of all, `cabal` and `ghc` should be installed in `PATH`

### Add operator for natural numbers

Given `example/add.sf`:

```
let add : Nat → Nat → Nat =
  fix (λf. λx : Nat . λy : Nat. natcase x {0 → y, S a → S (f a y)})
in

-- 3 + 2 = 5
add (S (S (S 0))) (S S (0))
```

Run

```
cabal build
cabal run systemf example/add.sf
```

It should output the inferred type and evaluated value of this program:

```
Type:
Nat

Result:
S (S (S (S (S 0))))
```

### Mutual Recursion

Mutual recursive functions can be easily defined with the fixpoint and projections.

Given `example/evenodd.sf`:

```
let evenodd : (Nat → Bool, Nat → Bool) =
  fix (λ eo  .
    let e = λ n . natcase n { 0 → True, S x → eo.2 x } in
    let o = λ n . natcase n { 0 → False, S x → eo.1 x } in
    (e, o))
in

let even = evenodd.1 in
let odd = evenodd.2 in

let five = S(S(S(S(S(0))))) in

(even five, odd five)
```

It reports:

```
Type:
(Bool, Bool)

Result:
(False, True)
```

### Ill-typed Program

Given `example/illtypedid`:

```
let id : Nat → Nat =
  (λx . x)
in
  id ()
```

It reports typechecking error:

```
Typecheck error:
cannot establish subtyping with Unit <: Nat
```

### Unit tests

`cabal test`

## (Planned) Features

- [x] Static semantic
- [x] Higher-rank polymorphism
- [x] Type inference
- [x] Dynamic semantic
- [x] Both annotated and implicit λ
- [x] Examples
- [x] Unit tests
- [x] Let Binding (not verified)
- [x] Extended basic types
  - [x] Bool
  - [x] Nat
  - [x] Product
  - [x] Sum
- [x] If-Else clause
- [x] Fixpoint for general recursion
- [x] Parser
- [x] Pretty printing

Maybe in Future

- [ ] Declarations
- [ ] Pattern match
- [ ] Type operators

## References

- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/abs/1306.6032)

- [Minimal Haskell implementation of Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://gist.github.com/lexi-lambda/287dc8513f6a20424457b9d3eda5026a)

- [Let Arguments Go First](https://link.springer.com/chapter/10.1007/978-3-319-89884-1_10)
