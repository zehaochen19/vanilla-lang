# SystemF

Brilliant PL people implement their own dependently typed languages.
I can only implement a System F.

For simplicity, this programming language only supports type checking and evaluation on closed terms.

## Grammar

```
Types           A, B, C   ::= Unit | Bool | Nat | α | ∀α.A | A → B
Monotypes       τ, σ      ::= Unit | Bool | Nat | α | τ → σ

Expressions     e         ::=   x                       -- variable
                              | ()                      -- unit
                              | True
                              | False
                              | 0
                              | S e                     -- natural number successor
                              | natcase e e1 x e2       -- natcase n {0 → e1, S x → e2}
                              | λx.e                    -- implicit λ
                              | λx : A.e                -- annotated λ
                              | e1 e2
                              | e : A                   -- annotation
                              | let x = e1 in e2
                              | if e then e1 else e2
                              | fix e                   -- fixpoint
```

## Usage

First, `stack` should be install in `PATH`

Given `example/id.sf`:

```
let id =
  λx . x : ∀A. A → A
in
  id ()
```

Run

```
stack install
systemf example/id.sf
```

It should output the inferred type and evaluated value of this program:

```
Type:
TUnit

Result:
EUnit
```

## (Planned) Features

- [x] Static semantic
- [x] Higher-rank polymorphism
- [x] Type inference
- [x] Dynamic semantic
- [x] Both annotated and implicit λ
- [x] Examples
- [x] Unit tests
- [x] Let Binding (not verified)
- [ ] Extended basic types
  - [x] Bool
  - [x] Nat
  - [ ] Product
  - [ ] Sum
  - [ ] List
- [x] If-Else clause
- [x] Fixpoint for general recursion
- [x] Parser
- [ ] Pretty printing

## References

- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/abs/1306.6032)

- [Minimal Haskell implementation of Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://gist.github.com/lexi-lambda/287dc8513f6a20424457b9d3eda5026a)

- [Let Arguments Go First](https://link.springer.com/chapter/10.1007/978-3-319-89884-1_10)
