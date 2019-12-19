# Vinilla Lang

Vanilla is a pure functional programming language based on System F, a classic but powerful type system.

It supports:

- Higher-order polymorphism
- Type inference (Only polymorphic bindings need annotations)
- Algebraic data types
- Pattern matching

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

Haskell-style comments are also supported.

TODO : update grammer of data types here

## Usage

First of all, `stack` should be installed in `PATH`

### Map for Lists

Given `example/map.vn`:

```
data List a = Nil | Cons a (List a).


let rec map : ∀a. ∀b. (a → b) → (List a) → (List b) =
          λ f.
          λ xs. case xs of {
      Nil → Nil,
      Cons y ys → Cons (f y) (map f ys)
    }
in

let not : Bool → Bool =
  λb. if b then False else True
in

let xs = Cons True (Cons False Nil) in

map not xs
```

Run

```
$ stack run example/map.vn
```

It should output:

```
Type:
List Bool

Result:
Cons False Cons True Nil
```

### Add operator for natural numbers

Given `example/add.vn`:

```
-- add can be defined by `fixpoint`
let add : Nat → Nat → Nat =
  fix (λf. λx : Nat . λy : Nat. natcase x {0 → y, S a → S (f a y)})
in

-- or `let rec`
let rec add2 : Nat → Nat → Nat =
  λ x . λ y . natcase x {0 → y, S a → S (add2 a y)}
in

-- 3 + 2 = 5
(add (S (S (S 0))) (S S (0)), add2 (S (S (S 0))) (S S (0)))
```

Run

```
$ stack run example/add.vn
```

It should output the inferred type and evaluated value of this program:

```
Type:
(Nat, Nat)

Result:
((S (S (S (S (S 0))))), (S (S (S (S (S 0))))))
```

### Mutual Recursion

Mutual recursive functions can be easily defined with the fixpoint and projections.

Given `example/evenodd.vn`:

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

Given `example/illtypedid.vn`:

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

`$ stack test`

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
- [x] Algebraic data types
  - [x] Declarations
  - [x] Pattern match
- [x] Type application
- [x] If-Else clause
- [x] Fixpoint for general recursion
- [x] Parser
- [x] Pretty printing

## References

- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/abs/1306.6032)

- [Minimal Haskell implementation of Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://gist.github.com/lexi-lambda/287dc8513f6a20424457b9d3eda5026a)

- [Let Arguments Go First](https://link.springer.com/chapter/10.1007/978-3-319-89884-1_10)
