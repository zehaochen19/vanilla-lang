# Vinilla Lang

Vanilla is a pure functional programming language based on System F, a classic but powerful type system.

## Merits

Simple as it is, Vanilla contains many features that most main-stream languages don't have:

- Higher-rank polymorphism
  - It allows using polymorphic functions as arguments of higher-order functions
- Strong type inference
  - Only polymorphic and recursive bindings need annotations
- Algebraic data types
- Simplicity
  - Only foralls`∀` and arrows`→` are built-in types
  - All regular types such as `Unit` and `Bool` can be declared as ADT and eliminated by `case` expression

## Defects

- No module system
- For simplicity, this programming language only supports type checking and evaluation on closed terms.

## Grammar

```
Types           A, B, C   ::= T A1...An | a | ∀a.A | A → B
Monotypes       τ, σ      ::= T τ1...τn | a | τ → σ

Data Type       T         ::= T
Constructor     K         ::= K
Declaration     D         ::= data T a1...at = K1 τ1...τm | ... | Kn σ1...σn .
Pattern         p         ::= K x1...xn

Expressions     e, f      ::=   x                                     -- variable
                              | K                                     -- constructor
                              | case e of {pi → ei}                   -- case
                              | λx.e                                  -- implicit λ
                              | λx : A.e                              -- annotated λ
                              | e1 e2                                 -- application
                              | e : A                                 -- annotation
                              | let x = e1 in e2                      -- let binding
                              | let x : A = e1 in e2                  -- annotated let binding
                              | let rec f : A = e1 in e2              -- recursive binding (sugar)
                              | fix e                                 -- fixpoint
                              | e @ A                                 -- type application

Program         P         ::= D P | e
```

A valid Vanilla program consists of several ADT declarations followed by a main expression.

ADT declarations are similar to ones in Haskell, except that they end with a dot for easy parsing.

Haskell-style comments (`--` and `{- -}`) are also supported.

## Usage

First of all, `stack` should be installed in your `$PATH`

## Examples

### Higher-rank Polymorphism

Given `example/cont.vn`:

```
data Unit = Unit.

-- Can you belive that
-- type `a` and `∀r. ((a → r) → r)` are isomorphic?

let cont : ∀a. a → ∀r. ((a → r) → r) =
  λ x . λ callback . callback x
in

let runCont : ∀a. (∀r. (a → r) → r) → a =
  λ f . (let callback = λ x . x in f callback)
in

-- should output Unit
runCont (cont Unit)
```

Run

```
stack run example/cont.vn
```

It should output:

```
Type:
Unit

Result:
Unit
```

### Map for Lists

Given `example/map.vn`:

```
data Bool = False | True.
data List a = Nil | Cons a (List a).


let rec map : ∀a. ∀b. (a → b) → (List a) → (List b) =
  λ f. λ xs. case xs of {
      Nil → Nil,
      Cons y ys → Cons (f y) (map f ys)
    }
in

let not : Bool → Bool =
  λb. case b of {False → True, True → False}
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
Cons False (Cons True Nil)
```

### Add operator for Natural Numbers

Given `example/add.vn`:

```
data Nat = Zero | Succ Nat.
data Prod a b = Prod a b.

-- add can be defined by `fixpoint`
let add : Nat → Nat → Nat =
  fix (λf. λx : Nat . λy : Nat. case x of {Zero → y, Succ a → Succ (f a y)})
in

-- or `let rec`
let rec add2 : Nat → Nat → Nat =
  λ x . λ y . case x of {Zero → y, Succ a → Succ (add2 a y)}
in

let two = Succ (Succ Zero) in
let three = Succ two in

-- 3 + 2 = 5
Prod (add three two) (add2 three two)
```

Run

```
$ stack run example/add.vn
```

It should output the inferred type and evaluated value of this program:

```
Type:
Prod Nat Nat

Result:
Prod (Succ (Succ (Succ (Succ (Succ Zero))))) (Succ (Succ (Succ (Succ (Succ Zero)))))
```

### Mutual Recursion

Mutual recursive functions can be easily defined with the fixpoint and projections.

Given `example/evenodd.vn`:

```
data Nat = Zero | Succ Nat.
data Bool = False | True.
data Prod a b = Prod a b.

let evenodd : Prod (Nat → Bool) (Nat → Bool) =
  fix (λ eo .
    let e = λ n : Nat . case n of { Zero → True, Succ x → (case eo of {Prod e o → o}) x } in
    let o = λ n : Nat . case n of { Zero → False, Succ x → (case eo of {Prod e o → e}) x } in
    (Prod e o))
in

let even = (case evenodd of {Prod e o → e}) in
let odd = (case evenodd of {Prod e o → o}) in

let five = Succ(Succ(Succ(Succ(Succ(Zero))))) in

Prod (even five) (odd five)
```

It reports:

```
Type:
Prod Bool Bool

Result:
Prod False True
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

```
$ stack test
...
Finished in 0.0575 seconds
121 examples, 0 failures

vanilla-lang> Test suite vanilla-test passed
```

## Features

- [x] Static semantic
- [x] Higher-rank polymorphism
- [x] Type inference
- [x] Dynamic semantic
- [x] Both annotated and implicit λ
- [x] Examples
- [x] Unit tests
- [x] Let Binding
- [x] Algebraic data types
  - [x] Declarations
  - [x] Introductions and eliminations
  - [x] Well-formedness checking
- [x] Type application
- [x] Fixpoint for general recursion
- [x] Let rec
- [x] Parser
- [x] Pretty printing

## References

- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/abs/1306.6032)
- [Minimal Haskell implementation of Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://gist.github.com/lexi-lambda/287dc8513f6a20424457b9d3eda5026a)
- [Bidirectional Typing](https://arxiv.org/abs/1908.05839)
- [Kind Inference for Datatypes](https://richarde.dev/papers/2020/kind-inference/kind-inference.pdf)
