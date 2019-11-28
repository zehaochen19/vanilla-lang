# SystemF

Brilliant PL people implement their own dependently typed languages.
I can only implement a System F.

For simplicity, this programming language only supports type checking and evaluation on closed terms.

## (Planned) Features

- [x] Static semantic
- [x] Higher-rank polymorphism
- [x] Bidirectional type inference
- [x] Dynamic semantic
- [x] Examples
- [x] Unit tests
- [x] Let Binding (not verified)
- [ ] Parser
- [ ] Extended basic types (Nat, Boolean, List, ...)
- [ ] If-Else clause
- [ ] Fixpoint for recursive functions

## References

- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/abs/1306.6032)

  The original paper that describes algorithmic bidrectional typing for higher-rank polymorphism

- [Minimal Haskell implementation of Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://gist.github.com/lexi-lambda/287dc8513f6a20424457b9d3eda5026a)

- [Let Arguments Go First](https://link.springer.com/chapter/10.1007/978-3-319-89884-1_10)
