# Closed Semirings

A closed semiring `(S,+,∘,*,0,1)` is a semiring `(S,+,∘,0,1)` extended with `a*` such that `a* = 1 + a∘a* = 1 + a*∘a`

This is important in our environment because we want to look at a datalog where we can use non-stratified
aggregation through closed semirings and closed-semiring homomorphisms and (ideally) lazily detect the
provenance information needed to break cycles.

* We can close any semiring (that may have a partial definition for Kleene *) by adding a ⊥ element and defining

```haskell
⊥ +' a = a +' ⊥ = ⊥
⊥ *' a = a *' ⊥ = ⊥
a *' | a* is defined = a*
     | otherwise = ⊥
```

* Tropical semirings are closed.
* Matrices over closed semirings form a closed semiring
* Closed semiring products form a closed semiring
* The Boolean semiring is closed. This lets us reason about lattices and generalized annotated programs as a subset.
* Regular languages form a closed semiring.
* The Kleene star generalizes `(1/1-x)`. e.g. `(R,+,*,0,1)` can be completed with

```haskell
  a* | a = (1,⊥) = ⊥
     | otherwise = 1/(1-a)
```

# ω-complete and ω-continuous semirings

Closed semirings have infinite sums but their "+" is idempotent which rules out provenance semantics.

Let `(K,+,∘,0,1)` be a semiring. Let `a <= b = ∃x. a + x = b`. If (<=) forms a partial order then `K` is "naturally ordered".

K is an ω-complete semiring if it is naturally ordered and <= is such that ω-chains `x_0 <= x_1 <= ... <= x_n <= ...` have least upper bounds.

In such semirings we can define countable sums.

An ω-continuous semiring is an ω-complete semiring such that + and ∘ are ω-complete in each argument.

Consequently Countable sums are associative and commutative and ∘ distributes over countable sums and countable sums are monotone in each addend.

* The boolean semiring is ω-continuous.
* The tropical semiring over N is ω-continuous.
* The fuzzy semiring `([0,1], max, min, 0, 1)` is ω-continuous.
* Sigma algebras
* Products of ω-continuous semirings are ω-continuous
