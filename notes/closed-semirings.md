# Closed Semirings

* We can close any semiring (that may have a partial definition for Kleene *) by adding a ⊥ element and defining

```haskell
⊥ +' a = a +' ⊥ = ⊥
⊥ *' a = a *' ⊥ = ⊥
a *' | a* is defined = a*
     | otherwise = ⊥
```

* Tropical semirings work
* Matrices over a closed semiring form a closed semiring
* The Boolean semiring is closed. This lets us reason about lattices.
* Regular expressions
* The Kleene star generalizes `(1/1-x)`. e.g. `(R,+,*,0,1)` can be completed with

```haskell
  a* | a = (1,⊥) = ⊥
     | otherwise = 1/(1-a)
```
