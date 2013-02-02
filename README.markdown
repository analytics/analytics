# analytics

[![Build Status](https://secure.travis-ci.org/ekmett/analytics.png?branch=master)](http://travis-ci.org/ekmett/analytics)

This is a work-in progress project for working with declarative Datalog style programs as an embedded domain-specific language in Haskell.

The current focus is on getting a rich Datalog-esque EDSL that works nicely as a monad transformer, so you can mix and match it with
tools from other domains.

Skim the [examples folder](https://github.com/ekmett/analytics/tree/master/examples) to get started for now.

_e.g._ assuming the existence of `edge` and `tc` as datalog predicates that take `Node` arguments that can be the entities 
`A`, `B`, `C` or variables, we can state the classic datalog transitive closure problem as:

```haskell
test :: Monad m => Datalog m [TC a]
test = do
  edge A B
  edge B C
  edge B A
  tc x y :- edge x y
  tc x z :- tc x y & edge y z
  query $ tc A x & no (edge x C)
  where x = "x"; y = "y"; z = "z"
```

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
