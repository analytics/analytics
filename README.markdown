# analytics

[![Build Status](https://secure.travis-ci.org/analytics/analytics.png?branch=master)](http://travis-ci.org/analytics/analytics)

This is a work-in progress project for working with declarative Datalog style programs as an embedded domain-specific language in Haskell.

The current focus is on getting a rich Datalog-esque EDSL that works nicely as a monad transformer, so you can mix and match it with
tools from other domains.

Skim the [examples folder](https://github.com/analytics/analytics/tree/master/examples) to get started for now.

Using `X`, `Y` and `Z` as `Node` variables, and `A` `B` and `C` as `Node` entities:

```haskell
test :: Datalog [TC]
test = do
  edge A B
  edge B C
  edge B A
  tc X Y :- edge X Y
  tc X Z :- tc X Y <* edge Y Z
  query $ tc A X <* no (edge X C)
```

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell-lens IRC channel on irc.freenode.net.

-Edward Kmett
