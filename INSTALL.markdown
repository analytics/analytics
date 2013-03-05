Installation Instructions
=========================

During `cabal configure`, we'll try to construct the `configure` script, but
to build from source you'll need a fairly recent set of `autotools` for this
to work.

If your build fails, you can try On a Mac you can retrieve them with `brew`
and manually run:

* `aclocal -Im4`
* `autoreconf -i`

Once that is done you can run `cabal`:

* `cabal install`

If you retrieved the package from `hackage`, it should have come with the
`autoconf` components already built.

Still having issues?
====================

Please feel free to contact me through [github](https://github.com/ekmett)
or on the `#haskell-lens` IRC channel on `irc.freenode.net` if you are
having a problem.

-Edward Kmett
