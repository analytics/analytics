Lambert's W arises when we contemplate ideal bucket sizes
---------------------------------------------------------

Lets say we wanted to write an 'almost sort' that sorted data down into buckets
and then said "meh, good enough", and left the remainder unsorted, then our asymptotic
insert and search time would be driven by the size of the buckets in two ways: both the
number of buckets `N/f(N)` and the size of the buckets `f(N)`.

A search would then consist of searching for the bucket in `log (N/f(N))` time and then linearly
scanning in `f(N)` time.

Balancing these costs leads us to want to cut out data up into buckets of size `W(x)`, so we can find the bucket in time
`log (N/W(N))` and search inside it in time `W(N)`. Since `log (N/W(N)) = W(N)` this gives
search in `O(W(N))`.

This does give guidance that we want the buckets to be scaled slightly smaller than `log`.

Using `W` is different than just using `log` directly at least at the scales we care about. e.g. for 4 billion entries:

```haskell
logBase 2 (2^32) = 32
W (2^32)/log 2 = 27.2327...
```
