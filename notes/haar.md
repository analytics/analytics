* http://www.research.ibm.com/people/m/minwang/publications/sigmod98.pdf

We can use Haar wavelets to approximate result set sizes:

We can now pay games with the haar wavelet approximation of our generalized-Morton-order dataset by only retaining the most significant features of the weighted Haar basis.

This basically comes down to figuring out what part of what bits we care about the most.

We can then set the remaining features to 0.

This gives us an M coefficient approximation to our dataset, with breaks wherever we're going to get the most bang for our buck.

Possible error measures:

```haskell
e_abs expected actual = abs (expected - actual)

e_rel expected actual = e_abs expected actual / fromIntegral actual

e_comb alpha beta expected actual
  | actual == 0 = alpha * e_abs expected actual
  | otherwise = min (alpha * e_abs) (beta * e_rel)
```

We can calculate the 1-norm, 2-norm or infinity-norm error, and we can weight based on depth in the tree or even by the depth in
their respective key spaces/priority.

Can we take two Haar sketches and calculate a sketch of the join?

Also can we calculate approximate aggregations over our data using wavelets of other data than just pop count, like total.

Should we consider other wavelets than Haar/D2 for other data types?

Storage:

We can easiy store these exactly by storing the sum rather than the average (weighting it as if we stored the latter)

```
1000 => 10 => 12 => 3 => 7 | 1 | 1 4 | -1 2 2 2 | -1 0 0 0 0 0 0 0
0011    02    04    4
0011    02
0011    02
```

We can use this for result set size and for sums.

Can we use max + delta to answer some queries in something like the Viterbi dioid?

```
1000 => 10 => 18 => 8 => 9 | 1 | 7 9 |  -1 8 0 1 | 1 0 0 4 0 0 4 7
0048    08    09    9
0037    07
0029    09
```

We can calculate both of these as part of the sketch, when we rebuild a leaf.

Just pass those sketches upstream to maintain the upstream sketches. Since we're assembling the Haar wavelet approximation
in order, we can construct an exact sketch.
