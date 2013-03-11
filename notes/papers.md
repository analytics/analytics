# UB-Trees

* [The Universal B-Tree for multidimensional indexing: General Concepts](http://citeseerx.ist.psu.edu/showciting?cid=13384)
* [The BUB-Tree (dealing with dead space)](http://www.cse.ust.hk/vldb2002/VLDB2002-proceedings/papers/S34P16.pdf) by Robert Fenk
* [Processing Relational OLAP Queries with UB-Trees and Multidimensional Hierarchical Clustering (2000)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.35.5820) by Markl and Bayer
* UB-Tree Indexing for Semantic Query Optimization of Range Queries by S. Housseno, A. Simonet and M. Simonet
* [Indexing Techniques in Data Warehousing Environment The UB-Tree Algorithm](http://www.aui.ma/personal/~H.Haddouti/UB_Tree_paper.pdf)

# Morton Ordering
* [Skjellum provides a nice way to make a Morton order that works even for non-integral numbers of bits](http://people.cs.vt.edu/~asandu/Public/Qual2005/Q2005_skjellum.pdf)
* [Adaptive query processing in point-transformation schemes](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.6190&rep=rep1&type=pdf) by B. Yu covers a way we can incrementally refine compution of overlapping ranges in Morton order.

# Hilbert Ordering
* [Efficient Range Query Using Multiple Hilbert Curves](http://cdn.intechopen.com/pdfs/16532/InTech-Efficient_range_query_using_multiple_hilbert_curves.pdf) shows that we can answer queries in fewer ranges if we store multiple copies of the data using different space filling curves or with rotations and shifts. This may point to a replication strategy. As with storing multiple copies for a column store using different orderings to get both fault tolerance, the same thing apply to space filling storage of the data.

# Peano Ordering
* [Cache Oblivious Dense and Sparse Matrix Multiplication Based on Peano Curves](https://para08.idi.ntnu.no/docs/submission_155.pdf)

# Order Preserving Codes
* The identity transformation
* [Shor's explanation of the Hu-Tucker algorithm](http://www-math.mit.edu/~shor/PAM/hu-tucker_algorithm.html)
* [Golomb coding](http://en.wikipedia.org/wiki/Golomb_coding)
* [Arithmetic coding](http://en.wikipedia.org/wiki/Arithmetic_coding)
* [Range coding](http://en.wikipedia.org/wiki/Range_encoding)
* [Order Preserving Key Compression](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.86.3306&rep=rep1&type=pdf) describes ALM coding and references the ZIL coding
* Sometimes we don't care about ordering. If we have categorical information, we could use Huffman directly on that dimension.
* [Dictionary-based Order-preserving String Compression
for Main Memory Column Stores](http://www.cs.uni-paderborn.de/fileadmin/Informatik/AG-Boettcher/Lehre/WS_09_10/pro-sem-ws09/Dictionary-based_Order-preserving_String_Compression_for_Main_Memory_Column_Stores.pdf)
* [Query Optimization In Compressed Database Systems](http://userpages.umbc.edu/~zhchen/papers/sigmod01-camera.pdf)

# Incremental Computation
* [Differential Dataflow](http://www.cidrdb.org/cidr2013/Papers/CIDR13_Paper111.pdf)
* [Project Naiad](http://research.microsoft.com/en-us/projects/naiad/)

# Weighted Datalog and Generalized Annotated Programs
* [Dyna: Extending Datalog for Modern AI](http://www.cs.jhu.edu/~nwf/datalog20-paper.pdf)
* [A Flexible Solver for Finite Arithmetic Circuits](http://cs.jhu.edu/~jason/papers/filardo+eisner.iclp12.pdf)

# Stable Models
* [Extending and Implementing the Stable Model Semantics](http://arxiv.org/pdf/cs/0005010.pdf)
* [Safe Database Queries with Arithmetic Operations](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.48.4845&rep=rep1&type=pdf)

# Top-Down Evaluation
* [Efficient Top-Down Computation of Queries Under the Well-Founded Semantics](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.38.6142&rep=rep1&type=pdf)

# Tabling
* [OLD Resolution with Tabulation](http://sato-www.cs.titech.ac.jp/reference/Sato-ICLP86.pdf)
* [A Thread In Time Saves Tabling Time](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.54.9454&rep=rep1&type=pdf)
* [More Efficient Datalog Queries: Subsumptive Tabling Beats Magic Sets](http://www.logicblox.com/publications/2011/sigmod11-tekle.pdf)

# *-Semirings and C-Semirings/ω-continuous semirings
```
[21:27] nwf:	 Paper dump:
[21:27] nwf:	 http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.71.7650 is a decent overview, though a little dated
[21:28] nwf:	 http://dl.acm.org/citation.cfm?id=973230 ties a lot of this in to NLP parsing, if you're curious
[21:28] nwf:	 http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.56.704 I have just skimmed and has some nice examples
[21:30] nwf:	 http://dl.acm.org/citation.cfm?doid=322261.322272 has some more graph-based examples
```
* [Weighted Datalog and Levels of Trust](http://dl.acm.org/citation.cfm?id=1371966)

# Provenance semirings and x-fast trees
* [Provenance Semirings](http://db.cis.upenn.edu/DL/07/pods07.pdf)
* [Provenance query evaluation: what's so special about it?](http://dl.acm.org/citation.cfm?id=1646040)

# Consistency and distribution

* [Distributed Algorithms in NoSQL Databases](http://highlyscalable.wordpress.com/2012/09/18/distributed-algorithms-in-nosql-databases/)

# Latch-free architecture

* [High-Performance Concurrency Control Mechanisms for Main-Memory Databases](http://vldb.org/pvldb/vol5/p298_per-akelarson_vldb2012.pdf)

# Persistence and Versioning

* [Fully Persistent B-Trees](http://cs.au.dk/~gerth/pub/soda12.html)
* [Confluently Persistent Tries for Efficient Version Control](http://erikdemaine.org/papers/ConfluentTries_Algorithmica/paper.pdf) by Demaine, Langerman and Price is particularly interesting to me, because they provide O(log log n) movement time fully persistent hash table.

# Cache-Obliviousness

* [Cache Oblivious Sorting](http://cs.au.dk/~gerth/pub/encyclopedia08.html)
* [Cache Oblivious Streaming B-Trees](http://supertech.csail.mit.edu/papers/sbtree.pdf)
* [Streaming B-Trees for Filesystem Grand Challenges](http://institutes.lanl.gov/hec-fsio/workshops/2007/presentations/day1/Farach-Colton_sbtree-nsf07.pdf)
* [Orded Files and Cache-Oblivious Priority Queues (video)](http://www.youtube.com/watch?v=jAh_bC4hYlc)
* [A density control algorithm for doing insertions and deletions in a sequentially ordered file in a good worst-case time](http://www.sciencedirect.com/science/article/pii/089054019290034D). This provides the O((log^2 N)/B) _worst case_ bound for ordered file maintanence. Note that to do so it uses a "calibrator tree" which corresponds directly with the total number of children under a given branch. This could be-expressed like (or as part of) a Haar sketch.

# Logging and Metrics

* [The log-structured merge tree](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.44.2782&rep=rep1&type=pdf) by O'Neil, Cheng, Gawlick and O'Neil provides
 great throughput for inserts in exchange for high read latency for write-mostly workloads.

# Compressed Search

We could store full-text-indices on some columns as a BWT transformed version of the leave chunk, then perform compressed-search techniques on top to permit efficient operations on them without paying for full decompression.

At ~4 meg leaves this actually fits pretty well with bzip2 window sizes.

* [Searching BWT compressed text with the Boyer-Moore algorithm and binary search](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.106.9296)
* [A Comparison of BWT Approaches to String Pattern Matching](http://vlsi.cs.ucf.edu/nsf/files/reportVer05driver.pdf) by Firth, Bell, Mukherjee and Adjeroh
* [The SBC-Tree: An Index for Run-Length Compressed Sequences](http://www.ittc.ku.edu/~jsv/Papers/EHS08.SBCtree.pdf)
* [Geometric Burrows-Wheeler Transform: Linking Range Searching and Text Indexing](http://www.ittc.ku.edu/~jsv/Papers/CHS08.geometricbw.pdf)
* [Compressed Text Indexing and Range Searching](http://www.cs.purdue.edu/research/technical_reports/2006/TR%2006-021.pdf)

# Batched buffer management

* [The Buffer Tree: A Technique for Designing Batched External Data Structures](http://www.cs.cmu.edu/~guyb/realworld/slidesF10/buffertree.pdf)
