# Bibliography

here are papers I've read and papers I want to read

## Things to read

[CS166 project topics handout](http://web.stanford.edu/class/cs166/handouts/090%20Final%20Project%20Topics.pdf). This has lots of data structures in it which I should put into my system. 

Pooya Davoodi, [Data Structures: Range Queries and
                Space Efficiency](http://www.cs.au.dk/~gerth/advising/thesis/pooya-davoodi.pdf)

> We study data structures for variants of range query problems. In particular, we consider
  (1) the range minimum problem: given a multidimensional array, find the position
  of the minimum element in a query rectangle; (2) the path minima problem: given a
  weighted tree, find the edge with minimum weight in a query path; (3) the range diameter
  problem: given a point set in the plane, find two points that are farthest away
  in a query rectangle. These and similar problems arise in various applications including
  document retrieval, genome sequence analysis, OLAP data cubes, network flows,
  shape-fitting, and clustering.
  The three mentioned problems are considered for either static inputs or dynamic inputs.
  In the static setting, we investigate the space-efficiency of data structures, which
  is an important aspect in massive data algorithmics. We provide lower bounds on the
  trade-off between the query time and the space usage of range minimum and range
  diameter data structures. We also present data structures for these problems to either
  complement the lower bounds or beating the lower bounds under certain assumptions
  about inputs. One of the results proves that to answer a multidimensional range minimum
  query, the product of the number of cells that we need to read and the number
  of bits stored in indexing data structures is at least equal to the number of elements
  in input arrays. Another result shows that using at most linear bits in addition to an
  array, we can achieve constant query time to support two-dimensional range minimum
  queries.
  In the dynamic setting, we present data structures for the path minima problem
  that support optimal query time for various types of update operations. One of the
  results presents a comparison-based data structure which answers path minima queries
  in sub-logarithmic time and supports updating the edge-weights of input trees in optimal
  logarithmic time. We also prove lower bounds on trade-offs between the query
  time and the update time of path minima data structures.
  Finally, we study the space-efficiency of cardinal tree representations in the dynamic
  setting, which has practical applications, for example in text indexing structures.
  We present a succinct dynamic data structure that supports traversing low-arity
  cardinal trees while answering queries during the traversal.

