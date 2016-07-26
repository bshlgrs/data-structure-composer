# Different kinds of algorithm problems

## Compose data structures to support a given API quickly

lots, eg

- min stack
- min queue
- Time-travelling key value store
- List with getIndexOfMax
- List with getMax
- Design a data structure that supports the following operations: insert_back(), remove_front() and find_mode(), all in O(1)
- Is there a data structure for key-value pairs that allows insert, update, deletion and finding the minimal value all in log(n)?

### Support a given API of queries which are compositions of simple operations

Eg making indexes for different kinds of queries like `blah.map(f).filter(g).count`

## Graph algorithms

Lots. Lots of things that are basically shortest-path algorithms.

## Dynamic programming on arrays

Build problems out of forall and thereexists and reductions on arrays.

For example:

Find the minimum sum subarray of an array:

    array.choose_n_indices(2).minimizeBy((i, j) => array[i..j].sum)

Find the most evenly balanced subarrays of an array:

    array.choose_n_indices(3)
         .minimizeBy((i, j, k) => abs(array[i..j] - array[j..k]))

Find the amount of water that fills up an array

    array.indices.map((i) => {
      min(array.take(i).maximize(_), array.drop(i).maximize(_)) - array[i]
    }).sum

Incidentally, you can generalize that one to graphs. It looks like this:

    graph.nodes.map((node) => {
      graph.pathsFromNodeToSet(set(node), boundary).minBy((path) => path.max)
    }).sum

Given a histogram, determine the area of the largest rectangle that can be drawn completely within the bounds of the histogram.

