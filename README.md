# Data structure composer

This is my latest attempt to write software which automatically chooses a good data structure for you, given an API which you need to support. I want the Quora user who asked [How can I design an efficient data structure that supports findMin, findMax, deleteMin, deleteMax, insert, and delete?](https://www.quora.com/How-can-I-design-an-efficient-data-structure-that-supports-findMin-findMax-deleteMin-deleteMax-Insert-and-delete) to be able to just go to a webpage, select those six methods from a checkbox, click "Choose", and get back the correct recommendation.

Here's how my software will do that.

The user might have some fancy UI, but my software will probably take an input that looks something like the following:

```
adt MinAndMaxSet {
  getMinimum
  getMaximum
  deleteMinimum!
  deleteMaximum!
  contains
  insertAnywhere!
  deleteByValue!
}
```

In this syntax, `adt` stands for "abstract data type".

I have a library of method implementations and data structures. Everything is a method on an ordered collection. Method implementations look like this--you can see all of them in `materials/MainImplementations.txt`:

```
// access by index
getFirst <- getByIndex
getNext <- getByIndex
getByIndex <- getFirst + n * getNext
getLast <- getByIndex
getPrev <- getByIndex

// looping
unorderedEach[f] <- each[f]
each[f] <- unorderedEach[_] + n * log(n) + f * n
each[f] <- getByIndex * n + f * n
each[f] <- getFirst + getNext * n + f * n

// insertions
insertAtEnd! <- getLast + insertAfterEndNode!
insertAfterEndNode! <- insertAfterNode!
insertBeforeFrontNode! <- insertBeforeNode!
insertAtFront! <- getFirst + insertBeforeFrontNode!
insertAtIndex! <- getByIndex + (insertAfterNode! | insertBeforeNode! )
insertAfterNode! <- insertNextToNode!
insertBeforeNode! <- insertNextToNode!
extend! <- getLast + insertAfterEndNode! * n
```

The first line means "You can implement `getFirst` with a constant number of calls to `getByIndex`." This requires that you know that `getFirst` is the method which returns the first node of the collection, and `getNext` is the method which returns the next node in the collection given one element.

The "node" means different things in the context of different data structures. In an array, the node is just the index in the array. In a linked list or a binary search tree, the node is the actual node in the data structure.

Different data structures are naturally defined in terms of different types of access methods. Arrays are most elegantly described as having a `getByIndex` method, and linked lists are most elegantly described as having a `getFirst` and `getNext` method. You can implement both of these in terms of each other, of course. The first three lines that I quoted above are:

```
// access by index
getFirst <- getByIndex
getNext <- getByIndex
getByIndex <- getFirst + n * getNext
```

An array would use the first two lines to define getFirst and getNext in terms of getByIndex, and a linked list would use the third line to define getByIndex in terms of getFirst and getNext.

This kind of fine-grained description of how different data structures work allows you to automatically make complex deductions about the speed of various operations. It's much more complicated and non-homogenous than the traditional way of describing different data structures. This makes it much more general and powerful.

Take a look at the [Big O Cheatsheet](http://bigocheatsheet.com/) quickly. It conflates all kinds of totally different things. For example, it gives you a single time column for "insertion", by which I assume it means insertion at an arbitrary index. If you break down insertion into finding the relevant node and then putting something after it, then you notice that the time complexities are quite different. Insertion of an arbitrary element into an array takes O(n) time, and that's because it's slow to insert something next to a node, not because it's slow to find the node. Insertion into a linked list takes O(n), but that's because finding the node was slow initially. This is all well known and obvious.

Here's a less-known case. AVL trees and red-black trees. Normally, insertion time is said to be O(log n) on average for both of them. This is true, but only because both of them require O(log n) time to find any particular node. It turns out that once you've found where you want to insert a node, red-black trees support amortized O(1) insertion (because their rebalancing is amortized O(1)), while AVL trees only support O(log n) ([see page 165 here](http://people.mpi-inf.mpg.de/~mehlhorn/ftp/Toolbox/SortedSequences.pdf)).

This matters, for example, when you're considering implementing an ordered collection with a red-black tree vs AVL tree. Inserting a sequence of length k into the middle of the list only takes O(log n + k) in a red-black tree, but O(k log n) in an AVL tree.

So I define a bunch of different data structures in terms of these different methods, and after defining a few basic operations, my software can infer how long all the other operations will take.

### Composed data structures

you can compose data structures

### Partial data structures

There's a bijection between linked lists and dynamically sized arrays--every operation which you can do on one, you can do on the other.

Some useful things can be called "partial data structures" because they don't support all of those things. I'm going to define "data structure" as "some data and some method implementations such that it can at least sometimes answer some queries".

For example, imagine that I have a list that I only ever add things to, and I want to store the maximum thing I've seen so far, by just having an extra "highest number" field and updating it on insert. I'd call this a partial data structure. In my syntax, I'd represent it like this:

```
HighestNumberMemoizer {
    insertAtIndex! <- 1
    getMaximumValue <- 1
}
```

(Why does this implement the `insertAtIndex!` method instead of, for example, `insertAtEnd!`? Because it supports every kind of insertion--I don't care where you're putting the new number, I can still take the maximum of it and my stored number--and with `insertAtIndex` you can do any other kind of insertion.)

### Parameterized and conditional data structures

The above data structure works for memoizing things other than the highest number. It works on any function which is commutative. So I can write

```
HighestNumberMemoizer {
    insertAtIndex! <- 1
    getMaximumValue <- 1
}
```

but can also do other things...

### Things that my v0.1 should be able to solve:

- min stack (Implement a stack data structure which supports `insert`, `pop`, and `findMinimumValue`, all in constant time)
- min queue (like the above, but a queue)
- List with getIndexOfMax
- List with getMax
- Design a data structure that supports the following operations: insert_back(), remove_front() and find_mode(), all in O(1)
- Is there a data structure for key-value pairs that allows insert, update, deletion and finding the minimal value all in log(n)?
- [What data structures support insertion, deletion and selection of a random element with a O(1)O(1) time complexity bound?](https://www.quora.com/What-data-structures-support-insertion-deletion-and-selection-of-a-random-element-with-a-math-O-1-math-time-complexity-bound)
- [How can I design an efficient data structure that supports findMin, findMax, deleteMin, deleteMax, Insert, and delete?](https://www.quora.com/How-can-I-design-an-efficient-data-structure-that-supports-findMin-findMax-deleteMin-deleteMax-Insert-and-delete)
- [What data structures support insertion, deletion and selection of a random element with a O(1) time complexity bound which allow duplicates?](https://www.quora.com/What-data-structures-support-insertion-deletion-and-selection-of-a-random-element-with-a-O-1-time-complexity-bound-which-allow-duplicates)
- [Does a data structure exist which supports logarithmic insertion/removal and constant-time query for the Kth smallest element?](https://www.quora.com/Does-a-data-structure-exist-which-supports-logarithmic-insertion-removal-and-constant-time-query-for-the-Kth-smallest-element)
- What data structure would allow adding an element, removing an element, and querying for the number of elements in the range (a, b), all in O(logn) time?
- [Can I delete an element from the middle of an array using queue in data structures?](https://www.quora.com/Can-I-delete-an-element-from-the-middle-of-an-array-using-queue-in-data-structures)

These are all eminently doable.

There might be some trickiness in making it easy for users to find all those methods--maybe they won't guess the name of `find_mode` or `pop!` or something like that. I will probably make a web app with descriptions for each method and data structure, and then send them to be backend with each request.

----

## Todo list:

immediate:

- [x] Handle parameterized data structures

then:

- [ ] Write Scalacheck tests to test that my parser can invert my toString methods
- [x] Parse ADT descriptions
- [x] Choose fastest data structure combination for a simple ADT
  - [x] initially, this can be done without the ability to introspect on what data structure is doing the work
  - [ ] then that introspection needs to be done.
- [ ] Store the chains of logic for every implementation, so that you can see what everything's relying on.
- [ ] generalize to parameterized ADTs.
- [ ] optimize the compiler to only consider composite data structures that could theoretically be better than simpler versions of themselves.

Interface things:

- [ ] Make a webpage
- [ ] Make a command line tool

new list of todos:

- [ ] figure out the data structure domination thing
- [ ] pass previous results of searching for read impls into the Chooser.getAllTimes method

### Things to include

- min stack memoizer

## Other crazy notes

- It might be nice to have higher-order data structures. example?
- My code assumes everything is an ordered list. Is there an elegant way of handling multiple different types (and conversions between them)?
- Do I have to do anything fancy to get things like `union` or `intersection` to work?
- The frontend might let you select between methods in multiple languages.

## more things

https://dzone.com/articles/introduction-probabilistic-0


----------

## Current tasks

- Front-end
  - [x] Let you specify non-primitive methods
  - Let you see which data structures correspond to which methods. (blocked by the search algorithm semantics one)
  - better search interface
- Search algorithm efficiency
  - Store an upper bound on cost required to be competitive with the best solutions, stop early on uncompetitive data structures
  - Better automatic reasoning on data structures not being helpful
  - As you add on data structures, auto-enqueing all the previous read methods found.
- Search algorithm semantics
  - Make it possible to see where the data structures were used in an implementation.
  - Allow me to nest data structures. So it's clearer when we're using an OST vs something else.
- Front end data structure wiki
  - [x] nah. Instead, make the parser able to handle descriptions 

---------

## Crazy extensions

work with more complex data? eg multiple columns?

output code???

- How hard would this be?
  - I think not that hard.
    - It relies on me knowing which method comes from where.
  - Key idea: just output all impls that were selected, except for the primitive insertions, which need to be done some other way.

