# How will my ADTs work?

Maybe I say this

    adt MinAndMaxSet {
      getMinimum
      getMaximum
      deleteMinimum!
      deleteMaximum!
      contains
      insertAnywhere!
      deleteByValue!
    }

So question: how does this work with my polymorphic data structures?

I have implementations like

    getMaximum <- getFirstBy[valueOrdering]
    
and data structures like

    // ordered by f, memoizing reductions on g
    OrderedRedBlackTree[f, g] {
        unorderedEach <- n
        getNext <- 1
        getPrev <- 1
        getFirst <- 1
        getLast <- 1
        updateNode! <- 1 + g
        getFirstBy[f] <- 1
        getLastBy[f] <- 1
        getKthBy[f] <- log(n)
        insertAtIndex! <- log(n) + g
        getFirstNodeWithValue[f] <- log(n)
        deleteNode! <- 1 + g
        countBetweenBy[f] <- log(n)
        twoSidedValueRangeQuery[g] <- g * log(n)
        oneSidedValueRangeQuery[g] <- g * log(n)
    }
    
This OrderedRedBlackTree data structure needs to learn that `f` is `valueOrdering` and `g` is unspecified/undefined/whatever.

### ideas

Maybe when I'm implementing I pretend `f` is anything you want it to be. If at the end, we find out that this was useful, then we see what it should be.

If it needs to be multiple contradictory things, then we can just build one of these trees for each of those. That's not going to be asymptotically slower. (This might do things which humans recognize as stupid though. Also I can imagine it perhaps interacting badly with cases where you need a Tree on f and a Heap on g.) 

What do I learn from conditions like `if f.foo` in the data structure? Nothing (unless I was allowed to specify `not foo` somewhere as a condition), because this condition is just going to stack with other conditions. But I will return it from the data structure binding as a limitation.

How do insertion methods work? If they are unfree, then it's easy: just take the sum of times from the different data structures and work with those.

#### Examples 

So here's the implementation.  

If I have a `VectorList` and a `Heap` with the following times:

    VectorList {
        getByIndex <- 1
        updateNode! <- 1
        insertAtEnd! <- 1
        deleteAtIndex! <- n
        deleteBetweenNodes! <- n
        deleteAtEnd! <- 1
    }
    
    Heap {
        unorderedEach <- n
        getMinimum <- 1
        deleteNode! <- log(n)
        updateNode! <- log(n)
        insertAtIndex! <- 1
    }


Firstly, filter them down to just their get methods:

    VectorList + Heap {
        getByIndex <- 1
        unorderedEach <- n
        getMinimum <- 1
    }

Now, run inference on their get methods.

(Optionally: If their get methods aren't faster than your current best choice, stop here.)

So we have a list of get methods. Now, for each data structure separately (optionally: only do this if their set methods take different amounts of time), figure out the best times for the mutating methods for that data structure. This is going to be a `Set[SearchResult]`. Now add all these results together, and you're done. :boom: :moneybag: :moneybag:

#### Example with parameterization

Screw concreteness:

    Foo[x] {
        a[x] <- log(n) + x
        b <- 1
    }
    
    Bar[y] {
        c[y] <- n * y
    }
    
    
    a[z] <- n * b + n * z
    c[w] <- n * a + n * w
    

Let's look at `Foo + Bar`:

    
    Foo + Bar {
        a[x] <- log(n) + x
        b <- 1
        c[y] <- n * y
    }
    
Combine this with the free impls, to get:

    a[z] <- n * b + n * z    (1) (source: natural)
    c[w] <- n * a[w] + n * w (2) (source: natural)
    a[x] <- log(n) + x       (3) (source: Foo[x])
    b <- 1                   (4) (source: Foo[x])
    c[y] <- n * y            (5) (source: Bar[y])

Search through all these and get

    b <- 1                   (4) (source: Foo[x])
    a[x] <- log(n) + x       (3) (source: Foo[x])
    c[y] <- n * y            (5) (source: Bar[y])

Now, we're basically done answering the question "how fast will everything go if you use a `Foo + Bar`?" But we haven't answered the question "What do we need to parameterize `Foo` and `Bar` on to get this amazing performance on a particular ADT?"


#### Example with single data structure and parameterization

    ds BST[x] {
        getKthBy[x] <- log(n) 
    }
    
    getFirstBy[x] <- getKthBy[x]
    getSmallest <- getFirstBy[valueOrdering]
    
    valueOrdering <- 1

    adt MyThing[f] {
        getSmallest
        getFirstBy[f]
    }

So we get our fastest impls

    getKthBy[x] <- log(n) // (1) From BST[x]
    getFirstBy[y] <- log(n) // (2) Using (1)
    getSmallest <- 1  // (3) Using (2)

Then we get the subset of these that matter to our ADT. Then we DFS up through the UnfreeImpls to see which usages of the BST there were. In this case we find that `getKthBy` used with both `f` and and `valueOrdering`. So the BST has to be on both of those.  

#### data structure relevance detection

^^ If we're already doing that kind of stuff, maybe we don't need to search separately for all the different data structures?

Maybe I can get a list of methods which potentially can be implemented faster if a particular data structure is implemented faster?

- Simplest version: the descendants of the data structure in the implementation dependency graph
- It could get fancier: it could exclude descendants which can't possibly be made faster by this data structure. Like, if I claim that a data structure also has an O(n**4) implementation of `getFirst`, I should be able to think about that and realize that the data structure isn't relevant just because `getFirst` is used.

## parameterized ADTs

Also, I could allow ADTs like:

    adt MinAndMaxSet[z] {
      getFirstBy[z]
      getLastBy[z]
      deleteMinimumBy![z]
      deleteMaximumBy![z]
      contains
      insertAnywhere!
      deleteByValue!
    }

and then the OrderedBlackTree should bind `f` to `z` and `g` to `_`.
