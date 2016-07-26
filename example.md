Here's an example of what I want my software to do.

Suppose you want to make a priority queue which lets you read priorities of elements in it.

    adt DoublePriorityQueue: Table[unique key, value] {
      insert: n
      getByUnique[key]: n
      getByMinimum[value]: n
    }

