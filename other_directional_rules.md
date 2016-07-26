# Other directional rules

We have rules like this one:

    getFirst <- getByIndex

Which is kind of short for

    structure.getFirst == structure.getByIndex(0)

Theoretically, we can explore both directions.

If we explore both directions, we can get arbitrarily long paths, which means that my algorithm might not terminate. This would suck. I might have to do some kind of breadth first search. But if I do that, my code might output nonoptimal solutions.
