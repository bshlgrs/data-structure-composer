Calls a function on every node in the list in order.

For example, you could use `each` to get the sum of a list, by doing
something like

```
function getSum() {
  let sum = 0;

  [1, 2, 3, 4, 5].each((node) => {
    sum += node.value;
  });

  return sum;
}
```

Note that in this case, you don't need to use `each` because you'd get the same result if you used a different iteration order, so you could use `unorderedEach` instead.
