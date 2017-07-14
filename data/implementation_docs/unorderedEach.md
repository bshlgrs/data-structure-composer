Calls a function on every node in the list in an undefined order.

For example, you could use unorderedEach to get the sum of a list, by doing
something like

```
function getSum() {
  let sum = 0;

  [1, 2, 3, 4, 5].unorderedEach((node) => {
    sum += node.value;
  });

  return sum;
}
```

Because addition is commutative, it's fine that we don't know the iteration order.

It's the same as `each`, except the iteration order is undefined, which
means that it works for representation of a list which don't know the list's ordering.
