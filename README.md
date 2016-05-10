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
  insertAtEnd!
  deleteByValue!
}
```

In this syntax, `adt` stands for "abstract data type".






------------

## Steps to victory

### Where I'm at now

I have parsers to get me data structures and implementations.

Both data structures and implementations are parameterizable.

### Step 1

Given an unparameterized data structure, choose implementations for all methods, but ignoring parameterization. I need some way of knowing from each implementation whether it came from the data structure or not.

### Step 2

Given an unparameterized data structure, choose implementations for all the methods, including the ones which are implemented via a parameterized implementation like `mostCommonElement <- mostNumerousEquivalenceClass[_]`.

This will involve dealing with predicates

### After that

Things I'll need to support:

- 
