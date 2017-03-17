# Arrays

## Rules from the spec

- When an array size is specified in a declaration, it must be an integral constant
  expression greater than 0

- Except for the last declared member of a shader storage block (a kind of interface block),
  the size of an array must be declared (explicitly sized) before it is indexed with
  anything other than an integral constant expression

- The size of any array must be declared before passing it as an argument to a function

- It is legal to declare an array without a size. When this is done:
 - redeclare the array specifying the size (unless it was in a block)
 - index with integral constant expressions (which implicitly sizes the array)

- No multidimensional array, but array of arrays are allowed

- All elements of arrays inherently have the same type and size except in the following case:
  The slot of a block can be dynamically sized. So this array'd block can have a different length
  'v' for each element of 'name'
```
    buffer b {
       int u[3];
       vec4 v[]; // okay, v will be sized dynamically, if not statically
    } name[3];
```

- Type decls
  `float[5]` // an array of size [5] of float
  `float[2][3]` // an array of size [2][3] of float, not size [3] of float[2]
  `float[5] a;`
  `vec4[3][2] a;` // size-3 array of size-2 array of vec4

- Constructor
  `float[5](3.4, 4.2, 5.0, 5.2, 1.1)`
  `vec4[3][2](b, b, b)`

- When in transparent memory (like in a uniform block), the layout is that the inner-most
  (right-most in declaration) dimensions iterate faster than outer dimensions. That is, for
  `vec4[3][2] a`, the order in memory would be:
  Low address : `a[0][0]` : `a[0][1]` : `a[1][0]` : `a[1][1]` : `a[2][0]` : `a[2][1]` : High address

- `length` cannot be called on an array that has not yet been explicitly sized

- Arrays are 1D so length is an int
  `vec4 a[3][2];`
  `a.length()` // this is 3
  `a[x].length()` // this is 2

## Our goals

- Support the behaviours
- Don't worry about the const rules for now we will revisit after adding proper support for that
-
