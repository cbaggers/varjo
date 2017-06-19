# Constructors

```
int(uint)// converts an unsigned integer to a signed integer
int(bool)   // converts a Boolean value to an int
int(float)   // converts a float value to an int
int(double)  // converts a double value to a signed integer
uint(int)    // converts a signed integer value to an unsigned integer
uint(bool)   // converts a Boolean value to an unsigned integer
uint(float)  // converts a float value to an unsigned integer
uint(double) // converts a double value to an unsigned integer
bool(int)   // converts a signed integer value to a Boolean
bool(uint)// converts an unsigned integer value to a Boolean value
bool(float)// converts a float value to a Boolean
bool(double) // converts a double value to a Boolean
float(int)      // converts a signed integer value to a float
float(uint)  // converts an unsigned integer value to a float value
float(bool)     // converts a Boolean value to a float
float(double)// converts a double value to a float
double(int)  // converts a signed integer value to a double
double(uint) // converts an unsigned integer value to a double
double(bool) // converts a Boolean value to a double
double(float)// converts a float value to a double
```

- When constructors are used to convert any floating-point type to an
  integer type, the fractional part of the floating-point value is
  dropped. It is undefined to convert a negative floating-point value
  to an uint.

- When a constructor is used to convert any integer or floating-point
  type to a bool, 0 and 0.0 are converted to false, and non-zero
  values are converted to true. When a constructor is used to convert
  a bool to any integer or floating-point type, false is converted to
  0 or 0.0, and true is converted to 1 or 1.0.

- The constructor int(uint) preserves the bit pattern in the argument,
  which will change the argument's value if its sign bit is set. The
  constructor uint(int) preserves the bit pattern in the argument,
  which will change its value if it is negative.

- Identity constructors, like float(float) are also legal, but of
  little use.

- Scalar constructors with non-scalar parameters can be used to take
  the first element from a non-scalar.  For example, the constructor
  float(vec3) will select the first component of the vec3 parameter.

vec & mat constructors as expected. But also

- To initialize the diagonal of a matrix with all other elements set to zero:
  mat2(float)
  mat3(float)
  mat4(float)

- to initialize a matrix by specifying vectors or scalars, the
  components are assigned to the matrix elements in column-major
  order. `mat2(vec2, vec2); // one column per argument`

structure constructors as known

And that's it? seems so. Other types would be opaque
