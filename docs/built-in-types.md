# Built in Types

The basic types are as one would expect

```
:bool - t or nil
:int - signed 32bit in
:uint - unsigned 32bit in
:float - single precision (__ bit) floating point number
:short-float - half size (__ bit) floating point number
:double - double precision (__ bit) floating point number
```

## Vectors

```
:vec2 - vector of 2 :floats
:vec3 - vector of 3 :floats
:vec4 - vector of 4 :floats
```

```
:bvec2 - vector of 2 :bools
:bvec3 - vector of 3 :bools
:bvec4 - vector of 4 :bools
```

```
:uvec2 - vector of 2 :uints
:uvec3 - vector of 3 :uints
:uvec4 - vector of 4 :uints
```

```
:ivec2 - vector of 2 :ints
:ivec3 - vector of 3 :ints
:ivec4 - vector of 4 :ints
```

```
:dvec2 - vector of 2 :doubles
:dvec3 - vector of 3 :doubles
:dvec4 - vector of 4 :doubles
```

## Matrices

```
:mat2 - 2x2 matrix of :floats
:mat3 - 3x3 matrix of :floats
:mat4 - 4x4 matrix of :floats
```

```
:dmat2 - 2x2 matrix of :doubles
:dmat3 - 3x3 matrix of :doubles
:dmat4 - 4x4 matrix of :doubles
```

```
:mat2x2 - 2x2 matrix of :floats
:mat2x3 - 2x3 matrix of :floats
:mat2x4 - 2x4 matrix of :floats
:mat3x2 - 3x2 matrix of :floats
:mat3x3 - 3x3 matrix of :floats
:mat3x4 - 3x4 matrix of :floats
:mat4x2 - 4x2 matrix of :floats
:mat4x3 - 4x3 matrix of :floats
:mat4x4 - 4x4 matrix of :floats
```

```
:dmat2x2 - 2x2 matrix of :doubles
:dmat2x3 - 2x3 matrix of :doubles
:dmat2x4 - 2x4 matrix of :doubles
:dmat3x2 - 3x2 matrix of :doubles
:dmat3x3 - 3x3 matrix of :doubles
:dmat3x4 - 3x4 matrix of :doubles
:dmat4x2 - 4x2 matrix of :doubles
:dmat4x3 - 4x3 matrix of :doubles
:dmat4x4 - 4x4 matrix of :doubles
```


## Samplers

```
:isampler-1d
:isampler-1d-array
:isampler-2d
:isampler-2d-array
:isampler-2d-ms
:isampler-2d-ms-array
:isampler-2d-rect
:isampler-3d
:isampler-buffer
:isampler-cube
:isampler-cube-array

```
:sampler-1d
:sampler-1d-array
:sampler-1d-array-shadow
:sampler-1d-shadow
:sampler-2d
:sampler-2d-array
:sampler-2d-array-shadow
:sampler-2d-ms
:sampler-2d-ms-array
:sampler-2d-rect
:sampler-2d-rect-shadow
:sampler-2d-shadow
:sampler-3d
:sampler-buffer
:sampler-cube
:sampler-cube-array
:sampler-cube-array-shadow
:sampler-cube-shadow
```
```
:usampler-1d
:usampler-1d-array
:usampler-2d
:usampler-2d-array
:usampler-2d-ms
:usampler-2d-ms-array
:usampler-2d-rect
:usampler-3d
:usampler-buffer
:usampler-cube
:usampler-cube-array
```
```
:isampler-rect
:usampler-rect
```

## Images

```
:image-1d
:iimage-1d
:uimage-1d
:image-2d
:iimage-2d
:uimage-2d
:image-3d
:iimage-3d
:uimage-3d
:image-rect
:iimage-rect
:uimage-rect
:image-2d-rect
:iimage-2d-rect
:uimage-2d-rect
:image-cube
:iimage-cube
:uimage-cube
:image-buffer
:iimage-buffer
:uimage-buffer
:image-1d-array
:iimage-1d-array
:uimage-1d-array
:image-2d-array
:iimage-2d-array
:uimage-2d-array
:image-cube-array
:iimage-cube-array
:uimage-cube-array
:image-2d-ms
:iimage-2d-ms
:uimage-2d-ms
:image-2d-ms-array
:iimage-2d-ms-array
:uimage-2d-ms-array
```

```
:ibuffer-image
:ubuffer-image
```

## Atomic

```
:atomic-uint
```
