# Built in Types

The basic types are as one would expect

`:bool` - t or nil
`:int` - signed 32bit in
`:uint` - unsigned 32bit in
`:float` - single precision (__ bit) floating point number
`:short-float` - half size (__ bit) floating point number
`:double` - double precision (__ bit) floating point number

## Vectors

`:vec2` - vector of 2 `:float`s
`:vec3` - vector of 3 `:float`s
`:vec4` - vector of 4 `:float`s

`:bvec2` - vector of 2 `:bool`s
`:bvec3` - vector of 2 `:bool`s
`:bvec4` - vector of 2 `:bool`s

`:uvec2` - vector of 2 `:uint`s
`:uvec3` - vector of 2 `:uint`s
`:uvec4` - vector of 2 `:uint`s

`:ivec2` - vector of 2 `:int`s
`:ivec3` - vector of 2 `:int`s
`:ivec4` - vector of 2 `:int`s

`:dvec2` - vector of 2 `:double`s
`:dvec3` - vector of 2 `:double`s
`:dvec4` - vector of 2 `:double`s

## Matrices

`:mat2` - 2x2 matrix of `:float`s
`:mat3` - 3x3 matrix of `:float`s
`:mat4` - 4x4 matrix of `:float`s

`:dmat2` - 2x2 matrix of `:double`s
`:dmat3` - 3x3 matrix of `:double`s
`:dmat4` - 4x4 matrix of `:double`s

`:mat2x2` - 2x2 matrix of `:float`s
`:mat2x3` - 2x3 matrix of `:float`s
`:mat2x4` - 2x4 matrix of `:float`s
`:mat3x2` - 3x2 matrix of `:float`s
`:mat3x3` - 3x3 matrix of `:float`s
`:mat3x4` - 3x4 matrix of `:float`s
`:mat4x2` - 4x2 matrix of `:float`s
`:mat4x3` - 4x3 matrix of `:float`s
`:mat4x4` - 4x4 matrix of `:float`s

`:dmat2x2` - 2x2 matrix of `:double`s
`:dmat2x3` - 2x3 matrix of `:double`s
`:dmat2x4` - 2x4 matrix of `:double`s
`:dmat3x2` - 3x2 matrix of `:double`s
`:dmat3x3` - 3x3 matrix of `:double`s
`:dmat3x4` - 3x4 matrix of `:double`s
`:dmat4x2` - 4x2 matrix of `:double`s
`:dmat4x3` - 4x3 matrix of `:double`s
`:dmat4x4` - 4x4 matrix of `:double`s

## Samplers

`:isampler-1d`
`:isampler-1d-array`
`:isampler-2d`
`:isampler-2d-array`
`:isampler-2d-ms`
`:isampler-2d-ms-array`
`:isampler-2d-rect`
`:isampler-3d`
`:isampler-buffer`
`:isampler-cube`
`:isampler-cube-array`

`:sampler-1d`
`:sampler-1d-array`
`:sampler-1d-array-shadow`
`:sampler-1d-shadow`
`:sampler-2d`
`:sampler-2d-array`
`:sampler-2d-array-shadow`
`:sampler-2d-ms`
`:sampler-2d-ms-array`
`:sampler-2d-rect`
`:sampler-2d-rect-shadow`
`:sampler-2d-shadow`
`:sampler-3d`
`:sampler-buffer`
`:sampler-cube`
`:sampler-cube-array`
`:sampler-cube-array-shadow`
`:sampler-cube-shadow`

`:usampler-1d`
`:usampler-1d-array`
`:usampler-2d`
`:usampler-2d-array`
`:usampler-2d-ms`
`:usampler-2d-ms-array`
`:usampler-2d-rect`
`:usampler-3d`
`:usampler-buffer`
`:usampler-cube`
`:usampler-cube-array`

`:isampler-rect`
`:usampler-rect`

## Images

`:image-1d`
`:iimage-1d`
`:uimage-1d`
`:image-2d`
`:iimage-2d`
`:uimage-2d`
`:image-3d`
`:iimage-3d`
`:uimage-3d`
`:image-rect`
`:iimage-rect`
`:uimage-rect`
`:image-2d-rect`
`:iimage-2d-rect`
`:uimage-2d-rect`
`:image-cube`
`:iimage-cube`
`:uimage-cube`
`:image-buffer`
`:iimage-buffer`
`:uimage-buffer`
`:image-1d-array`
`:iimage-1d-array`
`:uimage-1d-array`
`:image-2d-array`
`:iimage-2d-array`
`:uimage-2d-array`
`:image-cube-array`
`:iimage-cube-array`
`:uimage-cube-array`
`:image-2d-ms`
`:iimage-2d-ms`
`:uimage-2d-ms`
`:image-2d-ms-array`
`:iimage-2d-ms-array`
`:uimage-2d-ms-array`

`:ibuffer-image`
`:ubuffer-image`

## Atomic

`:atomic-uint`
