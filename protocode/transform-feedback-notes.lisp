#|| file:///home/baggers/.mozilla/firefox/ml3pk08v.default/ScrapBook/data/20170909142637/index.html

Has to be set before linking (for cepl to handle)

Transform feedback only valid from last vertex related stage (others will be
silently ignored). For us this means that maybe we should pass the values
along the pipeline.. hmm

2 captures modes:
- interleaved writes to same buffer
- seperate writes to seperate buffers..ugh wiki terminology grossness here
  they dont mean seperate buffer objects, could be same object but different
  binding points

captures primitives, so can end up with more results than verts. The index
buffer and shit matters basically. This is a good quality.

each primitive is written to the result in the order given but the vert order
depends on primitive assembly.

when declaring the captured outputs you pass an array on names, the order he
dictates the order the data is written, which is great.

A component will always be a GLfloat/GLdouble, GLint, or GLuint. No packing or
normalization is performed. The elements written will be package (unless
doubles fuck something up[0]) but the systems doesnt do anything fancy will the
components.

We will use transform-feedback-objects for state on gl>=4 and emulate it for
lower versions. Some stuff (nesting tfo scopes) would require 'pause' which is
only available on 4 and up so those will have to runtime fail.


[0] doubles requires 8 byte alignment
||#

#||

Maybe we could add a *minimum-supported-gl* var so that, given the program they
have they can find out the minimum gl required.

Hmm the issue with this that some things (like transform feedback nesting) are
only assertained at runtime..so would require instrumenting a bunch of things
(which is gross)

||#

#||

What are the alignments for various machines?
What does cffi let us do?
What about the lisp itself?

||#

#||

- Could make current-surface have default context
- then can make swap an inline func calling generic swap-impl? cant dispatch on
  &optional so need impl of some sort.
- then we can have compiler-macro for swap which (on no arg) uses swap-surface
  with (current-surface) as arg.

or we can come up with some other name.. may be better

||#
