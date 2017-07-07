#||

- v-unrepresentable-value is a bad name, gotta change this. This is the kind of
  ephem which requires function splicing (that is the function we pass it to
  gets spliced)
  Currently v-function & v-any-one-of are the types that are unreps

- v-any-one-of might me more correctly named v-all-of as it really is.. nah
  any-one-of is close enough

- I removed these:
  add-alternate-type-name
  resolve-name-from-alternative
  alternate-name-for

- maybe replace *registered-types* with looking at the keys of *v-names*

- #'type-specp -v- #'vtype-existsp. What are the differences

||#
