#||

It may be wise to allow defspecial to specify that the function can be taken as
a first class value even though in CL this is usually not allowed.

The example in this case is aref which is a regular function in CL but a
special in vari due to handling v-block-array.

||#
