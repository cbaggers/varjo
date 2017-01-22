
#||

One thing we need to do is split up the env into two parts. The lexical env and
the runtime env.

The lexical env is always passed down, you add bindings to it and that's all. It
has a 'parent' slot which points to it's lexical ancestor.

The runtime environment carries the side-effects. It is returned from ever
compilation action with the new state of the runtime environment. It has a
parent which points to state of the runtime before the compilation of that form
began.

||#
