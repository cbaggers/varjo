
(defun foo ()
  (in *view-space*
      (do-this)
      (do-that)))

(defun foo ()
  (convert-to
   *current-space*
   (let ((*current-space* *view-space*))
     (do-this)
     (do-that))))

#||

Needs the type to hold the value, so we are in dependent types again
crazy town

So let's make a compile-time-value type. We then make func-spec-val and space
inherit from it.

We then say that any time a function with ctv arguments is called we make a
local version of the func with those values hardcoded.

Now there are two types of ctv. One where it is removed entirely (e.g.
functions) and one where it is swapped out of another type (e.g. spaces).

remove is a special case of swap really. We should have some kind of calc-type
method that get's called and if you return :void it removes the arg.

Maybe instead of calc-type it should be calc-arg. Then for spaces we return
'*current-space* and for functions we return nil.

||#
