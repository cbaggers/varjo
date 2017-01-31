
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

hmm, I'm not happy with this ↑↑↑ yet. Let's crack out the old spaces impl and
see what it could do. We need to get a good baseline so we can work out how
to match it.

IIRC we actually dont need calc-arg. We always remove the args. Spaces is going
to calc a implicit uniform and use that. Making instances of spaces using
runtime data will be harder but fuck it, we dont have that in spaces today
either.

'or' types will be a special case where we will be returning an int to specify
which type it is at runtime, but that is for much later.

- - - Side Note - - -

I'm wondering if I actually want to have defvar & defparameter for varjo.
Just so we can use define things like *current-space* upfront so it doesnt
error when used in an external function.

eh, let's not do this yet

- - - - - - - - - - -

So back to the basics:

||#

(defun-g foo ((a :int) (f (function (:int) :int)))
  (funcall f a))

(defun-g bar ()
  (labels ((dubl ((a :int)) (* 2 a)))
    (foo 10 #'dubl)))

;; becomes

(defun-g bar ()
  (labels ((dubl ((a :int)) (* 2 a)))
    (labels ((foo ((a :int))
               (let ((f #'dubl))
                 (funcall f a))))
      (foo 10))))

#||

Nice and easy!

||#
