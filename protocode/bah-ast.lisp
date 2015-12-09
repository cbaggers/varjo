;; context is a bitch isnt it?

;; So we need to work out how to do spaces.

;; We can get the space used by a pos
;; so we need to know the current scope
;; we can search back through function calls for the last function called
;; 'use-space.

;; This will work but doesnt make me happy, how would we do this otherwise?

;; we should returns an ast where each node has:
;; - the code
;; - the starting environment
;; - the resulting code object

;; the code object was mutable so we had to change that. Fixed now :)

;; now we need a node

;; Ι hate defclass..I may fix this now as I have some time

;; eh, got bored
