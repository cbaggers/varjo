# The Packages

There are 2 packages of note in the Varjo system: [vari](http://techsnuffle.com/varjo/vari-reference.html#VARI) & [varjo](http://techsnuffle.com/varjo/varjo-reference.html#VARJO.API)

`:Use` the `vari` package in packages where you (or your users) will be writing shaders in Vari

`:Use` the `varjo` package in packages where you will be running the compiler. Usually this is done from withing function/macros the users interact with. See [CEPL](https://github.com/cbaggers/cepl) for an example of such a system.
