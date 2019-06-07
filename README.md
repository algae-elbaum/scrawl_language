# Scrawl

Scrawl is a language being made by Algae Elbaum and Kyle Seipp. (Now on
potentially infinite hiatus). It vaguely follows *Modern Compiler Implementation
in ML* by Andrew Appel.

At its current state, it can compile all planned features into an intermediate
'pseudo-assembly' language. Most of an interpreter is implemented for that language, but
lacks some features like function calls.

If we every decide to complete this, the next steps would be to complete the
interpreter and then work on translating the intermediate language into either
x86 or llvm. In the first case we would want to focus a lot on optimizations,
and in the second case my impression is that llvm largely takes care of that for
us. Learning about optimizations ourselves seems more interesting. Another
important thing to do would be to complete the documentation.

Dependencies:
  - ocamlbuild
  - menhir


TODOs are followed by numbers to indicate their priority. The higher the number the less
important
