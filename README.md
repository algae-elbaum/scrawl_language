#Scrawl

Scrawl is a language being made by Kyle Seipp and Henry Elbaum. (Now on
potentially infinite hiatus). It vaguely follows *Modern Compiler Implementation
in ML* by Andrew Appel.

At its current state, it can compile all planned features into an intermediate
'pseudo-assembly' language. An interpreter is implemented for that language, but
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

Use the file extension .spl for Scrawl programs


Since this project is long term, there are going to be TODOs that should be
kept in the back of one's head. Thus, after a TODO, there should be a number
ranging from 0 to 5 describing how urgent the change is. Example: TODO 4 is not
very urgent.
