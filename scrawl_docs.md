#Documentation

A scrawl program consists of a list of semicolon delimited expressions.

An expression is any of:
    a variable
    a variable declaration
    variable assignment
    a lambda expression
    a return expression
    an int, float, string, or boolean literal
    a function call
    two expressions combined using a binary operator
    an expression combined with a unary operator
    an if expression
    a for loop
    a while loop


Variable identifiers can be made of underscores and any alphanumeric characters.
However the first character may not be a number.
Additionally a variable identifier may be a variable identifier together with square
bracketed array indexing.
Any variable identifier that does not index into an array is called a simple variable, and those
that do index into arrays are called array variables. (Note that an identifier which represents an
array may itself be a simple variable or an array variable. Array variables are those which represent
members of arrays) 

eg var, var1, _var2, var3[idx], var4[idx1][idx2]


Need to put more stuff in here
------------

Free variables in functions, including lambdas are taken to refer to the object
of the same name in scope of the function  definition. Changes to the variable
in the function apply to the object to which it refers, and similarly changes to
the object are reflected in the function execution.

e.g.

    string a = "a";
    fun foo() -> none;
    foo = fun () -> none
    {
        print(a);
        a = "b";
    };
    foo();
    foo();

will print: ab
