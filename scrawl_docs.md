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


Variable identifiers can be made of any alphanumeric characters and underscores,
except for the first character which may not be a number.
Additionally they may be a variable identifier together with square bracket array indexing.

eg var, var1, _var2, var3[idx], var4[idx1][idx2]


