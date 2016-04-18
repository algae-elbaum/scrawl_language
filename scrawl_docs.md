#Documentation

I plan to start working in ernest on this doc later this week. For now I'll
leave it at this:

Right now, Scrawl syntax is largely C-like except for array declaration and
function definition.

An int array of length 5 named x would be declared as `int[5] x;`

Functions are defined with an =. So if we want to define a function at the time
of declaration it will look like  
`func_name(param_list) = ...`  
And if we want to define or redefine a function after it's been declared, it'll 
look like  
`func_name = lambda (param_list) -> ... `  
The return type will (hopefully) be inferred from the type of the expression in 
the return statement of the function
