Lambda calculus compiler using the Cheney on the MTA code generation technique
described here:

http://home.pipeline.com/~hbaker1/CheneyMTA.html
http://www.more-magic.net/posts/internals-gc.html


### the gist
* parses lambda syntax e.g. `(lambda (x) (lambda (y) (y x)))`
* converts closures by keeping track of all bound free-variables in each expression
* transforms the AST into Continuation-passing style 
  * no expression returns from a lambda, we call an explicit 'continuation' instead with the result, the continuation actually represents the remainder of the program)
* with explicit continuations, we can easily convert the code into C
  * each lambda is converted directly into a C function which takes a continuation and the environment of the lambda, as well as any free variables 
  * `(lambda (x) x)` becomes `void func(cont* kont, closure* x) { kont(x); }`
  * instead of returning from the C function, we just call into the continuation!
  * mark all C functions as 'noreturn' (gcc attribute)
  * allocate lambdas, closures, etc. directly on the C stack
  * check the position of the C stack ocasionally, if it gets too deep, perform GC by copying live objects (arguments to the current function, as well as everything locally allocated) into another memory pool (second generation), and use `longjmp` to blow away all the dead objects in the C stack. Easy generational garbage collection!
