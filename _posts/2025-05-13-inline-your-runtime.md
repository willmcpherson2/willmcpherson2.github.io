---
layout: post
title: "Inline Your Runtime"
published: false
---

| [github.com/willmcpherson2/inline-your-runtime](https://github.com/willmcpherson2/inline-your-runtime)

# Compiler vs Runtime

In programming language implementation, we often separate the **compiler** from the **runtime**.
This is useful when generating code that isn't affected by the input program.

For example, arithmetic and conditionals require generating very specific instructions, whereas a built-in data structure should be defined in the runtime system.
The compiler can then generate instructions to call into the runtime API.
This is especially pertinent if you're using something like LLVM where generating instructions can be cumbersome.

So basically: if you find yourself defining a function *via code generation*, you should just define that function in a programming language.

This separation of concerns becomes practically unavoidable for high-level languages where you need to define closures, data structures, I/O, etc.

# Link Your Runtime

Ok, so how do we actually use this technique?
The most straightforward solution is to implement the programming language as an executable compiler and a runtime library.
The compiler generates a binary which is then linked against the runtime library.

This is a fine solution, and I actually recommend it over the monstrosity that I'll be describing in this post.
However it's worth understanding some of the downsides of linking your runtime.

By separately compiling the executable from its runtime, we miss out on a lot of the optimisations available in LLVM.
While the executable and the runtime can be optimised separately, information is lost in the boundary.

This is particularly problematic because this is the *runtime of a programming language*.
Micro-optimisations actually matter here - a 1% improvement is a 1% improvement for every program.

# Inline Your Runtime

> Inlining is the mother of all optimisations

— Rémi Forax ([earliest source I could find](https://wingolog.org/archives/2011/07/05/v8-a-tale-of-two-compilers#788347f5d21641a7115ba069f58715848dba9850))

Basically we want something [Link Time Optimisation](https://llvm.org/docs/LinkTimeOptimization.html) (LTO), which can optimise across module boundaries.

LTO is a bit confusing and I don't understand it.
But basically, instead of linking object files, you tell the compiler (e.g. Clang) to emit something it can actually optimise (e.g. LLVM bitcode).
This enables whole-program optimisation.

But how do we do that with our executable and runtime?
Well we could compile our runtime to LLVM bitcode ahead of time, compile the input program to LLVM bitcode and then link them.
For example if `rts.bc` is our runtime library and `output.bc` is our generated code:

```
$ llvm-link rts.bc output.bc -o main.bc
$ opt main.bc -o main.bc
$ llc --filetype=obj main.bc -o main.o
$ cc main.o -o main
```

Our compiler calls some LLVM APIs to link the bitcode, optimise it and compile it to an object file.
Then we invoke the system compiler to link to an executable.

Or in the case of JIT compiling:

```
$ llvm-link rts.bc output.bc -o main.bc
$ opt main.bc -o main.bc
$ lli main.bc
```
