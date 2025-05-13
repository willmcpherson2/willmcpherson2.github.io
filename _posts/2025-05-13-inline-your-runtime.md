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

# Link Your Runtime

Ok, so how do we actually use this technique?
The most straightforward solution is to implement the programming language as an executable compiler and a runtime library.
The compiler generates a binary which is then linked against the runtime library.

This is a fine solution, and I actually recommend it over the monstrosity that I'll be describing in this post.
However it's worth understanding some of the downsides of linking your runtime.

> Inlining is the mother of all optimisations

— Rémi Forax ([earliest source I could find](https://wingolog.org/archives/2011/07/05/v8-a-tale-of-two-compilers#788347f5d21641a7115ba069f58715848dba9850))

By linking two object files, we're missing out on all sorts of optimisations.

# Inline Your Runtime
