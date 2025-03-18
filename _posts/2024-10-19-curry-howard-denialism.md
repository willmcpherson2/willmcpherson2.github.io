---
layout: post
title: "Curry-Howard Denialism"
---

# Introduction

The Curry-Howard correspondence is the observation that writing a computer program is equivalent to writing a mathematical proof.

In this interpretation, a type is a proposition and a program is a proof.

Let's look at a simple example in TypeScript:

```typescript
// Proposition: If A implies B, and B implies C, then A implies C
function compose<A, B, C>(f: (x: A) => B, g: (x: B) => C, x: A): C {
  // Proof
  return g(f(x));
}

// Usage
console.log(compose(n => n.toString(), s => s.length > 1, 42));
```

In programming terms, we've defined a composition function.

In mathematical terms, we've written a logical proof for transitivity.

And after our proof has been verified, we end up with a program we can run in a web browser.

# Consequences

This is a simple observation that has massive implications.

For mathematicians, it means that computers can verify proofs. Even better, it means that computers can assist humans in developing proofs. In the same way that calculators revolutionised how we deal with numbers, soon computers will automate the work of proving mathematical conjectures.

For programmers, it means that computers can verify programs. But at a deeper level, it means that we will soon understand computer programs in the same way that mathematicians understand numbers. We will develop completeley new ways to write programs.

Computers are new and we have no idea how to use them.

# Today

Today, we are oblivious to the Curry-Howard interpretation. In the history of computing, we are at the beginning.

What exactly are we doing today that will one day be considered primitive?

Current methods of achieving software reliability will be considered brutish. Currently, the primary method for verification in the software industry is testing. This is like testing a mathematical conjecture by checking lots of cases. In the future, software will be verified by compilers and there will be no tests. Proofs will be written with the assistance of a computer.

Today's software abstractions will be considered low-level. Programming libraries in the future will be indistinguishable from mathematics. They will contain theorems, proofs and all sorts of tools for writing complex programs with very little code. Programming will feel more like writing a specification.

The performance characteristics of software will also change dramatically. Types have a direct impact on run-time behaviour, and this impact will continue to grow as compilers become more sophisticated.

We have entered a period of *Curry-Howard denialism* in the software industry. Even though it has been shown that the application of type theory is a necessity, many will continue to insist on inferior, but more familiar systems.
