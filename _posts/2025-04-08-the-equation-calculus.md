---
layout: post
title: "The Equation Calculus"
published: false
---

* toc
{:toc}

---

| [github.com/willmcpherson2/equation](https://github.com/willmcpherson2/equation)

I love programming language design.
And a programming language is really just a bunch of extensions and syntactic sugar on top of a *core language*.

For some languages it's obvious what the core language is - Haskell is just [System F](https://en.wikipedia.org/wiki/System_F) (although it's a bit more complicated than that).

For other languages, the core language may not have a name, but you can kind of tell which bits are really fundamental - for Ruby this would be its object system.

The most well known core language is the [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus).
Most functional languages are based on the lambda calculus.
It only has 3 features:

| Feature     | Syntax  |
|-------------|---------|
| Variable    | `x`     |
| Application | `f x`   |
| Function    | `λx. y` |

These turn out to be enough to write any program.

What's interesting is how you develop a general purpose programming language on top of these simple rules.

While it's tempting to start adding your favourite keywords, you should step back and understand some of the complex behaviour that emerges from these simple rules.
It may seem that the lambda calculus is missing a whole lot, but once you learn how to encode data types and perform control flow, you realise just how much you get out of so little.

Once you get a deeper understanding of the core language's set of programs, you can start to see the [Turing tar pit](https://en.wikipedia.org/wiki/Turing_tarpit) you're dealing with.
That is, the stuff that is difficult, repetitive or missing.

Another interesting core language is [combinatory logic](https://en.wikipedia.org/wiki/Combinatory_logic).
For example, the [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) also has only 3 features:

| Combinator   | Definition            |
|--------------|-----------------------|
| Identity     | `I x = x`             |
| Constant     | `K x y = x`           |
| Substitution | `S f g x = f x (g x)` |

There are basically 3 built-in functions that you can use to express any computation.

So what about a language where you can only define combinators?
That would give us the following core language:

| Feature      | Syntax    |
|--------------|-----------|
| Variable     | `x`       |
| Application  | `f x`     |
| Definition   | `F x = y` |

So it's basically the lambda calculus, but with named functions instead of anonymous functions.

Or, it's a combinator calculus where you have no built-in combinators, but you can define your own.

I'm calling this the **equation calculus**.
I took this terminology (and syntax) directly from [Haskell functions](https://www.haskell.org/tutorial/functions.html).

Here's addition on natural numbers defined in the equation calculus:

```
succ n s z = s n
zero s z = z

add a b = b (addS a) a
addS a b = succ (add a b)
```

So how does it compare to the lambda calculus?

Well firstly, function definitions just make things much easier to structure.
Lambda calculus examples almost always introduce definitions just to make it possible to follow along.
That's cheating!

Another upside is that function definitions are naturally recursive, whereas lambda terms aren't.
In the lambda calculus you have to use something like the [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator_in_lambda_calculus) to get recursion.

Now, one of the downsides is that you don't get environment capture.
This is where a function can refer to everything that was in its environment when the function was created:

```
λx. f (λy. x)
```

In the equation calculus, you often have to define a separate function and explicitly pass in whatever you want captured:

```
main x = f (const x)
const x y = x
```
