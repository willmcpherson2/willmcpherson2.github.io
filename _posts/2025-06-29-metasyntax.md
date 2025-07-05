---
layout: post
title: "Metasyntax"
published: true
---

* toc
{:toc}

---

In this post I will look at some *metasyntaxes*.
These are general-purpose syntaxes that don't have inherent semantics.
Because a metasyntax describes a generic data structure instead of an AST, they are very easy to extend within the language through macros.

| ℹ️ ["Metasyntax"](https://en.wikipedia.org/wiki/Metasyntax) actually means _a syntax that can be used to describe other syntaxes_. I'm reusing that name for a different concept.

# Syntax

Most programming languages have a 1:1 relationship between syntax tokens and the AST.
You use keywords, delimiters and terminators to change the state of the parser.

```
def f(x, y) { print(x); print(y); }

def main() { f(1, 2); }
```

This is very easy to implement with a [parser generator](https://en.wikipedia.org/wiki/Comparison_of_parser_generators).

This approach is fine, and it's what most programming languages do.
However, these languages tend to become more complex over time, and we end up with a very large set of syntax rules.
It would be much better if we had a single, predictable way to write expressions.

# Metasyntax

## S-expressions

With an s-expression syntax, you basically just parse a tree using parentheses.
For example:

```
(def f (x y) (do (print x) (print y)))

(def main () (do (f 1 2)))
```

Everything is either a tree `(f x ...)` or an atom `x`.

You can see that even though it has built-in keywords like `def` and `do`, the parser will also recognise something like `(table (1 2 3) (4 5 6))`, and the language can provide some system to parse that tree within a program.
That's what Lisp's metaprogramming system is.

## Indentation trees

Another way to encode trees is with whitespace:

```
def f (x y)
  do
    print x
    print y

def main ()
  do (f 1 2)
```

There are a few ways of doing this, but here I'm using these rules:

1. Add an opening parenthesis to the start of each line
2. Add `max(0, 1 - N)` closing parentheses to the end of each line, where `N` is the change in indentation of the next line

You end up with this tree, which is identical to the previous s-expression:

```
(def f (x y)
  (do
    (print x)
    (print y)))

(def main ()
  (do (f 1 2)))
```

I like this metasyntax, because we already structure programs with indentation, this just makes it explicit.
However based on my research, whitespace-sensitivity is very controversial!
I also get the impression that most new languages are completely insensitive to whitespace.

## B-expressions

Instead of parsing a [rose tree](https://en.wikipedia.org/wiki/Rose_tree) with parentheses, you can also parse a [binary tree](https://en.wikipedia.org/wiki/Binary_tree) with binary operators.
I call these "b-expressions".

```
f = x, y -> print x; print y.

main = () -> f 1 2
```

Everything is a binary expression `f <op> x` or an atom `x`.

- We use an "empty operator" for function application `f x`. In Haskell this is sometimes called the "whitespace operator".
- We're even using a "full-stop operator" to separate definitions, making the entire program one big binary expression.
- We're using operator precedence and associativity, which removes the need for parentheses:

```
((f = ((x , y) -> ((print x) ; (print y)))) . (main = (() -> ((f 1) 2))))
```

This syntax is well-suited for expression-oriented functional programming languages.

I really like this metasyntax, but it does have some quirks.

First of all, it really needs operator precedence, otherwise it's unwieldy.
And if we get a user-defined operator `x >>= y`, we need a precedence declaration to figure out how to parse it.
But we need to parse the program to get the precedence declaration...
This is a bit of a chicken-and-egg problem.

Also, top-level expressions are a bit awkward.
In my example I used a full-stop operator, similar to [Coq](https://learnxinyminutes.com/coq).
But unlike Coq, it's a separator and not a terminator, so the last expression can't actually end in anything.

## et al.?

What other metasyntaxes are out there?
