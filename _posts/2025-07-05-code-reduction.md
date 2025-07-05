---
layout: post
title: "Code Reduction"
published: false
---

* toc
{:toc}

---

There are lots of goals in software engineering.
Most of these are obvious, like correctness, security or performance.
While these aren't always easy to achieve, we can mostly agree on what they mean and how to measure them.

Other goals are difficult to even define.
I'm talking about things like maintainability, simplicity or "best practice".
All the human-oriented stuff.
And of course there is more disagreement here - everyone thinks differently.

The temptation is to pick one of your favourite software engineering prescriptions and declare it a best practice, because *surely* everyone can agree on this one thing.
In this post I will try really hard not to do that.

# Code Reduction

So here is my big tent that I'm hoping everyone can agree on: *simplify your code so that there's less of it*.

If your first thought is "duh", then I count that as a success.
I hope that we can agree on this as a foundation before I lose you on a secondary issue.

So what do I mean by code reduction?
Well other than "less code", basically doing any of these:

- Rewriting code to be functionally equivalent but simpler
- Removing functionality, abstractions or optimisations that [you aren't gonna need](https://en.wikipedia.org/wiki/You_aren%27t_gonna_need_it)
- Removing noise such as unnecessary comments or formatting quirks
- Removing unnecessary duplication
- Reducing the cognitive complexity of code
- Outsourcing code to a high quality library

It goes without saying - you can't apply these when they violate your other requirements.
Hopefully I'm still in obvious territory and these aren't too controversial.

I should also highlight why exactly this is a good goal:

- Simple code is simple to change
- Smaller programs are easier to understand and navigate
- Large rewrites become small rewrites
- Noise and obfuscation makes bugs harder to see
- More code means more room for bugs

But if you're reading this, you've probably worked on a massive, brittle codebase and you've learned this the hard way.

So what are some concrete steps for achieving this?
I'll start less controversial.

## [YAGNI](https://en.wikipedia.org/wiki/You_aren't_gonna_need_it)

It's been said a million times, but it keeps needing to be said: **you aren't gonna need it**.

Is that feature actually required?
Spend that time on something that *is* required.

Is that "scalable code" necessary?
Write the simple version first.
When you do need to scale, your simple version will be much easier to adapt.

Is that abstraction needed?
How much complexity did you pay for it?

## Quit the noise

Code is a written medium.
What you put in version control will be printed on everyone's screen, forever.
We don't need to see that commented-out code.

Also, consider what comments are actually useful.
Comments aren't executed - in that sense, they are noise.
Sometimes the noise says something useful like "here's how this works", but it is still noise.
Before you write a comment, try to encode that comment in variable names.

Use a formatter.
Formatting quirks trip the mind and introduce friction.
Minimise surprise.

## Equational reasoning

So how do you actually write simpler code?
For this I would like to introduce the philosophy of *equational reasoning*.
This is a term celebrated primarily by the Haskell community, and it basically means simplifying mathematical expressions.

The idea is to apply mathematical thinking to programming.
We take a program, look at its parts, and find ways to reduce it down to simpler terms.

## Functional programming

## Types
