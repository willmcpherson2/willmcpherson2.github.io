---
layout: post
title: "Immutable JavaScript or: How I Learned to Stop Worrying and Love Shallow Copies"
---

Objects are the best and worst part of JavaScript.

The best:

1.  Objects are general-purpose and can be used in many different ways:
    - Simple key-value stores `{ alice: 1, bob: 2 }`
    - Arrays `[1, 2]`
    - Algebraic data types `{ type: "wizard", points: 1 }`
    - OOP objects `{ name: "alice", points: 1, addPoint() { this.points += 1; } }`
2.  Objects have a plethora of ergonomic features:
    - Dot notation `player1.points`
    - Bracket notation `players["alice"]`
    - Indexing `players[0]`
    - Spread syntax `{ ...player1, points: 0 }`
    - Computed property names `{ ...players, [getPlayer()]: 0 }`
    - Methods `{ getPoints() { return this.points; } }`
3.  Objects work incredibly well with TypeScript:
    - Keyof operator `keyof Player`
    - Indexed access types `Player["points"]`
    - Mapped types `{ [key: string]: number }`
    - Intersection types `{ name: string } & { points: number }`
    - The Extract type `Extract<Player, { type: "wizard" }>`
    - Read-only types `Readonly<Player>`

*The worst*:

1. Objects are mutable
2. Objects are references

For example:

```js
const addPoint = player => {
  player.points += 1;
  return player;
}

const player1 = { points: 1 };

// OK, we declared it like that
console.log(player1); // { points: 1 }

// OK, this is intended by the function
console.log(addPoint(player1)); // { points: 2 }

// !?
console.log(player1); // { points: 2 }
```

What's wrong with this?

- Functions give results by returning **and** mutating. This is a design flaw that results in awkward functions and inconsistent APIs.
- Mutation doesn't compose. For example: `addPoint(addPoint(player1))`. If `addPoint` just mutates its argument, this doesn't work. If `addPoint` mutates *and* returns, then we have one of those awkward functions. But if `addPoint` is pure, composition is effortless.
- Our old information is destroyed. If we want to access the previous state of our object, we have to defensively copy it. In JavaScript, this is no fun.[^1]
- Mutation gets harder to track as control flow becomes more complex. Mutating an object is fine as a quick hack, but eventually that object gets shared around, the call stack grows and things become very difficult to reason about.

# What if objects were not references?

Without references, you get "pass by value". All objects would be copied. This is already the case for primitives. However, there is no compiler flag to achieve this. Calling `copy()` everywhere would be cumbersome and inefficient.

# What if we removed mutability?

If you remove mutability, you get "pass by read-only reference". `player.points += 1` would not be valid and you would need to use `return { points: player.points + 1 }`.

Surprisingly, you are totally free to just never mutate an object. It may sound unnatural, but it works just like in any other immutable language.

We've avoided *the worst* by removing mutation. Objects are still references, but this no longer matters. Shallow copies work now. For example:

```js
const player1 = { points: 1, levels: { magic: 1 } };
const player2 = { ...player1, points: player1.points + 1 };
```

The depth of the copy makes no difference here because the nested object will never be mutated. That is unless you use `===`, which is a topic for another day.

Can we actually write JavaScript like this? I want to argue that it's possible in many cases. And with the right tools, it's actually much better than impure JavaScript.

## fp-ts

[fp-ts](https://github.com/gcanti/fp-ts) is a standard-esque library for typed functional programming. If you're going to write immutable JavaScript, you can't rely on the builtin methods. For example, the [Array API](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array) provides a mix of pure and impure methods. Fortunately the [fp-ts Array module](https://gcanti.github.io/fp-ts/modules/Array.ts.html) is totally pure and provides much more functionality.

## ts-pattern

[ts-pattern](https://github.com/gvergnaud/ts-pattern) is an exhaustive pattern matching library. Pattern matching is the key to writing pure functions and ts-pattern even rivals languages that have it built-in.

## monocle-ts

[monocle-ts](https://github.com/gcanti/monocle-ts) is a functional optics library. It allows you to perform composable nested updates. However, I found that for most use cases, spread syntax is sufficient.

## React

As you'd expect, [React](https://react.dev/) works very well with immutable programming.

## eslint-plugin-functional

[eslint-plugin-functional](https://github.com/eslint-functional/eslint-plugin-functional#no-mutations) is an ESLint plugin to disable mutation. I haven't tried it yet but I love the idea.

# Show me the code

I just published [Letscape](https://github.com/willmcpherson2/letscape) which is about 1,500 lines of TypeScript. It's a React application that doesn't use `let` or `obj.a = b` anywhere, and uses shallow copies everywhere.

# Performance

I haven't done any testing, but I would note that since we're mostly doing shallow copies, we're taking advantage of the fact that objects are references. Most of the time, we're transforming data by doing something like `{ ...old, new }`, which isn't too expensive.

If JavaScript was pure from the start, engines and compilers would be able to take advantage of this. Unfortunately, in languages that allow mutation, you don't get any of the performance benefits when you opt out.

# Footnotes

[^1]: There are a few ways to copy JavaScript objects:

    - Perform shallow copies via spread syntax `{ ...player }` and hope that you've prevented undesirable mutations
    - `JSON.parse(JSON.stringify(player))`, which is slow and relies on serialisation which prevents copying functions
    - `structuredClone(player)`, which is faster but also only works for serialisable objects

    Obviously, JavaScript was just never built to support copying. Even if there was some easy way to copy objects, it's still up to the programmer to ensure that copies and mutations happen in the right places.
