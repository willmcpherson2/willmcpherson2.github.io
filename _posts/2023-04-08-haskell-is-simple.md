---
title: "Haskell is simple"
---

* toc
{:toc}

---

Haskell is a large language – it has a complex syntax, an expressive type system and a long list of extensions. But I would argue that it's far simpler than almost all other popular programming languages.

# Primitive Power

A good definition of simplicity is *how much you get out of the primitives*. If there are too many primitives, the language becomes too complex. If the primitives don't provide enough power, the code written in the language becomes too complex.

Using just functions and algebraic data types, let's see what we can implement.

# Booleans

Sum types are mysteriously neglected in many languages. This has resulted in all sorts of superfluous primitives. The most painfully obvious being the boolean.

```haskell
import Prelude hiding (Bool (..), not)

data Bool = False | True
  deriving (Show)

not :: Bool -> Bool
not x = case x of
  True -> False
  False -> True

main :: IO ()
main = print $ not True
```

```haskell
False
```

# Numbers [^1]

Natural numbers (unsigned integers) are just an inductive sum type.

```haskell
import Prelude hiding ((+))

data Nat = Z | S Nat
  deriving (Show)

(+) :: Nat -> Nat -> Nat
n + m = case n of
  Z -> m
  S n -> S (n + m)

zero, one, two :: Nat
zero = Z
one = S zero
two = S one

main :: IO ()
main = print $ two + two
```

```haskell
S (S (S (S Z)))
```

# Null

A lack of sum types leads many languages to introduce `null`.

```haskell
import Prelude hiding (Maybe (..), map)

data Maybe a = Nothing | Just a
  deriving (Show)

map :: (a -> b) -> Maybe a -> Maybe b
map f x = case x of
  Just x -> Just (f x)
  Nothing -> Nothing

main :: IO ()
main = print $ map (+ 1) (Just 1)
```

```haskell
Just 2
```

# Lists

Many languages either have built-in lists or pointer-juggling implementations of lists. In Haskell, a list is a simple recursive data type.

```haskell
import Prelude hiding (map)

data List a = Nil | a :> List a
  deriving (Show)

infixr 5 :>

map :: (a -> b) -> List a -> List b
map f x = case x of
  x :> xs -> f x :> map f xs
  Nil -> Nil

main :: IO ()
main = print $ map (+ 1) (1 :> 2 :> 3 :> Nil)
```

```haskell
2 :> (3 :> (4 :> Nil))
```

# Mutation

Mutation can be implemented by returning the "mutated" value. Importantly, these operations can be composed, so this scales up.

```haskell
data Direction = U | D | L | R

type Route = [Direction]

type Position = (Int, Int)

move :: Direction -> Position -> Position
move direction (x, y) = case direction of
  U -> (x, y + 1)
  D -> (x, y - 1)
  L -> (x - 1, y)
  R -> (x + 1, y)

travel :: Position -> Route -> Position
travel = foldr move

main :: IO ()
main = print $ travel (0, 0) [U, U, R, R]
```

```haskell
(2,2)
```

# References

References can be implemented with lazy values.

In this example, each `Cell` has a number and a reference to the `World` which also has a number. The `updateCell` function updates a cell's number by adding the world number.

Notice how `world = World 100 [Cell 1 world, Cell 2 world]` naturally creates circular references.

```haskell
data World = World {worldNum :: Int, cells :: [Cell]}

data Cell = Cell {cellNum :: Int, world :: World}

updateWorld :: World -> World
updateWorld world = world {cells = map updateCell (cells world)}

updateCell :: Cell -> Cell
updateCell cell = cell {cellNum = cellNum cell + worldNum (world cell)}

getCellNums :: World -> [Int]
getCellNums world = map cellNum (cells world)

main :: IO ()
main =
  let world = World 100 [Cell 1 world, Cell 2 world]
      world' = updateWorld world
   in print $ getCellNums world'
```

```haskell
[101,102]
```

# Conditionals

Conditionals can be implemented by combining sum types and lazy evaluation.

```haskell
cond :: Bool -> a -> a -> a
cond x a b = case x of
  True -> a
  False -> b

main :: IO ()
main = print $ cond (2 > 1) "math works" "math is broken"
```

```haskell
"math works"
```

This is even better than a regular `if` builtin because it's a function that can be partially applied, composed and passed around.[^2]

However, booleans and conditionals are not needed as much in Haskell since you can define your own sum types.

# Loops

Loops can be implemented by combining conditionals and recursion.

We can implement `for` if we really want:

```haskell
for :: Int -> (Int -> Int) -> (Int -> Bool) -> a -> (a -> a) -> a
for n increment continue x body =
  if continue n
    then for (increment n) increment continue (body x) body
    else x

main :: IO ()
main = print $ for 0 (+ 1) (< 10) [1, 1] $ \xs@(x : y : _) -> (x + y) : xs
```

```haskell
[144,89,55,34,21,13,8,5,3,2,1,1]
```

However, you would never use anything like this. Haskell offers much more powerful higher-order recursive functions such as `map`, `foldr`, `iterate` and countless others.

# Functional Programming [^3] [^4]

But did you know that you don't even need built-in algebraic data types? You can implement them with functions.

Here's a sum type and a product type:

```haskell
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (Bool, not)

newtype Bool = Bool (forall r. r -> r -> r)

true :: Bool
true = Bool $ \x _ -> x

false :: Bool
false = Bool $ \_ y -> y

instance Show Bool where
  show (Bool x) = x "true" "false"

not :: Bool -> Bool
not (Bool x) = x false true

newtype Pair a b = Pair (forall r. (a -> b -> r) -> r)

pair :: a -> b -> Pair a b
pair x y = Pair $ \f -> f x y

instance (Show a, Show b) => Show (Pair a b) where
  show (Pair p) = p $ \x y -> "pair " <> show x <> " " <> show y

fst :: Pair a b -> a
fst (Pair p) = p $ \x _ -> x

main :: IO ()
main = print $ pair true (not true)
```

```haskell
pair true false
```

This hopefully makes it more clear how constructors and case expressions are functions and applications at heart.

To me, this is what "functional programming" means – programming with functions, even if they are hidden behind syntactic sugar.

{% include links.html %}

# Footnotes

[^1]: This is the least practical of the examples, due to both performance issues and syntactic load. This is more for demonstrational purposes. However, I believe with the right metaprogramming features and compiler optimisations, it could be possible to implement ergonomic and efficient integers as a library. Agda achieves this somewhat with [pragmas](https://agda.readthedocs.io/en/latest/language/built-ins.html#natural-numbers).

[^2]: Check out [`bool` from `Data.Bool`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Bool.html#v:bool).

[^3]: For these examples I enable [`RankNTypes`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/rank_polymorphism.html) which is enabled by default in [GHC2021](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html).

[^4]: These [`Show`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Text-Show.html) instances are not strictly legal because the string returned by `show` should only contain the constructors defined in the data type.
