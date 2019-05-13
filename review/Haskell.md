Code review checklist
=====================

When reviewing Haskell code, do watch out for the following points.

### Copyright notices at the top of every module

If required by the product owner, check for a copyright notice.

```Haskell
-- |
-- Copyright: (C) <YEARS> <COPYRIGHT_HOLDER>
--
-- Module description

{-# LANGUAGE ... #-}
{-# OPTIONS_GHC ... #-}
module M where

...
```

### Inverted boolean conditions

Functions like
```Haskell
filter :: (a -> Bool) -> [a] -> [a]
break :: (a -> Bool) -> [a] -> ([a],[a])
when :: Monad m => Bool -> m ()
or
and
if-then-else
```
take a boolean predicate. Ensure the boolean predicate must not compute
the opposite of what's intended. For instance, if the intention is to
remove from a list all the occurrences of 0, a wrong solution is
`filter (==0) [0,1,0,1]` the correct one being
`filter (/=0) [0,1,0,1]`.

### Boundaries of enumerations and recursive functions

The limits in expressions of the form `[x,x-1..y]`, `[x..]`, `[x..y]`
must be checked to be correct.

### Termination of recursive functions

Check that all recursive functions terminate (well-foundedness). The
arguments of each recursive call must ensure progress towards the base
case, and the base case has to be defined!

### Race conditions solved by locks

When `MVar`s are used as a locking mechanisms it should be checked that
the resources for which the `MVar` is intended are really protected,
and furthermore that such protection is not redundant with other
locks/`MVar`s.

### Unrevertable changes under `MVar`

When an exception is thrown during an `MVar` action (`modifyMVar`,
`withMVar`..) the `MVar` is reverted to its previous state. However,
this behaviour may lead to an inconsistent state in the presence of
other side effects, e.g. network actions or changes in other mutable
variables.

### Laziness when copying non-garbage-collected data

If the program manipulates memory which is released regardless of the
references that exist to it, care must be taken to ensure that
references do not exist after the memory is released.

This happens often in Haskell bindings to OS or hardware resources
which the garbage collector does not manage. Say we have a list of
`bytestrings xs :: [ByteString]` where the memory pointed by the
bytestrings can be released before the bytestrings are garbage
collected. Attempting to copy the data before the memory is released is
not well done with `map copy xs` since it defers copying until the
result of the map is needed. The good solution would be more strict:
`mapM (evaluate . copy) xs`.

### `foldl`: just say no - use `foldl'`

`foldl` is approximately *never* appropriate. See
[here][well-typed-foldl] for a longer discussion. Executive summary:
`foldl f z xs` leads to space leaks, even when `f` is strict in both
arguments.

[well-typed-foldl]: http://www.well-typed.com/blog/90/

### Use strict versions of containers where appropriate

In most cases strict version of containers (e.g. `Data.Map.Strict`) are
prefered over the lazy version - they perform better in most cases and
make space leaks less likely.

### List prefix memory leaks

Evaluating say
```Haskell
case take n xs of
  (_:xs') -> ... xs' ...
```
may keep all of `xs` in memory until the end of `xs'` is evaluated.

This memory leak can be fixed with
```Haskell
case Control.DeepSeq.force $ take n xs of
  (_:xs') -> ... xs' ...
```
though the above also forces evaluation of the list elements, which is
often overkill. The following can be enough:
```Haskell
forceSpine xs = length xs `seq` xs

case forceSpine $ take n xs of
  (_:xs') -> ... xs' ...
```
Or use `seqList` from the `parallel` package. Or better yet, use
a strict list datatype rather than the standard lazy one.

### Names of intermediate computations

When reviewing code like
```Haskell
let  x' = f x
    x'' = g x'
 in x''
```
ensure that the arguments given to the intermediate computations `f`
and `g` are the intended ones. Thus we could catch uses of, say, `x`
where `x'` was intended.

*Note:* the above is bad style. Such code should be written
pointlessly, with `f` and `g` chained. Or pointfully, but then with
meaningful names for the intermediate state rather than chains of
apostrophes.

### Catamorphisms

Use of catamorphisms (e.g. `foldr`, `foldl`) and other special case
higher-order functions (such as `map`) is preferable to writing
structurally recursive functions with explicit recursion. This is
because a fold makes it a type error to forget the base case -
a problem with the code that could otherwise go undetected. It also
documents the fact that the function is supposed to be structurally
recursive, as opposed to functions that use general recursion.

### Data types

Constructor fields should be strict, unless there's an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

```Haskell
-- Good
data Point = Point
  { pointX :: !Double  -- ^ X coordinate
  , pointY :: !Double  -- ^ Y coordinate
  }

-- Bad
data Point = Point
  { pointX :: Double  -- ^ X coordinate
  , pointY :: Double  -- ^ Y coordinate
  }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```Haskell
data Point = Point
  { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
  , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
  }
```

### Functions

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```Haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```
