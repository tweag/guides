Code review checklist
=====================

When reviewing Haskell code, do watch out for the following points.

Copyright notices at the top of every module
--------------------------------------------

```Haskell
-- |
-- Copyright: (C) 2014 EURL Tweag
--
-- Module description
--

{-# LANGUAGE    ... #-}
{-# OPTIONS_GHC ... #-}
module M where

...
```
Use *exactly one* `LANGUAGE` pragma per extension.

Inverted boolean conditions
---------------------------

Functions like
```Haskell
filter :: (a->Bool) -> [a] -> [a]
break :: (a->Bool) -> [a] -> ([a],[a])
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

Boundaries of enumerations and recursive functions
--------------------------------------------------

The limits in expressions of the form `[x,x-1..y]`, `[x..]`, `[x..y]`
must be checked to be correct.

Termination of recursive functions
----------------------------------

Check that all recursive functions terminate (well-foundedness). The
arguments of each recursive call must ensure progress towards the base
case, and the base case has to be defined!

Race conditions solved by locks
-------------------------------

When MVars are used as a locking mechanisms it should be checked that
the resources for which the `MVar` is intended are really protected,
and furthermore that such protection is not redundant with other
locks/MVars.

Unrevertable changes under MVar
-------------------------------

When an exception is thrown during an MVar action (`modifyMVar`,
`withMVar`..) the MVar is reverted to its previous state. However,
this behaviour may lead to an inconsistent state in the presence of
other side effects, e.g. network actions or changes in other mutable
variables.

Laziness when copying non-garbage-collected data
------------------------------------------------

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

`foldl`: just say no - use `foldl'`
-----------------------------------

`foldl` is approximately *never* appropriate. See
[here][well-typed-foldl] for a longer discussion. Executive summary:
`foldl f z xs` leads to space leaks, even when `f` is strict in both
arguments.

[well-typed-foldl]: http://www.well-typed.com/blog/90/

Use strict versions of containers where appropriate
---------------------------------------------------

In most cases strict version of containers (e.g. `Data.Map.Strict`) are 
prefered over the lazy version - they perform better in most cases and
make space leaks less likely.

List prefix memory leaks
------------------------

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
Or use `seqList` from the `parallel` package.

Names of intermediate computations
----------------------------------

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
