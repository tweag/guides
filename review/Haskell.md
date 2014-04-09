Code review checklist
=====================

check for inverted boolean conditions 
-------------------------------------

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

Check for boundaries of enumerations and recursive functions
------------------------------------------------------------

The limits in expressions of the form `[x,x-1..y]`, `[x..]`, `[x..y]`
must be checked to be correct.

Termination of recursive functions
----------------------------------

Check that all recursive functions terminate (well-foundedness). The
arguments of each recursive call must ensure progress towards the base
case, and the base case has to be defined!

Check the race conditions solved by locks
-----------------------------------------

When MVars are used as a locking mechanisms it should be checked that
the resources for which the `MVar` is intended are really protected,
and furthermore that such protection is not redundant with other
locks/MVars.

Check for laziness when copying non-garbage-collected data
----------------------------------------------------------

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

Check for copyright notices at the top of every module
------------------------------------------------------

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

Note that the character `|` needs to be dropped if the file does not
start with `module`.

```Haskell
-- Copyright: (C) 2014 EURL Tweag
--
-- File description
--

{-# LANGUAGE    ... #-}
{-# OPTIONS_GHC ... #-}
import ...

...
```

Consider using `foldl'` when using `foldl`
------------------------------------------

foldl' could keep the accumulator parameter small sometimes by
evaluating it to WHNF.


Look for memory leaks when writing pure code
--------------------------------------------

For instance, evaluating
```Haskell
case take n xs of
  (_:xss) -> ... xss ...
```
would retain a potentially long
list `xs`.

Fully evaluating `take 1 xs` would require writing either using
```Haskell
case Control.DeepSeq.force $ take n xs of
  (_:xss) -> ... xss ...
```
or using a function like
```Haskell
seqSpine :: [a] -> b -> b
seqSpine     [] = id
seqSpine (_:xs) = seqSpine xs

forceSpine xs = seqSpine xs xs

case force (take n xs) of
  (_:xss) -> ... xss ...
```

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

