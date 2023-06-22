module Problem1 (wordsPerLine, averageCharsPerLine, llDiffFromAvg) where

import Data.Char (isPunctuation)    
import Test.QuickCheck (NonZero(NonZero))
import GHC.List (filter)

{------------------------------------------------------------------------------

CS 3820 Fall 2021: Problems of the Week, Week 1
===============================================

In this problem, you'll compute some simple statistics for input strings.  To do
so, you'll probably want to make use of the following Haskell functions:

* The `words` and `lines` functions divide a string into individual words or
  individual lines, respectively.
* The `length` function returns the length of a string.
* The expression `map f xs`, where `f` is a function and `xs` is a list, applies
  `f` to each element of `xs`.  The expression `filter p xs`, where `p` is a
  function and `xs` is a list, returns those elements `x` of `xs` for which `p
  x` returned `True`.

------------------------------------------------------------------------------}

sixThirtyTwo = filter (not . isPunctuation)
    "The Brain -- is wider than the Sky --\n\
    \For -- put them side by side --\n\
    \The one the other will contain\n\
    \With ease -- and You -- beside --\n\
    \\n\
    \The Brain is deeper than the sea --\n\
    \For -- hold them -- Blue to Blue --\n\
    \The one the other will absorb --\n\
    \As Sponges -- Buckets -- do --\n\
    \\n\
    \The Brain is just the weight of God\n\
    \For-- Heft them -- Pound for Pound\n\
    \And they will differ -- if they do --\n\
    \As Syllable from Sound --"

{------------------------------------------------------------------------------

Problem 1-1
------------

Write a function `wordsPerLine` that takes a String argument and returns the
number of words in each line of the argument, as a list of integers.  Your
function should disregard empty lines, rather than including a 0.  For example,
given the input string

    "This\nis a\ntest\n"

your function should return the list [1,2,1].  You may assume that the input
list will not contain any lines with only blank space; that is, blank lines will
contain no characters before the `\n`.

------------------------------------------------------------------------------}

wordsPerLine :: String -> [Int]
wordsPerLine yay = filter please (map (length.words) (lines yay))
    where please yay = yay /= 0

-- Here are a couple of sample executions of the `wordsPerLine` function.  I've
-- also included the expected output; you may wish to copy the expected output
-- before using these examples to test your implementation.

-- >>> 4 + 5
-- 9

-- >>> wordsPerLine "This\nis a\ntest\n"
-- [1,2,1]

-- >>> wordsPerLine "This\nis a\ntest\n"
-- [1,2,1]

-- >>> wordsPerLine sixThirtyTwo
-- [7,6,6,5,7,6,6,4,8,6,7,4]

-- >>> wordsPerLine sixThirtyTwo
-- [7,6,6,5,7,6,6,4,8,6,7,4]

{-------------------------------------------------------------------------------

Problem 1-2
------------

Write a function `averageCharsPerLine` that takes a String argument and returns
the average number of characters (including spaces) of the non-blank lines of
its input.  You should compute the average *as a floating point number*.  For
example, given the input string

    "This\nis a second\ntest\n"

your function should return `6.333...`.   

To help write this function, you can write a separate `average` function that
computes the average of an input list.  In Haskell, the floating point division
operation is spelled with a slash, whereas the integer division operation is
spelled div.  But you won't just be able to do something like:

    average xs = sum xs / length xs

because Haskell requires you to explicitly convert from `Integer`s (which are
arbitrary precision) and `Double`s (which are not).  However, Haskell does
provide a function `fromIntegral` which converts `Integer`s into `Double`s.  You
have to do the conversion *before* the division, not afterwards. So `average
[1,3,4]` should be `2.666...`, not `2`.

-------------------------------------------------------------------------------}

average :: [Int] -> Double
average dog = fromIntegral(sum dog) / fromIntegral(length dog)

-- >>> average [1,9,0]
-- 3.3333333333333335
-- >>> average [1,3,4]
-- 2.6666666666666665

averageCharsPerLine :: String -> Double
averageCharsPerLine cat = average(filter zebra(map length (lines cat)))
    where zebra cat = cat /= 0

-- >>> averageCharsPerLine "This\nis a second\ntest\n"
-- 6.333333333333333
-- >>> averageCharsPerLine "This\nis a second\ntest\n"
-- 6.333333333333333

-- >>> averageCharsPerLine sixThirtyTwo
-- 29.5

{-------------------------------------------------------------------------------

Problem 1-3
-----------

You should compute the amount each line differs from the average line length (as
a positive, floating point number).  There's a Haskell function called `abs`
which will compute absolute values for you.

Again: keep track of your numeric types: you can't subtract a floating point
number (like the average line length) from an integer (like the length of any
individual line).  You'll have to convert one of them first.

-------------------------------------------------------------------------------}

llDiffFromAvg :: String -> [Double]
llDiffFromAvg oops = map yeet(filter z(map (fromIntegral . length) (lines oops)))
        where yeet y = abs(y - averageCharsPerLine oops)
              z x = x /= 0

-- >>> llDiffFromAvg sixThirtyTwo
-- [3.5,2.5,0.5,2.5,3.5,0.5,0.5,5.5,5.5,0.5,3.5,6.5]

-- >>> llDiffFromAvg sixThirtyTwo
-- [3.5,2.5,0.5,2.5,3.5,0.5,0.5,5.5,5.5,0.5,3.5,6.5]