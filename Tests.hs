module Tests where

-- GHC
import System.Exit
import Data.Char (isPunctuation)

-- External
import Test.HUnit

-- Lib
import Problem1 (wordsPerLine, averageCharsPerLine, llDiffFromAvg)

sixThirtyTwo, gettysberg :: String

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

gettysberg =
    "Four score and seven years ago our fathers brought forth on this continent, a\n\
    \new nation, conceived in Liberty, and dedicated to the proposition that all men\n\
    \are created equal. \n\
    \\n\
    \Now we are engaged in a great civil war, testing whether that nation, or any\n\
    \nation so conceived and so dedicated, can long endure. We are met on a great\n\
    \battle-field of that war. We have come to dedicate a portion of that field, as\n\
    \a final resting place for those who here gave their lives that that nation might\n\
    \live. It is altogether fitting and proper that we should do this. \n\
    \\n\
    \But, in a larger sense, we can not dedicate -- we can not consecrate -- we can\n\
    \not hallow -- this ground. The brave men, living and dead, who struggled here,\n\
    \have consecrated it, far above our poor power to add or detract. The world will\n\
    \little note, nor long remember what we say here, but it can never forget what\n\
    \they did here. It is for us the living, rather, to be dedicated here to the\n\
    \unfinished work which they who fought here have thus far so nobly advanced. It\n\
    \is rather for us to be here dedicated to the great task remaining before us--\n\
    \that from these honored dead we take increased devotion to that cause for which\n\
    \they gave the last full measure of devotion--that we here highly resolve that\n\
    \these dead shall not have died in vain--that this nation, under God, shall have\n\
    \a new birth of freedom--and that government of the people, by the people, for\n\
    \the people, shall not perish from the earth."

p11, p12, p13 :: Test


p11 = "p1-1" ~: test [ wordsPerLine "This\nis a\ntest\n\n" @?= [1,2,1]
                     , wordsPerLine sixThirtyTwo @?= [7,6,6,5,7,6,6,4,8,6,7,4]
                     , wordsPerLine gettysberg @?= [14,13,3,15,15,15,15,12,17,14,15,15,16,14,15,14,13,14,14,8]]

p12 = "p1-2" ~: test [ averageCharsPerLine "This\nis a second\ntest\n\n" @?= 19 / 3
                     , averageCharsPerLine sixThirtyTwo @?= 29.5
                     , averageCharsPerLine gettysberg @?= 72.45 ]

compareFloating :: Double -> Double -> Double -> Bool
compareFloating eps x y = abs (x - y) < eps

compareFloatingList :: Double -> [Double] -> [Double] -> Bool
compareFloatingList eps xs ys = and (zipWith (compareFloating eps) xs ys)

p13 = "p1-3" ~: test [ llDiffFromAvg "This\nis a\ntest\n\n" @?= [0, 0, 0]
                     , llDiffFromAvg sixThirtyTwo @?= [3.5,2.5,0.5,2.5,3.5,0.5,0.5,5.5,5.5,0.5,3.5,6.5]
                     , assert $ compareFloatingList 0.01 (llDiffFromAvg gettysberg)
                                   [4.55,6.55,53.45,3.55,3.55,5.55,7.55,6.45,5.55,5.55
                                   ,6.55,4.55,2.55,5.55,4.55,6.55,4.55,6.55,4.55,28.45] ]

tests = test [p11, p12, p13]

main :: IO ()
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
