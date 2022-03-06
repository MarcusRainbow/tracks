module TestTracks (tracksTests) where

import Test.HUnit
import Tracks

tracksTests :: [Test]
tracksTests = [
    testRowOptionsFirstRowOneSpace]

testRowOptionsFirstRowOneSpace :: Test
testRowOptionsFirstRowOneSpace = 
    TestCase $ assertEqual "A first row with one space has two options"
    [" ╚╝", "╚╝ "] (rowOptions "---" "   " "" 1 False)
