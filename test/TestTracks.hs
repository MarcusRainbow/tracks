module TestTracks (tracksTests) where

import Test.HUnit
import Tracks

tracksTests :: [Test]
tracksTests = [
    testValidOptions,
    testRowOptionsFirstRowOneSpace,
    testRowOptionsFirstRowZeroSpaces,
    testRowOptionsFirstRowTwoSpaces,
    testRowOptionsFirstRowOneSpaceForceThird,
    testRowOptionsFirstRowOneSpaceForceMid,
    testRowOptionsFirstRowOneSpaceForceIllegalExit,
    testRowOptionsFirstRowTwoSpacesForceExit,
    testGridOptionsSimple]

testValidOptions :: Test
testValidOptions = 
    TestCase $ assertEqual "╚ should be valid before ╝"
    "╚" (validOptions "╝" "" '╚')

testRowOptionsFirstRowOneSpace :: Test
testRowOptionsFirstRowOneSpace = 
    TestCase $ assertEqual "A first row with one space has two options"
    ["╚╝ ", " ╚╝"] (rowOptions "---" "" 1)

testRowOptionsFirstRowZeroSpaces :: Test
testRowOptionsFirstRowZeroSpaces = 
    TestCase $ assertEqual "A first row with no spaces has one option"
    ["╚═╝"] (rowOptions "---" "" 0)

testRowOptionsFirstRowTwoSpaces :: Test
testRowOptionsFirstRowTwoSpaces = 
    TestCase $ assertEqual "A first row with two spaces is not possible"
    [] (rowOptions "---" "" 2)

testRowOptionsFirstRowThreeSpaces :: Test
testRowOptionsFirstRowThreeSpaces = 
    TestCase $ assertEqual "A first row with three spaces has one option"
    ["   "] (rowOptions "---" "" 3)

testRowOptionsFirstRowOneSpaceForceThird :: Test
testRowOptionsFirstRowOneSpaceForceThird = 
    TestCase $ assertEqual "Forcing one of the two legal options"
    [" ╚╝"] (rowOptions "--╝" "" 1)

testRowOptionsFirstRowOneSpaceForceMid :: Test
testRowOptionsFirstRowOneSpaceForceMid = 
    TestCase $ assertEqual "Forcing the second of the two legal options"
    ["╚╝ "] (rowOptions "-╝-" "" 1)

testRowOptionsFirstRowOneSpaceForceIllegalExit :: Test
testRowOptionsFirstRowOneSpaceForceIllegalExit = 
    TestCase $ assertEqual "An exit with no legal solutions"
    [] (rowOptions "╝--" "" 1)

testRowOptionsFirstRowTwoSpacesForceExit :: Test
testRowOptionsFirstRowTwoSpacesForceExit = 
    TestCase $ assertEqual "An exit with one legal solution"
    ["╝  "] (rowOptions "╝--" "" 2)

testGridOptionsSimple :: Test
testGridOptionsSimple =
    TestCase $ assertEqual "A small grid with two options"
    [["╔╗ ",
      "║║ ",
      "╚╝ "],
     [" ╔╗",
      " ║║",
      " ╚╝"]]
    (gridOptions ["---", "---", "---"] [1, 1, 1])
