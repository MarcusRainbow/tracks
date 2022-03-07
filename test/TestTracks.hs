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
    testGridOptionsSimple,
    testGridOptionsPartFixed,
    testGridOptionsEntryExit,
    testGridOptionsTimesMarch5,
    testSolveEntryExit,
    testSolveTimesMarch5]

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

testGridOptionsPartFixed :: Test
testGridOptionsPartFixed =
    TestCase $ assertEqual "A small grid with only one solution"
    [[" ╔╗",
      " ║║",
      " ╚╝"]]
    (gridOptions 
     ["-╔-",
      "--║", 
      "---"] 
     [1, 1, 1])

testGridOptionsEntryExit :: Test
testGridOptionsEntryExit =
    TestCase $ assertEqual "A small grid with entry and exit"
    [[" ╔╝",
      " ║ ",
      "╔╝ "]]
    (gridOptions 
    ["--╝",
     "---", 
     "╔--"] 
     [1, 2, 1])

testGridOptionsTimesMarch5 :: Test
testGridOptionsTimesMarch5 =
    TestCase $ assertEqual "Puzzle from the Times March 5"
    192
    (length $ gridOptions 
    ["--------",
     "----╝---",
     "--------",
     "--------",
     "-----║--",
     "--------",
     "--------",
     "╝------║"]
     [5, 5, 5, 3, 4, 4, 4, 6])

testSolveEntryExit :: Test
testSolveEntryExit =
    TestCase $ assertEqual "A small grid with entry and exit"
    [[" ╔╝",
      " ║ ",
      "╔╝ "]]
    (solve 
    ["--╝",
     "---", 
     "╔--"] 
     [2, 1, 2]
     [1, 3, 1])

testSolveTimesMarch5 :: Test
testSolveTimesMarch5 =
    TestCase $ assertEqual "Puzzle from the Times March 5"
    [["    ╔═╗ ",
      "   ╔╝ ║ ",
      "   ║ ╔╝ ",
      "╔═╗║ ║  ",
      "║ ╚╝ ║  ",
      "║    ╚╗ ",
      "║     ╚╗",
      "╝      ║"]]
    (solve 
    ["--------",
     "----╝---",
     "--------",
     "--------",
     "-----║--",
     "--------",
     "--------",
     "╝------║"]
     [3, 3, 3, 5, 4, 3, 3, 2]
     [5, 1, 2, 4, 2, 5, 5, 2])
