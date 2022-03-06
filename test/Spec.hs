
import TestTracks
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList (
    tracksTests)