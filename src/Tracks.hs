module Tracks (
    rowOptions,
    validOptions
    ) where

{- | The tracks module solves track problems such as the ones in newspapers
where you have to find the path through a rectangular space where only some
track pieces are supplied, and the number of non-empty track pieces on each
row and column is specified.

Here, we represent tracks as two dimensional grids of unicode box-drawing
characters, for clarity and ease of printing. Empty pieces are represented
as spaces, and non-empty pieces are selected from "═║╔╗╚╝". Where appropriate,
unknown pieces are represented as a hyphen "-".

Each row of the grid is represented by a string, and the grid as a whole is
a list of strings. Where there are multiple possibilities, these are also
represented as a list.

Translation when unicode is escaped:
"═║" = "\9552\9553"
"╔╗" = "\9556\9559"
"╚╝" = "\9562\9565"
-}

type Cell = Char
type Row = String
type Grid = [Row]
type Counts = [Int]

{- | The top-level function takes a puzzle and returns a list of possible
solutions. If there are no solutions, empty list is returned. If the input
data is inconsistent (mismatching grid or counts) an error is returned.
Unlike some such puzzles, solutions containing loops are acceptable
-}
-- solve :: Grid -> Counts -> Counts -> Either String [Grid]

{- | For an unknown cell, return a list of possible contents. We pass in flags
to say whether there is a path from the cell to the South and East.
-}
cellOptions :: Bool -> Bool -> [Cell]
cellOptions False False = " ╝"
cellOptions False True  = "═╚"
cellOptions True  False = "║╗"
cellOptions True  True  = "╔"

{- | Says whether a cell has a path to the North 
-}
hasNorth :: Cell -> Bool
hasNorth c = c `elem` "║╚╝"

{- | Says whether a cell has a path to the West
-}
hasWest :: Cell -> Bool
hasWest c = c `elem` "═╗╝"

{- | Says whether a cell has a path to the South 
-}
hasSouth :: Cell -> Bool
hasSouth c = c `elem` "║╔╗"

{- | Says whether a cell has a path to the East
-}
hasEast :: Cell -> Bool
hasEast c = c `elem` "═╔╚"

{- | Given the fixed cells, the previous row, the current row to date, and
a count of available spaces, generate a list of possible complete rows
-}
rowOptions :: Row -> Row -> Row -> Int -> [Row]
rowOptions fix prev curr spaces =
    rowOptions' (reverse fix) (reverse prev) curr spaces False

{- | Like rowOptions, but taking a flag to say whether the previously
inserted item (head of curr) was forced by being a member of fix. Also,
takes the fixed and prev lists in reverse order, so they can be
accessed efficiently when constructing solutions from the right with
foldl.
-}
rowOptions' :: Row -> Row -> Row -> Int -> Bool -> [Row]
rowOptions' [] [] [] 0 _ = [] -- propagate null solution
rowOptions' [] [] (c:cs) 0 force = if (force || not (hasWest c)) then [(c:cs)] else [] 
rowOptions' [] [] _ _ _ = [] -- ignore any solutions with remaining spaces
rowOptions' [] _ _ _ _ = error "still have prev cells left over"
rowOptions' (f:fs) prev curr spaces _ = foldl rowOptions'' [] options
    where
        options = validOptions curr prev f
        fix = f /= '-'
        prev' = if null prev then [] else tail prev
        adj = adjustSpaces spaces
        rowOptions'' others cell = 
            rowOptions' fs prev' (cell:curr) (adj cell) fix ++ others

{- | Adjust the number of spaces if we use one 
-}
adjustSpaces :: Int -> Char -> Int
adjustSpaces spaces cell = if cell == ' ' then spaces - 1 else spaces

{- | Calculates a list of valid options for a cell, given the eastern cells on
the same row and the southern cells on the previous row, and a fixed value if
any. Returns either the full list, or the one matching the fixed value, or
null if nothing matches.
-}
validOptions :: [Cell] -> [Cell] -> Cell -> [Cell]
validOptions curr prev fix = 
    let
        knowEast = not (null curr)
        knowSouth = not (null prev)
        east = knowEast && hasWest (head curr)
        south = knowSouth && hasNorth (head prev)
        fixMatchEast  = not knowEast  || east  == (hasEast  fix) 
        fixMatchSouth = not knowSouth || south == (hasSouth fix)
    in
        if fix == '-' then
            -- if free choice of cell, allow any possibility that fits
            cellOptions south east
        else
            -- if fixed cell, allow it if possible (treating edges as unknown)
            if fixMatchEast && fixMatchSouth then
                [fix]
            else
                []  -- nothing fits
