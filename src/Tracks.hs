module Tracks (
    gridOptions,
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

{- | Given a grid of fixed cells and a count of available spaces for each
row, generate a list of possible complete grids
-}
gridOptions :: Grid -> Counts -> [Grid]
gridOptions fix spaces =
    gridOptions' (reverse fix) (reverse spaces) "" []

{- | Like gridOptions, but taking a current grid being constructed.
-}
gridOptions' :: Grid -> Counts -> Row -> Grid -> [Grid]
gridOptions' [] [] lastRow curr = endOfGrid curr lastRow
gridOptions' [] _ _ _ = error "more counts than rows in the grid"
gridOptions' _ [] _ _ = error "more rows than counts in the grid"
gridOptions' (f:fs) (c:cs) _ curr = foldr gridOptions'' [] options
    where
        prev = if null curr then [] else head curr
        options = rowOptions f prev c
        gridOptions'' row others = gridOptions' fs cs f (row:curr) ++ others

{- | Special handling for the end of the grid. We want to suppress
solutions that have tracks leading out of the edge, unless they
were part of the fixed grid.
-}
endOfGrid :: Grid -> Row -> [Grid]
endOfGrid [] _ = []
endOfGrid (r:rs) fixed = 
    -- only accept solutions where the only cells that lead north are fixed
    if all (\(c,f) -> f == c || not (hasNorth c)) (zip r fixed) then
        [(r:rs)]
    else
        [] 

{- | Given the fixed cells, the previous row, and a count of available
spaces, generate a list of possible complete rows.
-}
rowOptions :: Row -> Row -> Int -> [Row]
rowOptions fix prev spaces =
    rowOptions' (reverse fix) (reverse prev) spaces '-' ""

{- | Like rowOptions, but taking a flag to say whether the previously
inserted item (head of curr) was forced by being a member of fix. Also,
takes the fixed and prev lists in reverse order, so they can be
accessed efficiently when constructing solutions from the right with
foldl. Also, takes the current row to date, allowing this to be grown
recursively. Finally, takes the last fixed cell, so it can trim unwanted
solutions.
-}
rowOptions' :: Row -> Row -> Int -> Cell -> Row -> [Row]
rowOptions' [] [] 0 lastFix curr = endOfRow curr lastFix
rowOptions' [] [] _ _ _ = [] -- ignore any solutions with remaining spaces
rowOptions' [] _ _ _ _ = error "still have prev cells left over"
rowOptions' (f:fs) prev spaces _ curr = foldr rowOptions'' [] options
    where
        options = validOptions curr prev f
        prev' = if null prev then [] else tail prev
        adj = adjustSpaces spaces
        rowOptions'' cell others = 
            rowOptions' fs prev' (adj cell) f (cell:curr) ++ others

{- | Special handling for the end of the row. We want to suppress
solutions that have tracks leading out of the edge, unless they
were part of the fixed grid.
-}
endOfRow :: Row -> Cell -> [Row]
endOfRow [] _ = []
endOfRow (c:cs) fixed = 
    if fixed == c || not (hasWest c) then
        [(c:cs)] 
    else
        [] 

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
