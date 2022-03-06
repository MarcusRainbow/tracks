module Tracks (
    rowOptions
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

" \9562\9565" = " ╚╝"
"\9562\9565 " = "╚╝ "
"\9552\9565 " = "═╝ "
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

{- | Given a list of cell options and a possibly fixed value for the cell,
determines a valid list of options. Either the full list, or the one matching
the fixed value, or null if nothing matches.
-}
validOptions :: [Cell] -> Cell -> [Cell]
validOptions cells '-' = cells
validOptions cells fixed = if fixed `elem` cells then [fixed] else []

{- | Says whether a cell has a path to the North 
-}
hasNorth :: Cell -> Bool
hasNorth c = c `elem` "║╚╝"

{- | Says whether a cell has a path to the West
-}
hasWest :: Cell -> Bool
hasWest c = c `elem` "═╗╝"

{- | Given the fixed cells, the previous row, the current row to date, and
a count of available spaces, generate a list of possible complete rows
-}
rowOptions :: Row -> Row -> Row -> Int -> Bool -> [Row]
rowOptions [] [] [] 0 _ = [] -- propagate null solution
rowOptions [] [] (c:cs) 0 force = if (force || not (hasWest c)) then [(c:cs)] else [] 
rowOptions [] [] _ _ _ = [] -- ignore any solutions with remaining spaces
rowOptions [] _ _ _ _ = error "still have prev cells left over"
rowOptions _ [] _ _ _ = error "still have fixed cells left over"
rowOptions (f:fs) (p:ps) curr spaces _ = foldl rowOptions' [] options
    where
        west = not (null curr) && hasWest (head curr)
        north = hasNorth p
        fix = f /= '-'
        -- TODO the problem with validOptions is that we want to override it at the edges if forced
        options = validOptions (cellOptions north west ) f
        adj = adjustSpaces spaces
        rowOptions' others cell = 
            rowOptions fs ps (cell:curr) (adj cell) fix ++ others

{- | Adjust the number of spaces if we use one 
-}
adjustSpaces :: Int -> Char -> Int
adjustSpaces spaces cell = if cell == ' ' then spaces - 1 else spaces
  