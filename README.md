# tracks
Solving 2 dimensional track puzzles, such as those published in The Times every Saturday. This actually solves a problem that is slightly generalised in a number of ways.

* Supports any (finite) number of rows and columns
* Grids can have any number of exits and entries (though only even numbers give solutions)
* There can be multiple independent tracks within the solution, including loops
* If there are multiple solutions, all are shown

The rule for edges is that if a piece is specified in the original puzzle, it is always used in the solution, even if it leads into or out of an edge. Otherwise, pieces are not allowed to lead in or out of the edges of the bounding rectangle of the puzzle.

## Algorithm
Start at one corner, taking one row at a time. Initially ignore the column counts -- we want to find all the solutions that match the given row counts.

Starting from the initial corner of the first row, generate a list of all the possible legal rows that have the requisite number of non-empty cells, and match up with the supplied cells. Disallow any solutions where the final cell leaks out of the grid, unless it was one of the originally supplied cells.

For each of these first rows, generate a list of all the possible second rows that have the requisite number of non-empty cells. It is possible that some legal first rows will not have any legal second rows -- these empty lists are dropped.

Work through each of the rows like this so we have a list of all possible legal grids given the non-empty cell counts for each row.

Disallow any solutions where any of the last-row cells leak out of the grid, unless they were the originally supplied cells.

Finally, filter out any solutions that have the wrong counts for non-empty cells by column.
