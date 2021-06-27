-- | Lib defines all the different validation layers for haskell to perform. It checks for win on column,row and diagonal lines.
module Lib
  ( columnSize,
    rowSize,
    boardSize,
    checkIfWon,
    checkAllDiagonals,
    checkDiagonal,
    checkDiagonalDownRight,
    checkDiagonalDownLeft,
    checkAllColumns,
    checkColumn,
    checkAllRows,
    checkRow,
    getSubstring,
    getColumn,
  )
where

{---CONSTANT VALUES---}

-- | Size of the columns in connect 4
columnSize :: Int
columnSize = 7

-- | Amount of rows in connect 4.
rowSize :: Int
rowSize = 6

-- | The size of the game board where players play.
boardSize :: Int
boardSize = columnSize * rowSize

-- | Handler for all validation layers. Checks for all win conditions.
-- >>> checkIfWon "111100100000100001001100201000000201202000" '1'
-- True
-- >>> checkIfWon "110100100000100001001100201000220201202000" '2'
-- False
checkIfWon :: String -> Char -> Bool
checkIfWon xs x
  | checkAllRows xs x 0 = True
  | checkAllColumns xs x 0 = True
  | checkAllDiagonals xs x 0 = True
  | otherwise = False

-- | checks for diagonals everywhere on the board, all directions.
-- >>> checkAllDiagonals "000000000000000000100000110000122000121200" '1' 0
-- True
-- >>> checkAllDiagonals "000000000000000000200000210000211000211200" '2' 0
-- True
-- >>> checkAllDiagonals "000000000000000000000000110000122000121200" '1' 0
-- False
checkAllDiagonals :: String -> Char -> Int -> Bool
checkAllDiagonals xs x i
  | i >= (boardSize `div` 2) = False
  | checkDiagonal xs x i = True
  | otherwise = checkAllDiagonals xs x (i + 1)

-- | Check if there is 4 in a row diagonal from position i in the map
-- >>> checkDiagonal "000000002000000120200021210001112000211200" '2' 8
-- True
-- >>> checkDiagonal "000000000000000000200000210000211000211200" '1' 4
-- False
checkDiagonal :: String -> Char -> Int -> Bool
checkDiagonal xs x i
  | (i `mod` columnSize) < 4 && checkDiagonalDownRight xs x i = True
  | (i `mod` columnSize) > 2 && checkDiagonalDownLeft xs x i = True
  | otherwise = False

-- | Tests if a string has right diagonal line from index i.
-- >>> checkDiagonalDownRight "000000002000000120200021210001112000211200" '2' 8
-- True
-- >>> checkDiagonalDownRight "000000002000000120200021210001112000211200" '2' 7
-- False
-- >>> checkDiagonalDownRight "100002010000001000000100020000002000000202" '1' 1
-- False
checkDiagonalDownRight :: String -> Char -> Int -> Bool
checkDiagonalDownRight xs x i
  | xs !! i == x && xs !! (i + 8) == x && xs !! (i + 16) == x && xs !! (i + 24) == x = True
  | otherwise = False

-- | Tests if a string has left diagonal line from index i.
-- >>> checkDiagonalDownLeft "100002010000001000000100020000002000000202" '1' 1
-- False
-- >>> checkDiagonalDownLeft "000000000000000000100000110000122000121200" '1' 18
-- True
-- >>> checkDiagonalDownLeft "000000000000000000100000110000122000121200" '2' 18
-- False
checkDiagonalDownLeft :: String -> Char -> Int -> Bool
checkDiagonalDownLeft xs x i
  | xs !! i == x && xs !! (i + 6) == x && xs !! (i + 12) == x && xs !! (i + 18) == x = True
  | otherwise = False

-- | Checks all column wins in a string
-- >>> checkAllColumns "100002010000001000000100020000002000000202" '1' 0
-- True
-- >>> checkAllColumns "100002010000001000000100020000002000000202" '2' 0
-- False
checkAllColumns :: String -> Char -> Int -> Bool
checkAllColumns xs x i
  | i >= (rowSize -1) = False
  | checkColumn (getColumn xs i) x = True
  | otherwise = checkAllColumns xs x (i + 1)

-- | Checks for a straight line of 4 in a "column" string of size 7
--
-- >>> checkColumn "1111000" '2'
-- False
-- >>> checkColumn "1111000" '1'
-- True
checkColumn :: String -> Char -> Bool
checkColumn (a : b : c : d : _) x | x == a && x == b && x == c && x == d = True
checkColumn (_ : a : b : c : d : _) x | x == a && x == b && x == c && x == d = True
checkColumn (_ : _ : a : b : c : d : _) x | x == a && x == b && x == c && x == d = True
checkColumn _ _ = False

-- | Tests whether a string has any winning row.
--
-- >>> checkAllRows "010000010000011111210000200000202222200000" '1' 1
-- True
-- >>> checkAllRows "010000010000011111210000200000202222200000" '2' 1
-- False
checkAllRows :: String -> Char -> Int -> Bool
checkAllRows xs x i
  | i >= rowSize = False
  | checkRow (getSubstring (i * columnSize) ((i * columnSize) + columnSize) xs) x = True
  | otherwise = checkAllRows xs x (i + 1)

-- | Checks a string of length 6 if it contains 4 characters in a row.
--
-- >>> checkRow "0001111" '1'
-- True
-- >>> checkRow "2222000" '2'
-- True
-- >>> checkRow "" '1'
-- False
checkRow :: String -> Char -> Bool
checkRow (a : b : c : d : _) x | x == a && x == b && x == c && x == d = True
checkRow (_ : a : b : c : d : _) x | x == a && x == b && x == c && x == d = True
checkRow (_ : _ : a : b : c : d : _) x | x == a && x == b && x == c && x == d = True
checkRow (_ : _ : _ : a : b : c : d : _) x | x == a && x == b && x == c && x == d = True
checkRow _ _ = False

-- | Function returns the substring between two intervals if given a string
--
-- >>> getSubstring 6 22 "01234 return this part"
-- "return this part"
-- >>> getSubstring 1 5 ""
-- ""
-- >>> getSubstring 5 (-2) ""
-- ""
getSubstring :: Int -> Int -> String -> String
getSubstring x y = do
  if x < 0 || y < 0
    then getSubstring 0 0 -- just return an empty list in cases where the input is wrong.
    else take (y - x) . drop x

-- | Gets the column from the string, what is equal to a column on the game board.
--
-- >>> getColumn "000000000000001000000100000010000001000000" 5
-- "000000"
-- >>> getColumn "000000102000011001001100200110020001002000" 3
-- "001222"
getColumn :: String -> Int -> String
getColumn xs x = xs !! x : xs !! (x + 7) : xs !! (x + 14) : xs !! (x + 21) : xs !! (x + 28) : [xs !! (x + 35)]
