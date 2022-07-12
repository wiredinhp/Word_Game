module Lib
    ( Game(Game, gameGrid, gameWords)
    , score
    , totalWords
    , playWord
    , playGame
    , Grid
    , Cell(Cell,Indent)
    , formatGrid
    , findWord
    , findWords
    , getLines
    , gridWithCoords
    , findWordInCellInfix
    , findWordInCellPrefix
    , cells2string
    , cell2char
    , zipOverGridWith
    , zipOverGrid
    , coordsGrid
    , makeRandomGrid
    , fillInBlanks
    , formatGame
    , completed
    , outputGrid
    , mapOverGrid
    , makeGame
    , formatGameGrid
    ) where
 
import Data.List (isInfixOf, transpose)
import Data.Char (toLower)
import System.Random
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as M



type Grid a = [[a]]

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

zipOverGrid :: Grid a -> Grid b -> Grid(a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a->b->c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid(Integer, Integer)
coordsGrid =
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell -> IO()
outputGrid grid = putStrLn (formatGrid grid)

mapOverGrid :: (a->b) -> Grid a -> Grid b
mapOverGrid = map . map

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char


cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

data Game = Game { gameGrid :: Grid Cell,
                   gameWords :: M.Map String (Maybe [Cell])
                 }

data Cell = Cell (Integer, Integer) Char 
            | Indent
              deriving (Eq, Ord, Show)

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc  = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
  in Game gwc dict


formatGame :: Game -> String
formatGame game = formatGameGrid game 
               ++ "\n\n"
               ++ (show $ score game)
               ++ "/"
               ++ (show $ totalWords game)

makeRandomGrid gen = 
    let (gen1,gen2) = split gen
        row = randomRs ('A','Z') gen1
    in row : makeRandomGrid gen2

fillInBlanks gen grid = 
    let r = makeRandomGrid gen
        fill '_' r = r
        fill c _   = c
    in zipOverGridWith fill grid r


score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

playWord :: Game -> String -> Game
playWord game word | not (M.member word (gameWords game)) = game
playWord game word =
  let grid = gameGrid game
      foundWord = findWord grid word
      newGame = case foundWord of
        Nothing -> game
        Just cs ->
          let words = gameWords game
              words' = M.insert word foundWord words
          in Game grid words'
  in newGame

completed :: Game -> Bool
completed game = score game == totalWords game 

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word = 
  let grid = gameGrid game
      foundWord = findWord grid word
  in case foundWord of
      Nothing -> game
      Just cs ->
        let dict = gameWords game
            newDict = M.insert word foundWord dict
        in game { gameWords = newDict }
  

formatGameGrid :: Game -> String
formatGameGrid game = 
    let grid = gameGrid game
        dict = gameWords game :: M.Map String (Maybe [Cell])
        cellSet = concat . catMaybes . M.elems $ dict
        formatCell cell =
            let char = cell2char cell
            in if cell `elem` cellSet then char else toLower char
        charGrid = mapOverGrid cell2char grid
    in unlines charGrid

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
  let findWord' = findWord grid
      foundWords = map findWord' words
  in catMaybes foundWords

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      foundWords = map (findWordInCellInfix word) lines
  in listToMaybe (catMaybes foundWords)

getLines :: Grid Cell -> [[Cell]]
getLines grid =
  let horizontal = grid
      vertical = transpose horizontal
      diagonal = diagonalize horizontal
      diagonal' = diagonalize (map reverse horizontal)
      lines = horizontal ++ vertical ++ diagonal ++ diagonal'
  in lines ++ (map reverse lines)

diagonalize :: Grid Cell -> Grid Cell
-- diagonalize grid = transpose (skew grid)
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (x:xs) = x : skew (map indent xs)
  where indent line = Indent : line


findWordInCellInfix :: String -> [Cell] -> Maybe [Cell]
findWordInCellInfix _ [] = Nothing
findWordInCellInfix word line =
  let foundWord = findWordInCellPrefix [] word line
  in case foundWord of
       Nothing -> findWordInCellInfix word (tail line)
       Just _ -> foundWord

cells2string = map cell2char

findWordInCellPrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellPrefix acc (s:ss) (c:cs) | s == cell2char c
                                  = findWordInCellPrefix (c : acc) ss cs
findWordInCellPrefix acc []     _ = Just (reverse acc)
findWordInCellPrefix _    _     _ = Nothing
