--Game logic for arrowized tic tac toe
{-# LANGUAGE Arrows #-}
module Logic
  ( game, dummy,
    initialState,
    updateBoardAndPlayer,
    GameState(..), Board, Focus,
    delayedEcho,
    parseText,
    Player(..), Symbol(..)
  ) where

import           Control.Arrow
import           Data.List
import           Lib
import           SDLInput

data Symbol = X | O | N deriving (Show,Eq)

data Player = PX | PO deriving Show

notP PX = PO
notP PO = PX

getSymbol:: Player -> Symbol
getSymbol PX = X
getSymbol PO = O

type Board = [[Symbol]]
type Focus = (Int, Int)
type InputXY = (Int, Int)

data GameState = Game
  {getBoard  :: Board,
   getPlayer :: Player,
   hasWon    :: Bool,
   msg       :: [String]
  }

alternate :: SF () Bool
alternate = accum True (\_ acc -> (not acc, acc))

delayedEcho :: a -> SF a a
delayedEcho init = accum init $ \inp acc -> (inp, acc)

updateFocus :: SF (Maybe KeyInput) (Maybe InputXY, Focus)
updateFocus = accum (0,0) f
  where
    validate (x,y) mov = case mov of
      Up   -> if x==0 then (x,y) else (x-1,y)
      Down -> if x==2 then (x,y) else (x+1,y)
      Lft  -> if y==0 then (x,y) else (x,y-1)
      Rt   -> if y==2 then (x,y) else (x,y+1)
    f op currFocus = case op of
      Nothing ->  (currFocus, (Nothing, currFocus))
      Just Enter -> (currFocus, (Just currFocus, currFocus))
      Just Quit -> (currFocus, (Nothing, currFocus))
      Just mov -> let n = validate currFocus mov
                  in (n, (Nothing, n))

emptyBoard :: Board
emptyBoard = map (const [N, N, N]) [(), (), ()]

updateBoardAndPlayer :: SF (Int, Int) GameState
updateBoardAndPlayer = proc (x, y) -> do
  rec result <- makeMove -< (x, y, player, exitGame)
      hasEnded <- checkStatus -< fst result
      exitGame <- delayedEcho False -< hasEnded
      player <- changePlayer -< snd result
      msg    <- giveMessage -< (fst result, player, hasEnded)
  returnA -< Game (fst result) player exitGame msg
        where
          makeMove :: SF (Int, Int, Player, Bool) (Board, Maybe (Int,Int))
          makeMove = accum emptyBoard updateBoard
            where updateBoard (x, y, p, exitGame) b =
                    let modify b p x y =
                          let (rows1, row: rows2) = splitAt x b
                              (cols1, e: cols2) = splitAt y row
                          in rows1 ++ ( (cols1 ++ getSymbol p: cols2) : rows2 )
                    in if exitGame then (b, (b, Nothing))
                       else case b !! x !! y of
                              N -> let b' = modify b p x y
                                in (b', (b', Just (x,y)) )
                              _ -> (b, (b,Nothing) )

          checkStatus :: SF Board Bool
          checkStatus = arr checkR &&& arr checkC &&& arr checkD >>> arr summarise
            where summarise (br, (bc, bd)) = br || bc || bd
                  checkR = foldl (\acc input -> acc || input) False . map allTheSame
                    where allTheSame :: [Symbol] -> Bool
                          allTheSame [] = True
                          allTheSame [x] = True
                          allTheSame (x:y:xs) = x /= N && x == y && allTheSame (y:xs)
                  checkC = checkR . transpose
                  checkD [[a,_,d], [_,b,_], [f,_,c]] = checkR [[a,b,c],[d,b,f]]

          changePlayer :: SF (Maybe (Int, Int)) Player
          changePlayer = tag notP >>> accumulate PX >>> hold PX

          giveMessage :: SF (Board, Player, Bool) [String]
          giveMessage = proc (b, p, hasEnded) -> do
            let
                m = if hasEnded then show (notP p) ++ " has won" else "continue"
            returnA -< [m]

game :: SF (Maybe KeyInput) (GameState, Focus)
game = updateFocus >>> first (stepDown initialState updateBoardAndPlayer)

initialState = Game emptyBoard PX False ["Welcome to Arrow Tic Tac Toe"]

dummy = constant (initialState, (0,0))

--a text UI
toString :: Board -> String
toString b = unlines $ map toString b
  where toString row = intercalate " | " $ map show row

parseText :: String -> (Int,Int)
parseText = convert . take 2 . fst . head . (reads :: ReadS [Int])
  where convert [a,b] = (a,b)
