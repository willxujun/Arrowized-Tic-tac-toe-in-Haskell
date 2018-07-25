module Lib
    ( accum, accum', accumEvent, accumulate, constant,
      embed, hold, runSF, SF, stepDown, tag,
      sfInit, runOnce,
      SFState(..)
    ) where

import           Control.Arrow
import qualified Control.Category as Cat
import           Control.Monad
import           Data.IORef
import           Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-Arrow instance and primitives-}
newtype SF a b = SF {
    unSF :: a -> (SF a b, b)
}

instance Cat.Category SF where
    id = SF $ \a -> (Cat.id, a)
    (.) = dot
      where
        (SF cir2) `dot` (SF cir1) = SF $ \a ->
            let (cir1', b) = cir1 a
                (cir2', c) = cir2 b
            in  (cir2' `dot` cir1', c)

instance Arrow SF where
    arr f = SF $ \a -> (arr f, f a)
    first (SF f) = SF $ \(b,d) ->
            let (cir, c) = f b
            in  (first cir, (c,d))

instance ArrowChoice SF where
  left c@(SF f) = SF $ \input ->
    case input of
      Left l  -> let (newCir, out) = f l in (left newCir, Left out)
      Right r -> (left c, Right r)

instance ArrowLoop SF where
  loop c@(SF f) = SF $ \input ->
    let (sf', (c, d)) = f (input, d)
    in (loop sf', c)

--primitive
accum :: acc -> (a -> acc -> (acc, b)) -> SF a b
accum acc f = SF $ \inp ->
  let (acc',out) = f inp acc
  in (accum acc' f, out)

accum' :: acc -> (a -> acc -> acc) -> SF a acc
accum' acc f = accum acc (\inp acc -> let out = f inp acc in (out, out))

--primitive
tag :: b -> SF (Maybe a) (Maybe b)
tag val =
  arr $ \input -> case input of
                    Nothing      -> Nothing
                    Just someVal -> Just val

--primitive
hold :: a -> SF (Maybe a) a
hold init =
  let f inp acc = case inp of
        Nothing  -> (acc, acc)
        Just out -> (out, out)
  in accum init f

constant val = SF $ \_ -> (constant val, val)

--primitive to make a SF incapable of event processing behave as if it can
stepDown :: b -> SF a b -> SF (Maybe a) b
stepDown init sf = arr (\inp -> case inp of
                                  Nothing -> Right Nothing
                                  Just v  -> Left v)
              >>> ( (sf >>> arr Just) ||| arr id )
              >>> hold init

--primitive
accumulate :: a -> SF (Maybe (a -> a)) (Maybe a)
accumulate init =
  let f fIn acc = case fIn of
        Nothing -> (acc, Nothing)
        Just g -> let out = g acc
                  in (out, Just out)
  in accum init f

--primitive
accumEvent :: a -> (a -> a -> a) -> SF (Maybe a) a
accumEvent init g =
  let f inp acc = case inp of
        Nothing -> (acc, acc)
        Just curr -> let out = g curr acc
                     in (out, out)
  in accum init f

embed :: SF a b -> [a] -> [b]
embed cir ls = case ls of
  [] -> []
  (x:xs) -> let (cir', c) = unSF cir x
            in  c: embed cir' xs

runSF :: IO a     -- input channel
  -> (b -> IO ()) -- output channel, should be sdl renderer
  -> SF a b       -- signal function
  -> IO ()
runSF input output sf =
  do
     val <- input
     let (sf', res) = unSF sf val
     output res
     runSF input output sf'

data SFState a b = SFState {
  sf:: SF a b
  }

runOnce:: IORef (SFState a b) -> a -> IO b
runOnce ref input = do
  SFState sf <- readIORef ref
  let (cont, output) = unSF sf input
      newState = SFState cont
  writeIORef ref newState
  return output

sfInit = newIORef
