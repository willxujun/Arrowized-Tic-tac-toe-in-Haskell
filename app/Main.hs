{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow
import qualified Control.Category as Cat
import           Control.Monad
import           Data.Maybe

import qualified Graphics         as G
import           Lib
import           Logic
import           SDLInput

import           Foreign.C.Types
import qualified Graphics.UI.GLUT as GL
import           SDL
import qualified SDL.Font

{-user defined signal functions-}
type Input = (MouseLocation, Maybe MouseClickEvent)
type MouseLocation = Point V2 CInt
type ScreenPos = Point V2 CInt

mouseFollower :: SF Input ScreenPos
mouseFollower = arr fst

-- mouseFollowerAccum :: SF MouseLocation ScreenPos
-- mouseFollowerAccum =
--   let f curr prev = foldl (\acc v -> acc && (abs v) <= 15) True (curr - prev)
--   in accumEvent (mkPoint 0 0) f

leftClickFollower :: SF Input ScreenPos
leftClickFollower = proc (_, inp) -> do
  mpos <- arr (\event -> case event of
          Nothing                           -> Nothing
          Just (MouseClickEvent pos True _) -> Just pos
          _                                 -> Nothing) -< inp
  accumEvent (mkPoint 0 0) (\curr prev -> curr) -< (fmap convertIntPoint mpos)

dragger :: SF Input ScreenPos
dragger = proc (mloc, inp) -> do
  click <- arr (\event -> case event of
          Nothing                           -> Nothing
          Just (MouseClickEvent pos True _) -> Just True
          Just (MouseClickEvent _ _ True)   -> Just False
          _                                 -> Nothing) -< inp
  (newPos, _) <- accum' (mkPoint 0 0, False) (test 15) -< (mloc, click)
  returnA -< newPos
  where
    test threshold (mloc, click) acc@(pt, status) = case status of
      False -> case click of
                 Nothing -> acc
                 Just True -> let isNear = foldl (\acc v -> acc && abs v <= threshold) True (mloc - pt)
                              in if isNear then (mloc, True) else acc
                 _ -> acc
      True -> case click of
                Nothing    -> (mloc, True)
                Just False -> (pt, False)
                _          -> acc
{-Low level code-}
--SDL input channel, supplied by user
data MouseClickEvent = MouseClickEvent
  {
    pos    :: Point V2 Int
  , lbdown :: Bool
  , lbup   :: Bool
  }

processMouse :: IO (MouseLocation, Maybe MouseClickEvent)
processMouse = do
  e <- pollEvent
  pos <- getAbsoluteMouseLocation
  let extractPayload (Event _ p) = p
      p = fmap extractPayload e
      convertIntPoint (P p) = P $ fmap fromIntegral p
      extractMouseEventData (Just (MouseButtonEvent (MouseButtonEventData _ motion _ button _ pos)) )= Just $ MouseClickEvent (convertIntPoint pos) (motion == Pressed && button == ButtonLeft) (motion == Released && button == ButtonLeft)
      extractMouseEventData _ = Nothing
  return (pos, extractMouseEventData p)

rect x y w h = Rectangle (P $ V2 x y) (V2 w h)

mkPoint x y = P $ V2 x y

convertIntPoint (P p) = P $ fmap fromIntegral p

--SDL output channel, supplied by user
paintRect :: Renderer -> ScreenPos -> IO ()
paintRect r (P (V2 x y)) = do
  rendererDrawColor r $= V4 0 0 100 100
  clear r
  rendererDrawColor r $= V4 100 100 100 100
  fillRect r (Just $ rect (x-15) (y-15) 30 30)
  present r

--SDL loops
testLoop :: Renderer -> IO ()
testLoop r = runSF processMouse (paintRect r) mouseFollower

testLeftClick r = runSF processMouse (paintRect r) leftClickFollower

testDrag r = runSF processMouse (paintRect r) dragger

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  mousePos <- getAbsoluteMouseLocation
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
      -- extractMouse es = case es of
      --   [] -> Nothing
      --   (x:xs) ->
      --     case x of
      --       Event _ (MouseMotionEvent (MouseMotionEventData _ _ _ p _) ) -> Just p
      --       _ -> extractMouse xs
      -- newPos = fromMaybe (P $ V2 0 0) $ extractMouse events
      intToCInt (P sth) = P $ fmap fromIntegral sth
  rendererDrawColor renderer $= V4 0 0 100 100
  clear renderer
  paintRect renderer (intToCInt mousePos)
  unless qPressed (appLoop renderer)

gameLoop r = do
  font <- SDL.Font.load "/Library/Fonts/AmericanTypewriter.ttc" 40
  runSF processKeyboard (G.renderGame r font) game

gameLoop2 r ref = do
  font <- SDL.Font.load "/Library/Fonts/AmericanTypewriter.ttc" 40
  aux r ref font
    where aux r ref font = do
            keyInput <- processKeyboard
            output <- runOnce ref keyInput
            G.renderGame r font output
            unless (keyInput == Just Quit) (aux r ref font)

testFunc r = do
  rendererDrawColor r $= V4 0 0 100 100
  clear r

main :: IO ()
main = do
  initializeAll
  SDL.Font.initialize
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  ref <- sfInit (SFState game)
  gameLoop2 renderer ref

-- main :: IO ()
-- main = interact $ unlines
--   . ("Welcome to Arrow TTT" :)
--   . concat . map msg . takeWhile (not. hasWon)
--   . embed updateBoardAndPlayer
--   . map parseText
--   . lines
