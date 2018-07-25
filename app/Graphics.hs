-- |
{-# LANGUAGE OverloadedStrings #-}
module Graphics

where

import qualified Data.Text       as Text
import           Foreign.C.Types
import           Logic
import           SDL
import           SDL.Font
import           SDL.Primitive

data Colour = White | Red | Blue

setColor :: Renderer -> Colour -> IO ()
setColor r White  = rendererDrawColor r $= V4 maxBound maxBound maxBound maxBound
setColor r Red    = rendererDrawColor r $= V4 maxBound 0 0 maxBound
setColor r Blue   = rendererDrawColor r $= V4 0 0 maxBound maxBound

clearScreen:: Renderer -> IO ()
clearScreen r = do
  setColor r White
  clear r

boardPos x y ver hor =
  let
    shiftDown (x,y) = (x, y+ver)
    firstRow = [(x,y), (x+hor,y), (x+2*hor, y)]
    secondRow = map shiftDown firstRow
    thirdRow = map shiftDown secondRow
  in [firstRow, secondRow, thirdRow]

renderBoard r b x y ver hor =
  let pos = boardPos x y ver hor
      taggedBoard = zipWith zip b pos
  in mapM_ (renderRow r) taggedBoard

renderRow:: Renderer -> [(Symbol, (Int,Int))] -> IO ()
renderRow r ls =
  let
    mkCircle x y =
      let
        x' = fromIntegral x
        y' = fromIntegral y
      in fillCircle r (V2 x' y') 10
    renderElem O x y = mkCircle x y (V4 maxBound 0 0 maxBound)
    renderElem X x y = mkCircle x y (V4 0 0 maxBound maxBound)
    renderElem _ _ _ = return ()
  in mapM_ (\(symbol, (x,y)) -> renderElem symbol x y) ls

renderFocus:: Renderer -> Focus -> IO ()
renderFocus r (x,y) =
  let
    rel (a,b) = (a-1, b-1)
    --relative position has x, y axes flipped
    (relX, relY) = rel (y,x)
    center (a,b) = (400 + a*100, 300 + b*100)
    (cX, cY) = center (relX, relY)

    mkRectangle x1 y1 x2 y2 =
      let [x1',y1',x2',y2'] = map fromIntegral [x1,y1,x2,y2]
      in rectangle r (V2 x1' y1') (V2 x2' y2') (V4 maxBound 0 0 maxBound)
  in mkRectangle (cX-20) (cY-20) (cX+20) (cY+20)

renderPlayer r p font = do
  let clip = mkRect 25 25 50 50
  s <- solid font (V4 0 0 0 0) (Text.pack $ show p)
  t <- createTextureFromSurface r s
  copy r t Nothing (Just clip)
  freeSurface s
  destroyTexture t

renderMessage r message font = do
  let clip = mkRect 0 500 200 30
  s <- solid font (V4 0 0 0 0) (Text.pack $ head message)
  t <- createTextureFromSurface r s
  copy r t Nothing (Just clip)
  freeSurface s
  destroyTexture t

mkRect :: Int -> Int -> Int -> Int -> Rectangle CInt
mkRect x1 y1 x2 y2 =
  let x1' = fromIntegral x1
      x2' = fromIntegral x2
      y1' = fromIntegral y1
      y2' = fromIntegral y2
  in Rectangle (P (V2 x1' y1')) (V2 x2' y2')

--top level
renderGame r font (Game b p exit message, focus) = do
  clearScreen r
  renderBoard r b 300 200 100 100
  renderFocus r focus
  renderPlayer r p font
  renderMessage r message font
  present r
