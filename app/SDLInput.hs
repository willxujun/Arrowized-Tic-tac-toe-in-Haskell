-- |

module SDLInput
  ( KeyInput(..),
    processKeyboard,
    parse
  ) where

import           SDL

data KeyInput = Up | Down | Lft | Rt | Enter | Quit deriving Eq

parse:: Maybe Keycode -> Maybe KeyInput
parse keycode =
  case keycode of
    Just KeycodeUp     -> Just Up
    Just KeycodeDown   -> Just Down
    Just KeycodeLeft   -> Just Lft
    Just KeycodeRight  -> Just Rt
    Just KeycodeReturn -> Just Enter
    Just KeycodeQ      -> Just Quit
    _                  -> Nothing

--low level binding
processKeyboard :: IO (Maybe KeyInput)
processKeyboard = do
  e <- pollEvent
  let p = fmap eventPayload e
      eventData (Just (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ c _)))) = Just c
      eventData _ = Nothing
  return (parse (eventData p))
