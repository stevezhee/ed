module Main where

import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (get)
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Char
import Control.Monad.State
import Data.List

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
  startGUI defaultConfig setup

canvasSize = 400

setup window = do
  return window # set title "Canvas - Examples"
  bdy <- getBody window
  tv <- liftIO $ atomically $ newTVar $ St Insert NoShift $ Z (reverse "hello, ") "world"
  
  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width  canvasSize
    # set style [("border", "solid black 1px"), ("background", "#eee")]

  getBody window #+
    [ column [element canvas]
    ]

  let disp i = do
        v <- liftIO $ atomically $ readTVar tv
        canvas # UI.clearCanvas
        canvas # UI.fillText (pp $ zp v) (10,60)
        canvas # UI.fillText (unwords [show i, show (move v), show (shift v)]) (10,120)
        
  on UI.keydown bdy $ \i -> do
    liftIO $ atomically $ modifyTVar tv $ execState (action i)
    disp i

  disp 0

safeHead x = case x of
  "" -> ""
  a:_ -> [a]

safeTail x = case x of
  "" -> ""
  _:bs -> bs

moveRight (Z a b) = Z (safeHead b ++ a) (safeTail b)

data MoveMode = Move | Insert deriving (Show, Eq)
data ShiftMode = NoShift | Shift | CapsLock deriving (Show, Eq)

data St = St
  { move :: MoveMode
  , shift :: ShiftMode
  , zp :: Z Char
  }

type M a = State St a

action :: Int -> M ()
action i = modify $ actionf i'
  where
    c = toEnum i
    i'
      | isUpper c = fromEnum $ toLower c
      | otherwise = case lookup i keycodeTbl of
          Nothing -> i
          Just c -> fromEnum c

keycodeTbl :: [(Int, Char)]
keycodeTbl =
  [ (186,';')
  , (187,'=')
  , (188,',')
  , (189,'-')
  , (190,'.')
  , (191,'/')
  , (192,'`')
  , (219,'[')
  , (220,'\\')
  , (221,']')
  , (222,'\'')
  ]
  
actionf :: Int -> St -> St
actionf i st
  | i == 13 = case mv of
    Move -> st{ move = Insert }
    Insert -> st{ move = Move }
  | i == 16 = case sh of
      NoShift -> st{ shift = Shift }
      Shift -> st{ shift = CapsLock }
      CapsLock -> st{ shift = NoShift }
  | mv == Move = case c of
      'a' -> st{ zp = Z [] $ reverse a ++ b }
      'f' -> st{ zp = Z (safeTail a) (safeHead a ++ b) }
      'j' -> st{ zp = moveRight $ zp st }
      ';' -> st{ zp = Z (reverse b ++ a) [] }
      _ -> st
  | sh == NoShift = insert c
  | sh == CapsLock = insert $ upper c
  | otherwise = (insert $ upper c){ shift = NoShift }
  where
    insert c = st{ zp = moveRight $ Z a (c : b) }
    upper c
      | isLower c = toUpper c
      | otherwise = case lookup c upperTbl of
          Nothing -> c
          Just c' -> c'
    sh = shift st
    c = toEnum i
    z@(Z a b) = zp st
    mv = move st

upperTbl = zip "`1234567890-=[]\\;',./" "~!@#$%^&*()_+{}|:\"<>?"

-- f i z@(Z a b) = case i of
--   _ | c == 'A' -> 
--   _ | c == 'F' -> 
--   _ | c == 'J' -> 
--   186 -> 
--   _ | isUpper c -> moveRight $ Z a (toLower c : b)
--   _ | isDigit c -> 
  
--   _ -> moveRight $ Z a (c : b)
--   where
--     c = toEnum i

class PP a where pp :: a -> String

data Z a = Z [a] [a]

instance PP a => PP (Z a) where
  pp (Z a b) = concatMap pp (reverse a) ++ "_" ++ concatMap pp b

instance PP Char where pp c = [c]
