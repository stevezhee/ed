{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (get, text)
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Char
import Control.Monad.State
import Data.List
import Text.PrettyPrint hiding (style)

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
  startGUI defaultConfig setup

canvasSize = 800

lineHeight = 30

setup window = do
  return window # set title "Canvas - Examples"
  bdy <- getBody window
  tv <- liftIO $ atomically $ newTVar $ St Move NoShift $ toZ a
  -- tv <- liftIO $ atomically $ newTVar $ St Move NoShift $ Z (reverse "hello, ") "world."
  
  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width  canvasSize
    # set UI.textFont "20px sans-serif"
    # set style [("border", "solid black 1px"), ("background", "#eee")]

  getBody window #+
    [ column [element canvas]
    ]

  let disp i = do
        v <- liftIO $ atomically $ readTVar tv
        canvas # UI.clearCanvas
        let ss = lines $ show $ pp $ zp v
        sequence_ [ canvas # UI.fillText s (10,y) | (s,y) <- zip ss [60, 60 + lineHeight .. ] ]
        -- canvas # draw (zp v) (10, 60)
        canvas # UI.fillText (unwords [show i, show (move v), show (shift v)]) (10,520)
        
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
    i' = case lookup i keycodeTbl of
      Just a -> fromEnum a
      Nothing | isUpper c -> fromEnum $ toLower c
      _ -> i
    c = toEnum i

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
  , (226,'\\')
  ]

-- moveBegin (Z a b) = Z [] $ reverse a ++ b
-- moveEnd (Z a b) = Z (reverse b ++ a) []
-- moveLeft (Z a b) = Z (safeTail a) (safeHead a ++ b)
-- moveRight (Z a b) = Z (safeHead b ++ a) (safeTail b)

-- del (Z a "") = Z (safeTail a) ""
-- del (Z a b) = Z a (tail b)

actionf :: Int -> St -> St
actionf i st
  | i == 13 = case mv of
    Move -> st{ move = Insert }
    Insert -> st{ move = Move }
  | i == 16 = case sh of
      NoShift -> st{ shift = Shift }
      Shift -> st{ shift = CapsLock }
      CapsLock -> st{ shift = NoShift }
  -- | i == 46 = modZ del
  | mv == Move = case c of
      -- 'a' -> modZ moveBegin
      -- 'f' -> modZ moveRight
      -- 'j' -> modZ moveLeft
      -- ';' -> modZ moveEnd
      _ -> st
  -- | sh == NoShift = insert c
  -- | sh == CapsLock = insert $ upper c
  -- | otherwise = (insert $ upper c){ shift = NoShift }
  where
    -- insert c = st{ zp = moveRight $ Z a (c : b) }
    upper c
      | isLower c = toUpper c
      | otherwise = case lookup c upperTbl of
          Nothing -> c
          Just c' -> c'
    sh = shift st
    c :: Char = toEnum i
    -- z@(Z a b) = zp st
    mv = move st
    -- modZ f = st{ zp = f $ zp st }

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

-- class Draw a where draw :: a -> UI.Point -> UI.Canvas -> UI ()
-- instance Draw Char where draw c = UI.fillText [c]

class PP a where pp :: a -> Doc

instance PP Char where pp c = char c

data Tree a = T a ([Tree a], [Tree a])
  deriving Show

data Z a = Z
  { down :: ([Tree a], [Tree a])
  , up :: [Tree a]
  , horiz :: ([Tree a], [Tree a])
  , val :: a
  } deriving Show

ppTree x ys = x $+$ (nest 4 $ vcat ys)

instance PP a => PP (Tree a) where
  pp (T a (bs, cs)) = ppTree (pp a) $ map pp bs ++ map pp cs

instance PP a => PP (Z a) where
  pp x = ppTree (hcat [ text ">", pp $ val x]) $ map pp $ lChilds x ++ rChilds x
   
-- instance Draw a => Draw (Tree a) where
-- instance Draw a => Draw (Z a) where

-- data Z a = Z [a] [a]
-- instance PP a => PP (Z a) where
--   pp (Z a b) = concatMap pp (reverse a) ++ "_" ++ concatMap pp b

a = mkT 'a' [b,c]
b = mkT 'b' []
c = mkT 'c' [d,e]
d = mkT 'd' []
e = mkT 'e' []

mkT a bs = T a ([], bs)

toZ (T a b) = Z
  { down = b
  , up = []
  , horiz = ([], [])
  , val = a
  }

moveDown z = case down z of
  ([], []) -> z
  (e, T a b : ds) -> Z
    { horiz = (e, ds)
    , val = a
    , up = T (val z) (horiz z) : up z
    , down = b
    }
  _ -> moveLeft $ moveDown z

left = fst . horiz
right = snd . horiz
lChilds = fst . down
rChilds = snd . down

moveUp z = case up z of
  [] -> z
  T a b : cs -> Z
    { horiz = b
    , val = a
    , up = up z
    , down = (left z, T (val z) (down z) : right z)
    }

moveLeft z = case horiz z of
  (a : bs, c) -> z{ horiz = (bs, a : c) }
  _ -> z

moveRight z = case horiz z of
  (bs, c : ds) -> z{ horiz = (c : bs, ds) }
  _ -> z
