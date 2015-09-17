{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

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

canvasSize = 700

lineHeight = 30

setup window = do
  return window # set title "Canvas - Examples"
  bdy <- getBody window
  tv <- liftIO $ atomically $ newTVar $ St minBound minBound $ toZTree a
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
  _ : bs -> bs

data MoveMode = Move | Insert deriving (Show, Eq, Enum, Bounded)
data ShiftMode = NoShift | Shift | CapsLock deriving (Show, Eq, Enum, Bounded)

data St = St
  { move :: MoveMode
  , shift :: ShiftMode
  , zp :: ZTree (ZList Char)
  }

instance PP a => PP (ZList a) where
  pp (L bs cs) = hcat $ fmap pp (reverse bs) ++ [char '~'] ++ fmap pp cs
    
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

incWrap :: (Enum a, Bounded a, Eq a) => a -> a
incWrap x = if x == maxBound then minBound else succ x

actionf :: Int -> St -> St
actionf i st
  | c == ' ' = st{ move = incWrap $ move st }
  | i == 16 = st{ shift = incWrap $ shift st }
  -- | i == 46 = modZ del
  | mv == Move = case c of
      -- 'a' -> modZ moveBegin
      'k' -> modZ moveRight
      'j' -> modL moveRight
      'l' -> modZ moveDown
      'd' -> modZ moveLeft
      's' -> modZ moveUp
      'f' -> modL moveLeft
      _ -> st
  -- | mv == MoveVert = case c of
  --     -- ';' -> modZ moveEnd
  --     _ -> st
  -- | sh == NoShift = insert c
  -- | sh == CapsLock = insert $ upper c
  -- | otherwise = (insert $ upper c){ shift = NoShift }
  | otherwise = st
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
    modZ f = st{ zp = f $ zp st }
    modL f = modZ $ \z -> z{ val = f $ val z }

upperTbl = zip "`1234567890-=[]\\;',./" "~!@#$%^&*()_+{}|:\"<>?"

class PP a where pp :: a -> Doc

instance PP Char where pp = char
instance PP a => PP [a] where pp = hcat . map pp

data Tree a = T a ([Tree a], [Tree a])
  deriving (Show, Functor)

data ZList a = L [a] [a] deriving (Show, Functor)

data ZTree a = Z
  { down :: ([Tree a], [Tree a])
  , up :: [Tree a]
  , horiz :: ([Tree a], [Tree a])
  , val :: a
  } deriving (Show, Functor)

instance PP Doc where pp = id

instance PP a => PP (Tree a) where
  pp (T a (bs, cs)) = pp a $+$ (nest 4 $ vcat $ map pp (reverse bs) ++ map pp cs)

instance PP a => PP (ZTree a) where
  pp x = pp $ T (val b) (down b)
   where
     a = fmap pp x
     b = moveUp $ moveUp $ moveUp $ a{ val = hcat [ val a, text "<" ] } -- bal: move up until the top

-- data Z a = Z [a] [a]
-- instance PP a => PP (Z a) where
--   pp (Z a b) = concatMap pp (reverse a) ++ "_" ++ concatMap pp b

mkL x = L [] x

mkLT a bs = mkT (mkL a) bs

a = mkLT "hello" [b,c,f]
b = mkLT "world" [mkLT "nice2" [], mkLT "meetu" []]
c = mkLT "how" [d,e]
d = mkLT "are" []
e = mkLT "you?" []
f = mkLT "I" [mkLT "am" [], mkLT "fine" []]

mkT a bs = T a ([], bs)

toZTree :: Tree a -> ZTree a
toZTree (T a b) = Z
  { down = b
  , up = []
  , horiz = ([], [])
  , val = a
  }

left = fst . horiz
right = snd . horiz

moveUp z = case up z of
  [] -> z
  T a b : cs -> Z
    { val = a
    , horiz = b
    , up = cs
    , down = (left z, unfocus z : right z)
    }

moveDown z = case down z of
  ([], []) -> z
  (e, T a b : ds) -> Z
    { horiz = (e, ds)
    , val = a
    , up = T (val z) (horiz z) : up z
    , down = b
    }
  (a:bs, []) -> moveDown $ z{ down = (bs, [a]) }

class MoveHoriz a where
  moveLeft :: a -> a
  moveRight :: a -> a

unfocus z = T (val z) (down z)

instance MoveHoriz (ZList a) where
  moveLeft z = case z of
    L [] cs -> L (reverse cs) []
    L (a:bs) cs -> L bs (a:cs)
  moveRight z = case z of
    L cs [] -> L [] (reverse cs)
    L cs (a:bs) -> L (a:cs) bs
    
instance MoveHoriz (ZTree a) where
  moveLeft z = case horiz z of
    ([], []) -> z
    (T a b : cs, ds) -> z
      { horiz = (cs, unfocus z : ds)
      , val = a
      , down = b
      }
    ([], ds) -> z
      { horiz = (cs, [])
      , val = a
      , down = b
      }
      where
      T a b : cs = reverse $ unfocus z : ds

  moveRight z = case horiz z of
    ([], []) -> z
    (ds, T a b : cs) -> z
      { horiz = (unfocus z : ds, cs)
      , val = a
      , down = b
      }
    (ds, []) -> z
      { horiz = ([], cs)
      , val = a
      , down = b
      }
      where
      T a b : cs = reverse $ unfocus z : ds
    
