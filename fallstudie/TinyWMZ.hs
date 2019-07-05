module TinyWMZ where

import Data.Maybe   
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

-- Zipper

data StackSet a = StackSet {
  current :: Workspace a,
  prev :: [Workspace a],
  next :: [Workspace a]
}

data Workspace a = Workspace { tag :: Int, stack :: Stack a }

data Stack a =
    Empty
  | Node {
      focus :: a,
      left :: [a],
      right :: [a]
    }

-- Constructing a new window manager with 'n' virtual workspaces
new :: Int -> StackSet a
new n | n > 0 = StackSet t [] rs
    where
        (t:rs) = [ Workspace i Empty | i <- [0 ..n-1]]

-- Extract the currently visible window, or nothing if the workspace is empty
peek   :: StackSet a -> Maybe a
{-
peek stackSet =
  case stack (current stackSet) of
    Empty -> Nothing
    Node window _ _ -> Just window
-}

with forEmpty forNode stackSet  =
    case stack (current stackSet) of
      Empty -> forEmpty
      node@(Node _ _ _) -> forNode node
  
peek = with Nothing (Just . focus)

-- Index. Extract the windows on the current workspace as a list, for tiling
index :: StackSet a -> [a]
index stackSet =
  with [] (\ (Node t ls rs) -> (reverse ls) ++ [t] ++ rs) stackSet

modify :: Stack a -> (Stack a -> Stack a) -> StackSet a -> StackSet a
modify = undefined

-- Move focus to the left or right window on the current workspace
focusLeft, focusRight :: StackSet a -> StackSet a

focusLeft = undefined
focusRight = undefined


insertLeft, insertRight :: a -> StackSet a -> StackSet a
insertLeft  a = modify (Node a [] []) $ \(Node t l r) -> Node a l (t:r)
insertRight a = modify (Node a [] []) $ \(Node t l r) -> Node a (t:l) r
                                                             
-- Delete the currently focused window
delete :: StackSet a -> StackSet a
delete = undefined

-- View the virtual workspace to the left or right.
viewLeft  :: StackSet a -> StackSet a
viewLeft  (StackSet t (l:ls) rs) = StackSet l ls (t:rs)
viewLeft  t = t

viewRight :: StackSet a -> StackSet a
viewRight (StackSet t ls (r:rs)) = StackSet r (t:ls) rs
viewRight t = t

-- Move the currently focused window to workspace with tag 'n'
shift  :: Int -> StackSet a -> StackSet a
shift new s 
   | new >= 0 && new /= old = maybe s go (peek s)
   | otherwise                                      = s
 where
   old = tag (current s)

   -- now, if everything is in order, run our little script:
   go w = foldl (flip ($)) s  
       [ delete        -- first, delete current window
       , view new      -- set new workspace as current
       , insertLeft w  -- insert into this workspace
       , view old ]    -- now go back to the original workspace

view :: Int -> StackSet a -> StackSet a
view i s@(StackSet (Workspace n _) _ _)
    | i >= 0     = foldr traverse s [1.. abs (i-n)]
    | otherwise  = s

 where -- work out which direction to move
    traverse _ = if signum (i-n) >= 0 then viewRight else viewLeft
