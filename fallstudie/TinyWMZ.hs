module TinyWMZ where

import Data.Maybe   
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

data StackSet a =
  StackSet { current ::  Workspace a    -- currently focused workspace
           , prev    :: [Workspace a]   -- workspaces to the left
           , next    :: [Workspace a] } -- workspaces to the right

data Workspace a = Workspace  { tag :: Int, stack :: Stack a }

data Stack a =
    Empty
  | Node { focus  ::  a        -- focused window
         , left   :: [a]       -- clowns to the left
         , right  :: [a] }     -- jokers to the right

-- Constructing a new window manager with 'n' virtual workspaces
new :: Int -> StackSet a
new n | n > 0 = StackSet t [] rs
    where
        (t:rs) = [ Workspace i Empty | i <- [0 ..n-1]]

-- Extract the currently visible window, or nothing if the workspace is empty
peek   :: StackSet a -> Maybe a
peek s = case stack (current s) of
    Empty      -> Nothing
    Node t _ _ -> Just t

-- refactor:
with :: b -> (Stack a -> b) -> StackSet a -> b
with d f s =
  case stack (current s) of
    Empty -> d
    v     -> f v

-- v2:
-- peek   :: StackSet a -> Maybe a
-- peek = with Nothing (return . focus)

        
-- Index. Extract the windows on the current workspace as a list, for tiling
index :: StackSet a -> [a]
index = with [] $ \(Node t ls rs) -> reverse ls ++ t : rs

modify :: Stack a -> (Stack a -> Stack a) -> StackSet a -> StackSet a
modify d f s = s { current = (current s) { stack = with d f s } }

-- Move focus to the left or right window on the current workspace
focusLeft, focusRight :: StackSet a -> StackSet a
focusLeft = modify Empty $ \c -> case c of
   Node _ []     [] -> c
   Node t (l:ls) rs -> Node l ls (t:rs)
   Node t []     rs -> Node x xs [t] where (x:xs) = reverse rs -- wrap

focusRight = modify Empty $ \c -> case c of
   Node _ []     [] -> c
   Node t ls (r:rs) -> Node r (t:ls) rs
   Node t ls     [] -> Node x [t] xs where (x:xs) = reverse ls -- wrap

insertLeft, insertRight :: a -> StackSet a -> StackSet a
insertLeft  a = modify (Node a [] []) $ \(Node t l r) -> Node a l (t:r)
insertRight a = modify (Node a [] []) $ \(Node t l r) -> Node a (t:l) r
                                                             
-- Delete the currently focused window
delete :: StackSet a -> StackSet a
delete = modify Empty $ \c -> case c of
    Node t ls     (r:rs) -> Node r ls rs -- try right first
    Node t (l:ls) []     -> Node l ls [] -- else left.
    Node _ []     []     -> Empty

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
