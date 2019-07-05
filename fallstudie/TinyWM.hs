
module TinyWM where

import Data.Maybe   
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

-- ---------------------------------------------------------------------
-- A data structure for multiple workspaces containing stacks of screens
--

data StackSet a = StackSet
    { current :: Int, -- aktueller Stack
      stacks :: Map Int [a] } -- map workspaces to window stacks
    deriving (Eq, Show, Read)

-- | /O(n)/. Create a new empty stackset of 'n' workspaces
empty :: Ord a => Int -> StackSet a
empty n = StackSet { current = 0, stacks  = ws }
  where
    ws = M.fromList (zip [0..n-1] (repeat []))

-- | /O(log n)/. Set the given stack as being visible. If the index is out of
-- bounds, the stack is returned unmodified.
view :: Int -> StackSet a -> StackSet a
view index stackSet =
  if M.member index (stacks stackSet)
  then stackSet { current = index }
  else stackSet

-- | /O(log s)/. Extract the element on the top of the current stack.
-- If no such element exists, Nothing is returned.
peek :: Ord a => StackSet a -> Maybe a
peek stackSet =
  case M.lookup (current stackSet) (stacks stackSet) of
    Nothing -> Nothing
    Just [] -> Nothing
    Just (window:_) -> Just window

-- | /O(log n)/. rotate. cycle the current window list up or down.
-- Has the effect of rotating focus. In fullscreen mode this will cause
-- a new window to be visible.
--
--  rotate EQ   -->  [5,6,7,8,1,2,3,4]
--  rotate GT   -->  [6,7,8,1,2,3,4,5]
--  rotate LT   -->  [4,5,6,7,8,1,2,3]
--
--  where xs = [5..8] ++ [1..4]
--
rotate :: Ordering -> StackSet a -> StackSet a
rotate o w = w { stacks = M.adjust rot (current w) (stacks w) }
  where
    rot [] = []
    rot xs = case o of
        GT -> tail xs ++ [head xs]
        LT -> last xs : init xs
        _  -> xs

-- ---------------------------------------------------------------------
-- operations that affect multiple workspaces

-- | /O(log n)/. Push. Insert an element onto the top of the current stack.
-- If the element is already in the current stack, it is moved to the top.
-- If the element is managed on another stack, it is removed from that stack.
--
push :: Ord a => a -> StackSet a -> StackSet a
push k w = insert k (current w) w

-- | /O(log n)/. Insert an element onto the top of stack 'n'.
-- If the element is already in the stack 'n', it is moved to the top.
-- If the element exists on another stack, it is removed from that stack.
-- If the index is wrong an exception is thrown.
insert :: Ord a => a -> Int -> StackSet a -> StackSet a
insert k n old = new { stacks = M.adjust (k:) n (stacks new) }
    where new = delete k old

-- | /O(n)/. Delete an element entirely from from the StackSet.
-- If the element doesn't exist, the original StackSet is returned unmodified.
-- If the current element is focused, focus will change.
delete :: Ord a => a -> StackSet a -> StackSet a
delete k w = maybe w del $ L.find ((k `elem`) . snd) (M.assocs (stacks w))
  where
    del (i,_) = w { stacks = M.adjust (L.delete k) i (stacks w) }

-- | /O(log n)/. shift. move the client on top of the current stack to
-- the top of stack 'n'. If the stack to move to is not valid, an
-- exception is thrown. If there's no client on the current stack, the
-- stack set is returned unchanged.
shift :: (Ord a) => Int -> StackSet a -> StackSet a
shift n w = maybe w (\k -> insert k n w) (peek w)

-- | /O(log n)/. Index. Extract the stack at workspace 'n'.
-- If the index is invalid, an exception is thrown.
index :: Int -> StackSet a -> [a]
index k w = fromJust (M.lookup k (stacks w))

