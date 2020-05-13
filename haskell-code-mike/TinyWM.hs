module TinyWM where

import Data.Maybe   
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

-- ---------------------------------------------------------------------
-- A data structure for multiple workspaces containing stacks of screens
--

data StackSet window = StackSet
    { current :: Int, -- aktueller Stack
      stacks :: Map Int [window] } -- map workspaces to window stacks
    deriving (Eq, Show, Read)

-- | /O(n)/. Create a new empty stackset of 'n' workspaces
empty :: Int -> StackSet window
empty n = StackSet { current = 0, stacks  = windows }
  where
    windows = Map.fromList (zip [0..n-1] (repeat []))

-- | /O(log n)/. Set the given stack as being visible. If the index is out of
-- bounds, the stack is returned unmodified.
view :: Int -> StackSet window -> StackSet window
view stackIndex stackSet =
  if Map.member stackIndex (stacks stackSet)
  then stackSet { current = stackIndex }
  else stackSet

-- | /O(log s)/. Extract the element on the top of the current stack.
-- If no such element exists, Nothing is returned.
peek :: StackSet window -> Maybe window
peek stackSet =
  case Map.lookup (current stackSet) (stacks stackSet) of
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
rotate :: Ordering -> StackSet window -> StackSet window
rotate direction stackSet = stackSet { stacks = Map.adjust rot (current stackSet) (stacks stackSet) }
  where
    rot [] = []
    rot xs = case direction of
        GT -> tail xs ++ [head xs]
        LT -> last xs : init xs
        _  -> xs

-- ---------------------------------------------------------------------
-- operations that affect multiple workspaces

-- | /O(log n)/. Push. Insert an element onto the top of the current stack.
-- If the element is already in the current stack, it is moved to the top.
-- If the element is managed on another stack, it is removed from that stack.
--
push :: Eq window => window -> StackSet window -> StackSet window
push window stackSet = insert window (current stackSet) stackSet

-- | /O(log n)/. Insert an element onto the top of stack 'n'.
-- If the element is already in the stack 'n', it is moved to the top.
-- If the element exists on another stack, it is removed from that stack.
-- If the index is wrong an exception is thrown.
insert :: Eq window => window -> Int -> StackSet window -> StackSet window
insert window stackIndex stackSet = new { stacks = Map.adjust (window:) stackIndex (stacks new) }
    where new = delete window stackSet

-- | /O(n)/. Delete an element entirely from from the StackSet.
-- If the element doesn't exist, the original StackSet is returned unmodified.
-- If the current element is focused, focus will change.
delete :: Eq window => window -> StackSet window -> StackSet window
delete window stackSet =
  (maybe stackSet del) (List.find ((window `elem`) . snd) (Map.assocs (stacks stackSet)))
  where
    del (stackIndex, _) = stackSet { stacks = Map.adjust (List.delete window) stackIndex (stacks stackSet) }

-- | /O(log n)/. shift. move the client on top of the current stack to
-- the top of stack 'n'. If the stack to move to is not valid, an
-- exception is thrown. If there's no client on the current stack, the
-- stack set is returned unchanged.
shift :: Ord window => Int -> StackSet window -> StackSet window
shift stackIndex stackSet = maybe stackSet (\window -> insert window stackIndex stackSet) (peek stackSet)

-- | /O(log n)/. Index. Extract the stack at workspace 'n'.
-- If the index is invalid, an exception is thrown.
index :: Int -> StackSet window -> [window]
index stackIndex stackSet = fromJust (Map.lookup stackIndex (stacks stackSet))

