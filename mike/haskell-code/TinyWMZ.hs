module TinyWMZ where

import Data.Maybe   

-- Zipper

data StackSet window = StackSet {
  current :: Workspace window,
  prev :: [Workspace window],
  next :: [Workspace window]
}

data Workspace window = Workspace { tag :: Int, stack :: Stack window }

data Stack window =
    Empty
  | Node {
      focus :: window,
      left :: [window],
      right :: [window]
    }

-- Constructing a new window manager with 'n' virtual workspaces
new :: Int -> StackSet window
new n | n > 0 = StackSet workspace [] workspaces
    where
        (workspace:workspaces) = [ Workspace tag Empty | tag <- [0 .. n-1]]

-- Extract the currently visible window, or nothing if the workspace is empty
peek :: StackSet window -> Maybe window
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
index :: StackSet window -> [window]
index stackSet =
  with []
       (\ (Node focus leftWindows rightWindows) ->
          (reverse leftWindows) ++ [focus] ++ rightWindows)
       stackSet

modify :: Stack window -> (Stack window -> Stack window) -> StackSet window -> StackSet window
modify = undefined

-- Move focus to the left or right window on the current workspace
focusLeft, focusRight :: StackSet window -> StackSet window

focusLeft = undefined
focusRight = undefined


insertLeft, insertRight :: window -> StackSet window -> StackSet window
insertLeft  window = modify (Node window [] []) (\(Node focus left right) -> Node window left (focus:right))
insertRight window = modify (Node window [] []) (\(Node focus left right) -> Node window (focus:left) right)
                                                             
-- Delete the currently focused window
delete :: StackSet window -> StackSet window
delete = undefined

-- View the virtual workspace to the left or right.
viewLeft :: StackSet window -> StackSet window
viewLeft (StackSet focus (left0:left) right) = StackSet left0 left (focus:right)
viewLeft stackSet = stackSet

viewRight :: StackSet window -> StackSet window
viewRight (StackSet focus left (right0:right)) = StackSet right0 (focus:left) right
viewRight stackSet = stackSet

-- Move the currently focused window to workspace with tag 'n'
shift  :: Int -> StackSet window -> StackSet window
shift stackIndex stackSet 
   | stackIndex >= 0 && stackIndex /= oldIndex = maybe stackSet go (peek stackSet)
   | otherwise = stackSet
 where
   oldIndex = tag (current stackSet)

   -- now, if everything is in order, run our little script:
   go window =
       foldl (flip ($))
             stackSet
             [ delete        -- first, delete current window
             , view stackIndex      -- set stackIndex workspace as current
             , insertLeft window  -- insert into this workspace
             , view oldIndex ]    -- now go back to the original workspace

view :: Int -> StackSet window -> StackSet window
view stackIndex stackSet@(StackSet (Workspace currentTag _) _ _)
    | stackIndex >= 0 = foldr traverse stackSet [1 .. abs (stackIndex - currentTag)]
    | otherwise = stackSet

 where -- work out which direction to move
    traverse _ = if signum (stackIndex - currentTag) >= 0 then viewRight else viewLeft
