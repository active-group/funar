{-# LANGUAGE ScopedTypeVariables #-}


module TinyWMProperties where
 
import TinyWM

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Text.Show.Functions
import Test.QuickCheck

import Debug.Trace

------------------------------------------------------------------------
--
-- Building StackSets from lists
--

fromList :: Ord window => (Int, [[window]]) -> StackSet window
fromList (_, []) = error "Cannot build a StackSet from an empty list"
fromList (stackIndex, windows) | stackIndex < 0 || stackIndex >= length windows
                = error $ "Cursor index is out of range: " ++ show (stackIndex, length windows)
fromList (stackIndex, windows) = 
  let stackSet0 = empty (length windows)
      insertAt windows stackIndex stackSet =
        foldr (\ window stackSet -> insert window stackIndex stackSet) stackSet windows
      stackSet1 =
        foldr (\ (stackIndex, windows) stackSet -> insertAt windows stackIndex stackSet)
              stackSet0
              (zip [0..] windows)
  in view stackIndex stackSet1

-- flatten a stackset to a list
toList  :: StackSet window -> (Int, [[window]])
toList stackSet = (current stackSet, map snd (Map.toList (stacks stackSet)))

-- ---------------------------------------------------------------------
--
-- Some useful predicates and helpers
--

-- a window is a member
member :: Eq window => window -> StackSet window -> Bool
member window stackSet =
    case List.find ((window `elem`) . snd) (Map.assocs (stacks stackSet)) of
        Nothing -> False
        _       -> True

-- | /O(n)/. Number of stacks
size :: T -> Int
size = Map.size . stacks

-- | Height of stack 'n'
height :: Int -> T -> Int
height stackIndex stackSet = length (index stackIndex stackSet)

--
-- Generate arbitrary stacksets
--
instance (Show window, Ord window, Arbitrary window) => Arbitrary (StackSet window) where
    arbitrary = do
        sz <- choose (1,5)
        n  <- choose (0,sz-1)
        ls <- vector sz
        let s = fromList (fromIntegral n,ls)
        return s

instance (Ord window, CoArbitrary window) => CoArbitrary (StackSet window) where
    coarbitrary stackSet= coarbitrary (toList stackSet)

------------------------------------------------------------------------

--
-- constrain it to a simple element type
--
type T = StackSet Int

-- Invariants:
--
-- * no element should ever appear more than once in a StackSet
-- * the current index should always be valid
--
-- All operations must preserve this.
--
invariant (stackSet :: T) = inBounds stackSet && noDuplicates (concat (Map.elems (stacks stackSet)))
    where
        noDuplicates windows = List.nub windows == windows
        inBounds stackSet = current stackSet >= 0 && current stackSet < sz where sz = Map.size (stacks stackSet)

-- test generator
prop_invariant = invariant


prop_empty_I  stackIndex = stackIndex > 0 ==> invariant (empty stackIndex)
prop_view_I   stackIndex (stackSet :: T) = invariant (view stackIndex stackSet)
prop_rotate_I stackIndex (stackSet :: T) = invariant (rotate stackIndex stackSet)
prop_push_I   stackIndex (stackSet :: T) = invariant (push stackIndex stackSet)
prop_delete_I stackIndex (stackSet :: T) = invariant (delete stackIndex stackSet)
prop_shift_I  stackIndex (stackSet :: T) = stackIndex >= 0 && stackIndex < size stackSet
                                 ==> invariant (shift stackIndex stackSet)
prop_insert_I stackIndex window (stackSet :: T) = window >= 0 && window < size stackSet
                                 ==> invariant (insert stackIndex window stackSet)


------------------------------------------------------------------------

-- empty StackSets have no windows in them
prop_empty n = n > 0 ==> all null (Map.elems (stacks stackSet))
    where stackSet = empty n :: T

-- empty StackSets always have focus on workspace 0
prop_empty_current n = n > 0 ==> current stackSet == 0
    where stackSet = empty n :: T

------------------------------------------------------------------------

-- pushing a window into an empty stackset means that that window is now
-- a member of the stackset
prop_push  window n = n > 0 ==> member window (push window stackSet)
    where stackSet = empty n :: T

-- if i `notElem` x, then pop . push == id
prop_push_pop (stackSet :: T) window = not (member window stackSet) ==> pop (push window stackSet) == stackSet
    where
        -- | Delete the currently focused window
        pop  :: Ord window => StackSet window -> StackSet window
        pop stackSet | Just window <- peek stackSet = delete window stackSet
                     | otherwise = stackSet

-- push shouldn't change anything but the current workspace
prop_push_local (stackSet :: T) window = not (member window stackSet) ==> hidden stackSet == hidden (push window stackSet)
  where
     hidden stackSet = [ index stackIndex stackSet | stackIndex <- [0 ..sz-1], stackIndex /= current stackSet ]
     sz = Map.size (stacks stackSet)

-- push is idempotent
prop_push_idem window (stackSet :: T) = push window stackSet == push window (push window stackSet)

-- push is just insert on the current stack
prop_push_insert (stackSet :: T) window = push window stackSet == insert window (current stackSet) stackSet

-- pushing n new elements increases the size by n
prop_size_push_n :: [Int] -> Int -> Property
prop_size_push_n windows n = n > 0 ==> size (foldr push stackSet windows) == n
    where stackSet = empty n :: T

-- pushing only adds to the height of the current stack
prop_current_push_n windows n = n > 0 ==>
    height (current stackSet) (foldr push stackSet nubWindows) == length nubWindows
    where
        nubWindows = List.nub windows
        stackSet = empty n  :: T

-- the value on top of the stack is the last element pushed
prop_push_peek_n (stackSet :: T) windows =
    not (null windows) ==> fromJust (peek (foldr push stackSet windows)) == head windows

-- if a value is peekable, it is also a member
prop_peek_member (stackSet :: T) = isJust (peek stackSet) ==> member (fromJust (peek stackSet)) stackSet

------------------------------------------------------------------------

-- delete removes windows
prop_delete (stackSet :: T) window = not (member window (delete window stackSet))

-- deletion does nothing if 'x' is not in the thing
prop_delete_uniq window (stackSet :: T) =
  not (member window stackSet) ==> delete window stackSet == stackSet

-- delete and push leave the original thingy
prop_delete_push window (stackSet :: T) =
  not (member window stackSet) ==> delete window (push window stackSet) == stackSet
    where _ = stackSet :: T

-- deletion is idempotent
prop_delete_idem window (stackSet :: T) =
  delete window stackSet == delete window (delete window stackSet)

------------------------------------------------------------------------

-- rotation is reversible in two directions
prop_rotaterotate1 (stackSet :: T)   = rotate LT (rotate GT stackSet) == stackSet
prop_rotaterotate2 (stackSet :: T)   = rotate GT (rotate LT stackSet) == stackSet

-- rotation through the height of a stack gets us back to the start
prop_rotate_all (stackSet :: T) =
  foldr (\_ stackSet -> rotate GT stackSet) stackSet [1..n] == stackSet
    where
      n = height (current stackSet) stackSet

prop_view_reversible (stackSet :: T) stackIndex' =
    let currentIndex  = current stackSet
        sz = size stackSet
        stackIndex  = stackIndex' `mod` sz
    in view currentIndex (view stackIndex stackSet) == stackSet

prop_view_idem (stackSet :: T) stackIndex' =
    let stackIndex = stackIndex' `mod` sz
        sz = size stackSet
    in view stackIndex (view stackIndex stackSet) == (view stackIndex stackSet)

prop_shift_reversible stackIndex' (stackSet :: T) =
    let stackIndex = stackIndex' `mod` sz
        sz = size stackSet
        currentIndex  = current stackSet
    in height currentIndex stackSet > 0
         ==> (view currentIndex . shift currentIndex . view stackIndex . shift stackIndex) stackSet == stackSet

------------------------------------------------------------------------

main = do
    let runT s a = do print s; a

    runT "invariant" $ do
         quickCheck prop_invariant
         quickCheck prop_empty_I
         quickCheck prop_view_I
         quickCheck prop_rotate_I
         quickCheck prop_push_I
         quickCheck prop_delete_I
         quickCheck prop_shift_I
         quickCheck prop_insert_I

    runT "empty" $ do
         quickCheck prop_empty
         quickCheck prop_empty_current

    runT "push" $ do
        quickCheck prop_push
        quickCheck prop_push_pop
        quickCheck prop_push_local
        quickCheck prop_push_insert
--        quickCheck prop_size_push_n
        quickCheck prop_current_push_n
        quickCheck prop_push_idem

    runT "delete" $ do
        quickCheck prop_delete
        quickCheck prop_delete_uniq
        quickCheck prop_delete_push

    runT "peek" $ do
        quickCheck prop_push_peek_n
        quickCheck prop_peek_member
        quickCheck prop_delete_idem

    runT "rotate" $ do
        quickCheck prop_rotaterotate1
        quickCheck prop_rotaterotate2
        quickCheck prop_rotate_all

    runT "view" $ do
        quickCheck prop_view_reversible
        quickCheck prop_view_idem

    runT "shift" $ do
        quickCheck prop_shift_reversible

