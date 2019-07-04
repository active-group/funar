{-# LANGUAGE ScopedTypeVariables #-}


module TinyWMProperties where
 
import TinyWM

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

import Text.Show.Functions
import Test.QuickCheck

import Debug.Trace

------------------------------------------------------------------------
--
-- Building StackSets from lists
--

fromList :: Ord a => (Int, [[a]]) -> StackSet a
fromList (_,[]) = error "Cannot build a StackSet from an empty list"
fromList (n,xs) | n < 0 || n >= length xs
                = error $ "Cursor index is out of range: " ++ show (n, length xs)
fromList (o,xs) = view o $
    foldr (\(i,ys) s ->
        foldr (\a t -> insert a i t) s ys)
            (empty (length xs)) (zip [0..] xs)

-- flatten a stackset to a list
toList  :: StackSet a -> (Int,[[a]])
toList x = (current x, map snd $ M.toList (stacks x))

-- ---------------------------------------------------------------------
--
-- Some useful predicates and helpers
--

-- a window is a member
member :: Ord a => a -> StackSet a -> Bool
member k w =
    case L.find ((k `elem`) . snd) (M.assocs (stacks w)) of
        Nothing -> False
        _       -> True

-- | /O(n)/. Number of stacks
size :: T -> Int
size = M.size . stacks

-- | Height of stack 'n'
height :: Int -> T -> Int
height i w = length (index i w)

--
-- Generate arbitrary stacksets
--
instance (Show a, Ord a, Arbitrary a) => Arbitrary (StackSet a) where
    arbitrary = do
        sz <- choose (1,5)
        n  <- choose (0,sz-1)
        ls <- vector sz
        let s = fromList (fromIntegral n,ls)
        return s

instance (Ord a, CoArbitrary a) => CoArbitrary (StackSet a) where
    coarbitrary s = coarbitrary (toList s)

------------------------------------------------------------------------

--
-- constrain it to a simple element type
--
type T = StackSet Int

-- reflexive
prop_eq_refl (a :: T)     = a == a

prop_eq_symm (a :: T) b   = a == b ==> b == a

prop_eq_tran (a :: T) b c = a == b && b == c ==> a == c

prop_eq_subs (a :: T) b f = a == b ==> f a == f b
    where
        _ = f :: T -> T

------------------------------------------------------------------------

-- Invariants:
--
-- * no element should ever appear more than once in a StackSet
-- * the current index should always be valid
--
-- All operations must preserve this.
--
invariant (w :: T) = inBounds w && noDuplicates (concat $ M.elems (stacks w))
    where
        noDuplicates ws = L.nub ws == ws
        inBounds x      = current x >= 0 && current x < sz where sz = M.size (stacks x)

-- test generator
prop_invariant = invariant


prop_empty_I  n          = n > 0 ==> invariant $ empty n
prop_view_I   n (x :: T) =           invariant $ view n x
prop_rotate_I n (x :: T) =           invariant $ rotate n x
prop_push_I   n (x :: T) =           invariant $ push n x
prop_delete_I n (x :: T) =           invariant $ delete n x
prop_shift_I  n (x :: T) = n >= 0 && n < size x
                                 ==> invariant $ shift n x
prop_insert_I n i (x :: T) = i >= 0 && i < size x
                                 ==> invariant $ insert n i x


------------------------------------------------------------------------

-- empty StackSets have no windows in them
prop_empty n = n > 0 ==> all null (M.elems (stacks x))
    where x = empty n :: T

-- empty StackSets always have focus on workspace 0
prop_empty_current n = n > 0 ==> current x == 0
    where x = empty n :: T

------------------------------------------------------------------------

-- pushing a window into an empty stackset means that that window is now
-- a member of the stackset
prop_push  i n = n > 0 ==> member i (push i x)
    where x = empty n :: T

-- if i `notElem` x, then pop . push == id
prop_push_pop (x :: T) i = not (member i x) ==> pop (push i x) == x
    where
        -- | Delete the currently focused window
        pop  :: Ord a => StackSet a -> StackSet a
        pop  w   | Just k <- peek w = delete k w
                 | otherwise        = w

-- push shouldn't change anything but the current workspace
prop_push_local (x :: T) i = not (member i x) ==> hidden x == hidden (push i x)
  where
     hidden w = [ index n w | n <- [0 ..sz-1], n /= current w ]
     sz = M.size (stacks x)

-- push is idempotent
prop_push_idem i (x :: T) = push i x == push i (push i x)

-- push is just insert on the current stack
prop_push_insert (x :: T) i = push i x == insert i (current x) x

-- pushing n new elements increases the size by n
prop_size_push_n :: [Int] -> Int -> Property
prop_size_push_n ws n = n > 0 ==> size (foldr push x ws) == n
    where x = empty n :: T

-- pushing only adds to the height of the current stack
prop_current_push_n is n = n > 0 ==>
    height (current x) (foldr push x js) == length js
    where
        js = L.nub is
        x = empty n  :: T

-- the value on top of the stack is the last element pushed
prop_push_peek_n (x :: T) is =
    not (null is) ==> fromJust (peek (foldr push x is)) == head is

-- if a value is peekable, it is also a member
prop_peek_member (x :: T) = isJust (peek x) ==> member (fromJust (peek x)) x

------------------------------------------------------------------------

-- delete removes windows
prop_delete (x :: T) i = not (member i (delete i x))

-- deletion does nothing if 'x' is not in the thing
prop_delete_uniq i x = not (member i x) ==> delete i x == x
    where _ = x :: T

-- delete and push leave the original thingy
prop_delete_push i x = not (member i x) ==> delete i (push i x) == x
    where _ = x :: T

-- deletion is idempotent
prop_delete_idem i (x :: T) = delete i x == delete i (delete i x)

------------------------------------------------------------------------

-- rotation is reversible in two directions
prop_rotaterotate1 (x :: T)   = rotate LT (rotate GT x) == x
prop_rotaterotate2 (x :: T)   = rotate GT (rotate LT x) == x

-- rotation through the height of a stack gets us back to the start
prop_rotate_all (x :: T) = foldr (\_ y -> rotate GT y) x [1..n] == x
  where
    n = height (current x) x

prop_view_reversible (x :: T) r =
    let n  = current x
        sz = size x
        i  = r `mod` sz
    in view n (view (fromIntegral i) x) == x

    where _ = x :: T

prop_view_idem (x :: T) r =
    let i = fromIntegral $ r `mod` sz
        sz = size x
    in view i (view i x) == (view i x)

prop_shift_reversible r (x :: T) =
    let i  = fromIntegral $ r `mod` sz
        sz = size x
        n  = current x
    in height n x > 0 ==> (view n . shift n . view i . shift i) x == x

------------------------------------------------------------------------

main = do
    let runT s a = do print s; a

    runT "Eq" $ do
        quickCheck prop_eq_refl
        quickCheck prop_eq_symm
        quickCheck prop_eq_tran
        quickCheck prop_eq_subs

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

