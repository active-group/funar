{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, IncoherentInstances, ExtendedDefaultRules, OverloadedStrings, DeriveDataTypeable #-}
module Language.BASIC.Parser(
    getBASIC, BASIC,
    PRINT(..), END(..), LET(..), GOTO(..), IF(..), THEN(..), INPUT(..), FOR(..), TO(..), NEXT(..), STEP(..),
    Expr((:=)), (Language.BASIC.Parser.<>), (==), (<), (>), (<=), (>=), (^)
    ) where
import Prelude hiding ((==),(<),(>),(<=),(>=),(^))
import qualified Prelude as P
import Data.List
import Data.Maybe
import Data.Function
import Data.Typeable
import Data.IORef
import Data.String
import System.IO.Unsafe

import Language.BASIC.Types

--import Debug.Trace

-- 10 PRINT X; Y
joinPrint :: [Expr a] -> [Expr a]
joinPrint (Cmd l Print es : e : cs) | not (isCmd e) = joinPrint (Cmd l Print (es ++ [e]) : cs)
joinPrint (c : cs) = c : joinPrint cs
joinPrint [] = []

-- Get rid of := by moving operands into the command.
joinAssign :: Expr a -> Expr a
--joinAssign c | trace (show c) False = undefined
joinAssign (Cmd l For [v] := Binop e1 ";" (Binop e2 ";" e3)) = Cmd l For [v, e1, e2, e3]
joinAssign (Cmd l For [v] := Binop e1 ";" e2) = Cmd l For [v, e1, e2, Dbl 1]
joinAssign (Cmd l Let [v] := e) = Cmd l Let [v, e]
joinAssign c = c

isCmd :: Expr a -> Bool
isCmd (Cmd _ _ _) = True
isCmd _ = False

infix 4 <>, ==, <, >, <=, >=
(<>) :: Expr a -> Expr a -> Expr a
(<>) = binop "<>"
(==) :: Expr a -> Expr a -> Expr a
(==) = binop "=="
(<) :: Expr a -> Expr a -> Expr a
(<) = binop "<"
(>) :: Expr a -> Expr a -> Expr a
(>) = binop ">"
(<=) :: Expr a -> Expr a -> Expr a
(<=) = binop "<-"
(>=) :: Expr a -> Expr a -> Expr a
(>=) = binop ">="

infixr 8 ^
(^) :: Expr a -> Expr a -> Expr a
(^) = binop "^"

-- Rebalance some ops
binop :: String -> Expr a -> Expr a -> Expr a
binop op (Cmd l c [x]) y = Cmd l c (binops x op y)           -- 10 PRINT X+Y
binop op x (Binop y ";" z) = Binop (Binop x op y) ";" z      -- FOR
binop op (Binop x ";" y) z = Binop x ";" (Binop y op z)      -- FOR
binop op x y = Binop x op y

binops :: Expr a -> String -> Expr a -> [Expr a]
binops x op (Binop y ";" z) = [Binop x op y, z]
binops x op y = [Binop x op y]

-- Allow pahntom type changes.
flex :: Expr a -> Expr b
flex (Cmd l c es) = Cmd l c (map flex es)
flex (Str s) = Str s
flex (Dbl d) = Dbl d
flex (Label l) = Label l
flex (Binop e1 op e2) = Binop (flex e1) op (flex e2)
flex (e1 := e2) = flex e1 := flex e2
flex (SIN x) = SIN (flex x)
flex (COS x) = COS (flex x)
flex (TAN x) = TAN (flex x)
flex (ATN x) = ATN (flex x)
flex (EXP x) = EXP (flex x)
flex (LOG x) = LOG (flex x)
flex (ABS x) = ABS (flex x)
flex (SQR x) = SQR (flex x)
flex (RND x) = RND (flex x)
flex (INT x) = INT (flex x)
flex (SGN x) = SGN (flex x)
flex Var = Var
flex A = A
flex B = B
flex C = C
flex D = D
flex E = E
flex F = F
flex G = G
flex H = H
flex I = I
flex J = J
flex K = K
flex L = L
flex M = M
flex N = N
flex O = O
flex P = P
flex Q = Q
flex R = R
flex S = S
flex T = T
flex U = U
flex V = V
flex W = W
flex X = X
flex Y = Y
flex Z = Z
flex None = None

data PRINT = PRINT
data END = END | STOP | RETURN | REM deriving (Eq)
data LET = LET
data GOTO = GOTO | GOSUB deriving (Eq)
data IF = IF
data THEN = THEN
data INPUT = INPUT
data FOR = FOR
data TO = TO
data NEXT = NEXT
data STEP = STEP

-- Yuck!  But this is the only way I could figure out
-- how to make a Monad like Expr actually be able to save
-- every statement.
-- Now if we could just write 'x' instead of 'X' there
-- would be no need for unsafePerformIO.
instance Monad Expr where
    a >> b = unsafePerformIO $ do push (flex a); push (flex b)

instance Applicative Expr where

instance Functor Expr where
       
-- Stack for saving allevery expression we see.
stack :: IORef [Expr ()]
stack = unsafePerformIO $ newIORef []

-- Save an expression.
push :: Expr () -> IO (Expr a)
push None = return None
push x = do
    s <- readIORef stack
    writeIORef stack (x:s)
    return None

instance Num (Expr a) where
    (+) = binop "+"
    (-) = binop "-"
    (*) = binop "*"
    fromInteger = Dbl . fromInteger

instance Fractional (Expr a) where
    (/) = binop "/"
    fromRational = Dbl . fromRational

instance IsString (Expr a) where
    fromString = Str

-- 10 PRINT X
instance Eq (PRINT -> Expr a -> Expr b)
instance Show (PRINT -> Expr a -> Expr b)
instance Num (PRINT -> Expr a -> Expr b) where
    fromInteger i _ v = Cmd i Print [flex v]

-- 10 PRINT SQR(X)
instance Eq (PRINT -> (Expr c -> Expr c) -> Expr a -> Expr b)
instance Show (PRINT -> (Expr c -> Expr c) -> Expr a -> Expr b)
instance Num (PRINT -> (Expr c -> Expr c) -> Expr a -> Expr b) where
    fromInteger i _ f v = Cmd i Print [flex $ f $ flex v]

-- 10 PRINT 1
-- 10 PRINT 1.5
instance Eq (PRINT -> t -> Expr a)
instance Show (PRINT -> t -> Expr a)
instance (Show t, Typeable t) => Num (PRINT -> t -> Expr a) where
    fromInteger i _ x =
        let f con = fmap (\ v -> Cmd i Print [con v]) (cast x)
        in  case catMaybes [f Str, f (Dbl . fromInteger), f Dbl] of
            c : _ -> c
            [] -> error $ "Bad type(1) " ++ show x ++ " :: " ++ show (typeOf x)

-- 10 END
instance Eq (END -> Expr a)
instance Show (END -> Expr a)
instance Num (END -> Expr a) where
    fromInteger i c = Cmd i (if c P.== RETURN then Return else if c P.== REM then Rem else End) []

-- 10 LET X := Y, i.e., (10 LET X) := Y
instance Eq (LET -> Expr a -> Expr b)
instance Show (LET -> Expr a -> Expr b)
instance Num (LET -> Expr a -> Expr b) where
    fromInteger i _ v = Cmd i Let [flex v]

-- 10 GOTO 10
instance Eq (GOTO -> t -> Expr a)
instance Show (GOTO -> t -> Expr a)
instance (Integral t) => Num (GOTO -> t -> Expr a) where
    fromInteger i c l = Cmd i (if c P.== GOSUB then Gosub else Goto) [Label $ toInteger l]

-- 10 IF X <> 0 THEN 10, i.e., (10 IF X) <> (0 THEN 10)
instance Eq (IF -> Expr a -> Expr b)
instance Show (IF -> Expr a -> Expr b)
instance Num (IF -> Expr a -> Expr b) where
    fromInteger i _ v = Cmd i If [flex v]

-- 10 IF X <> 0 THEN 10, i.e. (10 IF X) <> (0 THEN 10)
instance Eq (THEN -> t -> Expr a)
instance Show (THEN -> t -> Expr a)
instance (Integral t) => Num (THEN -> t -> Expr a) where
    fromInteger c _ l = Binop (Dbl (fromInteger c)) ";" (Label $ fromIntegral l)

-- 10 INPUT X
instance Eq (INPUT -> Expr a -> Expr b)
instance Show (INPUT -> Expr a -> Expr b)
instance Num (INPUT -> Expr a -> Expr b) where
    fromInteger i _ v = Cmd i Input [flex v]

-- 10 FOR X := ...
instance Eq (FOR -> Expr a -> Expr b)
instance Show (FOR -> Expr a -> Expr b)
instance Num (FOR -> Expr a -> Expr b) where
    fromInteger i _ v = Cmd i For [flex v]

-- 10 FOR X := 1 TO Y, i.e., (10 FOR X) := (1 TO Y)
instance Num (TO -> Expr b -> Expr a) where
    fromInteger c _ x = Binop (Dbl (fromInteger c)) ";" (flex x)

castExpr x =
    let f con = fmap con (cast x)
        id' :: Expr () -> Expr a
        id' = flex
    in  case catMaybes [f (Dbl . fromInteger), f Dbl, f id'] of
        e : _ -> e
        [] -> error $ "Bad type(3) " ++ show x ++ " :: " ++ show (typeOf x)

-- 10 FOR X := 1 TO 10
-- 10 FOR X := 1 TO 10.5
instance Eq (TO -> t -> Expr a)
instance Show (TO -> t -> Expr a)
instance (Show t, Typeable t) => Num (TO -> t -> Expr a) where
    fromInteger c _ x = Binop (Dbl (fromInteger c)) ";" (castExpr x)
instance (Show t, Typeable t) => Fractional (TO -> t -> Expr a) where
    fromRational c _ x = Binop (Dbl (fromRational c)) ";" (castExpr x)

-- 10 FOR X := 1 TO 10 STEP 2
instance Eq (TO -> t -> STEP -> s -> Expr a)
instance Show (TO -> t -> STEP -> s -> Expr a)
instance (Show t, Typeable t, Show s, Typeable s) => Num (TO -> t -> STEP -> s -> Expr a) where
    fromInteger c _ x _ y =
      Binop (Dbl (fromInteger c)) ";" (Binop (castExpr x) ";" (castExpr y))
instance (Show t, Typeable t, Show s, Typeable s) => Fractional (TO -> t -> STEP -> s -> Expr a) where
    fromRational c _ x _ y =
      Binop (Dbl (fromRational c)) ";" (Binop (castExpr x) ";" (castExpr y))

-- 10 NEXT X
instance Eq (NEXT -> Expr a -> Expr b)
instance Show (NEXT -> Expr a -> Expr b)
instance Num (NEXT -> Expr a -> Expr b) where
    fromInteger i _ v = Cmd i Next [flex v]

getBASIC :: BASIC -> [Expr ()]
getBASIC cs = getBASIC' (cs >> 999999 END)

getBASIC' :: BASIC -> [Expr ()]
getBASIC' None =
    sortBy (compare `on` cmdLabel) $
    joinPrint $
    map joinAssign $
    reverse $
    unsafePerformIO (readIORef stack)
getBASIC' e = error $ "getBASIC: " ++ show e

