{-# LANGUAGE DeriveDataTypeable #-}
module Language.BASIC.Types where
import Data.Typeable

infix 0 :=
data Expr a
    = Cmd Integer Command [Expr a]
    | Str String
    | Dbl Double
    | Label Integer
    | Binop (Expr a) String (Expr a)
    | Expr a := Expr a
    | SIN (Expr a) | COS (Expr a) | TAN (Expr a)
    | ATN (Expr a) | EXP (Expr a) | LOG (Expr a)
    | RND (Expr a) | INT (Expr a) | SGN (Expr a)
    | ABS (Expr a) | SQR (Expr a)
    | Var
    | A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    | None
    deriving (Eq, Ord, Show, Typeable)

cmdLabel :: Expr a -> Integer
cmdLabel (Cmd l _ _) = l
cmdLabel e = error $ "cmdLabel: Strange top level command " ++ show e

data Command = Print | End | Let | Goto | Gosub | Return | If | Input | For | Next | Rem
    deriving (Eq, Ord, Show, Typeable)

type BASIC = Expr ()

