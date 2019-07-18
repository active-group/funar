-- |A simplified embedded version of original BASIC.
-- Some things work, some things just give utterly mysterious type errors.
--
-- Beware, this is just a fun weekend hack.
module Language.BASIC(module Language.BASIC.Parser, module Language.BASIC.Types, runBASIC) where
import System.TimeIt

import Language.BASIC.Parser hiding (Expr)
import Language.BASIC.Types(Expr(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,
                                 SIN, COS, TAN, ATN, EXP, LOG, SQR, ABS, RND, INT, SGN))
import Language.BASIC.Interp

-- |Run a BASIC program with an interpreter.
runBASIC :: BASIC -> IO ()
runBASIC x = do
    let cmds = getBASIC x
--    putStrLn $ unlines $ map show cmds
    timeIt $ executeBASIC cmds
