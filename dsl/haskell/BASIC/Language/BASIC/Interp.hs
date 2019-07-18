{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Language.BASIC.Interp(executeBASIC) where
import Data.List
import qualified Data.Map as M
import System.Random
import Text.Printf

import Language.BASIC.Types

executeBASIC :: [Expr a] -> IO ()
executeBASIC cmds = run M.empty [] [] cmds
  where -- map from line number to the rest of the lines from that point.
        mcmds = M.fromList $ map (\ cs -> (cmdLabel (head cs), cs)) $ init $ tails cmds

        -- The function 'run' takes an environment (maps variables to values),
        -- a GOSUB stack, and FOR stack.
        --run stk cs | trace (show (stk, cmds)) False = undefined
        run _ _ _ [] = return ()
        run _ _ _ (Cmd _ End _:_) = return ()
        run env stk fors (Cmd _ Goto [Label l]:_) = goto env stk fors l
        run env stk fors (Cmd _ Gosub [Label l]:cs) = goto env (cs:stk) fors l
        run env stk fors (Cmd _ If [e, Label l]:cs) = do
            d <- eval env e
            if d /= Dbl 0 then goto env stk fors l else run env stk fors cs
        run _ [] fors (Cmd _ Return _ : _) = error "RETURN without GOSUB"
        run env (cs:stk) fors (Cmd _ Return _:_) = run env stk fors cs
        run env stk fors cs@(Cmd _ For [v,l,_,_] : cs') = do
            d <- eval env l
            run (M.insert v d env) stk (cs:fors) cs'
        run env stk fors@((Cmd _ For [v,_,h,s] : bs) : fors') (Cmd _ Next [v'] : cs) | v == v' = do
            let Dbl i = env M.! v
            Dbl hv <- eval env h
            Dbl sv <- eval env s
            let i' = i + sv
            if i' <= hv then run (M.insert v (Dbl i') env) stk fors bs
                       else run env stk fors' cs
        run env stk fors (Cmd _ Next _ : _) = error $ "Unmatched FOR/NEXT"
        run env stk fors (c:cs) = do env' <- run1 env c; run env' stk fors cs

        goto env stk fors l = maybe (error $ "No line " ++ show l) (run env stk fors) (M.lookup l mcmds)

        run1 env (Cmd _ Print es) = do mapM_ (\ e -> eval env e >>= prExpr) es; putStrLn ""; return env
        run1 env (Cmd _ Let [v,e]) = do d <- eval env e; return $ M.insert v d env
        run1 env (Cmd _ Rem _) = return env
        run1 env (Cmd _ Input [v]) = do
            let loop = do
                    s <- getLine
                    case reads s of
                        [(d,"")] -> return d
                        _ -> do putStrLn "Not a number, try again"; loop
            d <- loop
            return $ M.insert v (Dbl d) env
        run1 _ e = error $ "run1: " ++ show e

        eval _ e@(Dbl _) = return e
        eval _ e@(Str _) = return e
        eval env (Binop e1 op e2) = do
            v1 <- eval env e1
            v2 <- eval env e2
            case (v1, op, v2) of
             (Dbl d1, "+", Dbl d2)  -> return $ Dbl (d1 + d2)
             (Dbl d1, "-", Dbl d2)  -> return $ Dbl (d1 - d2)
             (Dbl d1, "*", Dbl d2)  -> return $ Dbl (d1 * d2)
             (Dbl d1, "/", Dbl d2)  -> return $ Dbl (d1 / d2)
             (Dbl d1, "^", Dbl d2)  -> return $ Dbl (d1 ** d2)
             (Dbl d1, "<>", Dbl d2) -> return $ Dbl (if d1 /= d2 then 1 else 0)
             (Dbl d1, "==", Dbl d2) -> return $ Dbl (if d1 == d2 then 1 else 0)
             (Dbl d1, "<", Dbl d2) -> return $ Dbl (if d1 < d2 then 1 else 0)
             (Dbl d1, ">", Dbl d2) -> return $ Dbl (if d1 < d2 then 1 else 0)
             (Dbl d1, "<=", Dbl d2) -> return $ Dbl (if d1 <= d2 then 1 else 0)
             (Dbl d1, ">=", Dbl d2) -> return $ Dbl (if d1 >= d2 then 1 else 0)
             x -> error $ "eval binop expected numbers " ++ show x
        eval env (SIN e) = unop env sin e
        eval env (COS e) = unop env cos e
        eval env (TAN e) = unop env tan e
        eval env (ATN e) = unop env atan e
        eval env (EXP e) = unop env exp e
        eval env (LOG e) = unop env log e
        eval env (ABS e) = unop env abs e
        eval env (SQR e) = unop env sqrt e
        eval env (SGN e) = unop env signum e
        eval env (INT e) = unop env (fromIntegral . truncate) e
        eval _   (RND _) = do d <- randomIO; return (Dbl d)
        eval env x | x > Var && x < None = return $ maybe (Dbl 0) id $ M.lookup x env
        eval _ x = error $ "eval: " ++ show x

        prExpr (Dbl i) = putStr $ chopDec $ printf "%g" i
        prExpr (Str s) = putStr s
        prExpr e = error $ "prExpr: " ++ show e
        chopDec s = let r = reverse s in if take 2 r == "0." then reverse (drop 2 r) else s

        unop env op e = do
            v <- eval env e
            case v of
                Dbl x -> return $ Dbl $ op x
                x -> error $ "eval unop expected numbers"
