module Language.BV.Symbolic.Test where

import System.Random (getStdGen, randoms)
import Control.Monad(forM_)


import Language.BV.Gen
import Language.BV.Eval
import Language.BV.Symbolic.SEval
import Language.BV.Symbolic.Types

main = do
    r <- getStdGen
    let ops = ["plus"]
    let size = 3
    let exprs = genExpr ops size
    forM_ exprs $ \e ->
        let x = 0
            ctx = [('x', x)]
            ret = word2sword $ evalExpr e ctx
            

            sctx = [('x', word2sword x)]
            sret = seval e sctx
        in if ret == sret
           then print("OK")
           else do print("!!!")
                   print(e)
                   print(ret)
                   print(sret)
                   error("LOOBSTER!")
        
