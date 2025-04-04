module Beta where

import Data.List (delete)
import Data.Containers.ListUtils (nubOrd)
import Fresh ( FreshGenerateAble(..), Avoiding(..) )
import LambdaExpression ( Λ(..) )
import Control.Monad.Writer (Writer, MonadWriter (..))

freeVariables :: Ord var => Λ var -> [var]
freeVariables (Variable x) = [x]
freeVariables (Abstract arg m) = delete arg (freeVariables m)
freeVariables (Apply m n) = freeVariables m `union` freeVariables n
                                       where union s1 s2 = nubOrd ( s1 ++ s2 )

(~:=) :: (Ord var, FreshGenerateAble var) =>
         var -- x
    -> Λ var -- N
    -> Λ var -- M
    -> Λ var -- M[x:=n]
( old ~:= new ) (Variable x) = if x == old
                            then new
                            else Variable x
( old ~:= new ) (Abstract arg m) = if arg == old
                                then Abstract      arg                                         m
                                else Abstract arg' $ ( old ~:= new ) $ (arg ~:= Variable arg') m
                                where         arg' = generateFresh arg Avoiding (old:freeVariables new)
( old ~:= new ) (Apply  m  n) = Apply ( ( old ~:= new ) m) ( ( old ~:= new ) n)

isRedex :: Λ var -> Bool
isRedex (Apply (Abstract _ _) _) = True
isRedex           _              = False

containsRedex :: Λ var -> Bool
containsRedex (Apply (Abstract _ _) _) = True
containsRedex (Variable _)   = False
containsRedex (Abstract _ m) = containsRedex m
containsRedex (Apply m n)    = containsRedex m || containsRedex n

betaReduceExpression :: (Ord var, FreshGenerateAble var, Show var) => Λ var -> Writer String ( Λ var )
betaReduceExpression (Apply (Abstract arg m) n) = writer ( ( arg ~:= n ) m , "beta reduce" )
betaReduceExpression              m             = error ( show m ++ " isn't a redex, but was called to be beta reduced." )

-- betaReduceSubExpression :: (Ord var, FreshGenerateAble var, Show var) => Λ var -> Λ var
-- betaReduceSubExpression expression
--     | isRedex expression = betaReduceExpression expression
--     | containsRedex expression = case expression of
--                                 (Variable x)     -> error ( "variable " ++ show x ++ " does not contain redex" )
--                                 (Abstract arg m) -> Abstract arg $ betaReduceSubExpression m
--                                 (Apply m n)      -> if containsRedex m
--                                                      then Apply (betaReduceSubExpression m)             n
--                                                      else Apply              m              (betaReduceSubExpression n)
--     | otherwise = error ( show expression ++ " does not contain redex" )

-- betaNormalize :: (Ord var, FreshGenerateAble var, Show var) => Λ var -> Λ var
-- betaNormalize expression = if containsRedex expression
--                             then betaNormalize $ betaReduceSubExpression expression
--                             else expression