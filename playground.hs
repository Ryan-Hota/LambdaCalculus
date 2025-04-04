import LambdaExpression
import AbbreviatedLambdaExpression
import Data.Bifunctor (bimap, Bifunctor (..))
import Beta
import Fresh
import System.IO (withFile, IOMode (..), hGetContents, readFile')
import SimpleString (SimpleString (..))
import Data.Functor ((<&>))

{-
 
-}
(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

initDefs :: Λ SimpleString -> Λ SimpleString
initDefs = id
    --   ( Simple "K" 0 ~:= formalize (read " \\ x y . x ") )
    -- . ( Simple "S" 0 ~:= formalize (read " \\ f x v . (f v) ( x v ) ") )
    -- . ( Simple "X" 0 ~:= formalize (read " \\ x . x S K") )
    

solveBetaQuestions :: IO ()
solveBetaQuestions =
    readFile' "betaQuestions.txt" 
    >>= (
        lines
        |> map (span (/=':'))
        |> map (bimap read (formalize.read.drop 2))
        |> ( zipWith second <$> scanl (\m (x,n)->m.(x~:=n)) initDefs <*> id )
        |> map (second (abbreviate.betaNormalize))
        |> map (
            bimap show ((" = "++).show)
            |> uncurry (++)
            )
        |> unlines
    )
    |> putStr