module SimpleString where

import Data.Char (isAlpha, isNumber)
import Control.Applicative ((<|>))

data SimpleString = Simple String Int deriving (Eq, Ord)

instance Read SimpleString where
    readsPrec :: Int -> ReadS SimpleString
    readsPrec d stringIntRest = do
        _ <- filter isAlpha $ take 1 stringIntRest -- check whether there any alphas at all
        let (           string           , intRest ) = span isAlpha  stringIntRest
        let (                        int ,    rest ) = span isNumber  ('0':intRest)
        return ( Simple string (read int) ,    rest )
        
instance Show SimpleString where
    show :: SimpleString -> String
    show (Simple string 0) = string
    show (Simple string n) = string ++ show n