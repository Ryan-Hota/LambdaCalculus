module Fresh where

import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Bifunctor (first,second)
import Data.Char (isAlpha)
import SimpleString 

data Avoiding = Avoiding

class FreshGenerateAble a where
    generateFresh :: a -> Avoiding -> [a] -> a

instance FreshGenerateAble Int where
    generateFresh :: Int -> Avoiding -> [Int] -> Int
    generateFresh n _ = fromMaybe undefined . flip find [n..] . flip notElem

instance FreshGenerateAble (String, Int) where
    generateFresh :: (String, Int) -> Avoiding -> [(String, Int)] -> (String, Int)
    generateFresh (str, int) _ = (,) str . generateFresh int Avoiding . map snd . filter (fst.first (==str))

instance FreshGenerateAble SimpleString where
    generateFresh :: SimpleString -> Avoiding -> [SimpleString] -> SimpleString
    generateFresh string _ = uncurry Simple . generateFresh (interpret string) Avoiding . map interpret 
        where interpret (Simple string int) = (string, int)

