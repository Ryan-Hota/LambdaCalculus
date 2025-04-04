{-#OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}


import Prelude hiding (Word, words)
import Data.List.NonEmpty (NonEmpty((:|)), toList)
import Data.Char (isSpace)

-- data LambdaExpression var = V var | L var (LambdaExpression var) | A (LambdaExpression var) (LambdaExpression var) 

data Class = Space | Lambda | Dot | Text deriving (Eq, Show)

data Word = Word { classOf :: Class, string :: String } | Bracketed Sentence deriving Show
type Sentence = [Word]

tokenizeByClassifier :: (Char -> Class) -> String -> Sentence
tokenizeByClassifier classifier = assertBalanced . foldr step ([]:|[]) where

    assertBalanced (x:|[]) = x
    assertBalanced _ = error "unbalanced - more ')' than '('"

    -- | unTokenize . step chr === (:) chr . intercalate ')' . map unTokenize \
    -- These ')' are the ones for which a corresponding '(' is yet to be found
    step ')'                 sentences                 = [                                ]  :| toList sentences
    step '(' (    _     :| [                       ] ) =     error "unbalanced - less ')' than '('"
    step '(' ( sentence :| ( sentence' : sentences ) ) = ( Bracketed sentence : sentence' )  :| sentences
    step chr ( sentence :|          sentences        ) = chr `prependChrToSentence` sentence :| sentences

    prependChrToSentence chr [                        ] = [ Word classOfChr [chr            ] ]                                 where classOfChr = classifier chr
    prependChrToSentence chr ( word@(Word{}) : words )  =
        (             
                       if classOfChr == classOf word 
                                                     then [ Word classOfChr (chr:string word) ]
                                                     else [ Word classOfChr [chr            ] , word ]
        )                                                                                              ++ words                 where classOfChr = classifier chr
    prependChrToSentence chr words@( Bracketed _ : _ ) =  [ Word classOfChr [chr            ] ]        ++ words                 where classOfChr = classifier chr


main :: IO ()
main = print $ tokenizeByClassifier classifier "( x y ) z" where classifier chr = if isSpace chr then Space else Text
