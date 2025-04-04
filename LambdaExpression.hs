module LambdaExpression where


import Control.Applicative ((<|>))
import Data.Char (isSpace)


data Λ var = 
    Variable var 
    | Abstract var ( Λ var )
    | Apply ( Λ var ) ( Λ var )


instance Read var => Read ( Λ var ) where

    readsPrec :: Read var => Int -> ReadS (Λ var)

    readsPrec d string = readVariable string <|> readAbstract string <|> readApplication string where

        readVariable = readParen False $ 
            \ spaces_var_rest -> do {
            (   _   ,var_rest) <- pure $ span isSpace spaces_var_rest ;
            (        var,rest) <- readsPrec d                var_rest ; return ( Variable var , rest) }

        readAbstract open_lambda_arg_dot_m_close_rest = do {
            (        "(" ,lambda_arg_dot_m_close_rest) <- lex          open_lambda_arg_dot_m_close_rest ;
            (              "\\" ,arg_dot_m_close_rest) <- lex               lambda_arg_dot_m_close_rest ;
            (Variable            arg,dot_m_close_rest) <- readVariable             arg_dot_m_close_rest ;
            (                        ".",m_close_rest) <- lex                          dot_m_close_rest ;
            (                            m,close_rest) <- readsPrec d                      m_close_rest ;
            (                               ")" ,rest) <- lex                                close_rest ; return ( Abstract arg m , rest ) }

        readApplication open_m_n_close_rest = do {
            (           "(" ,m_n_close_rest) <- lex         open_m_n_close_rest ;
            (                m,n_close_rest) <- readsPrec d      m_n_close_rest ;
            (                  n,close_rest) <- readsPrec d        n_close_rest ;
            (                     ")" ,rest) <- lex                  close_rest ; return ( Apply m n , rest ) }


instance Show var => Show (Λ var) where

    show :: Show var => Λ var -> String

    show (Variable x) = show x

    show (Abstract arg m) = "( \\ " ++ show arg ++ " . " ++ show m ++ " )"

    show (Apply m n) = "( " ++ show m ++ " " ++ show n ++ " ) "