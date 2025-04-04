module AbbreviatedLambdaExpression (
    AbbreviatedΛ(Abstracts, Applications),
    abbreviate,
    formalize
) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import qualified LambdaExpression as L

data AbbreviatedΛ var =
    Variable var
    | Abstracts [ var ] ( AbbreviatedΛ var ) -- \xs.E
    | Applications [ AbbreviatedΛ var ] -- Ms in reverse order

instance Read var => Read ( AbbreviatedΛ var ) where

    readsPrec :: Read var => Int -> ReadS (AbbreviatedΛ var)

    readsPrec d expression_rest = readOne expression_rest <|> readMany expression_rest where

        readOne , readMany , readBracket , readVariable , readAbstracts , readApplications :: ReadS ( AbbreviatedΛ var )

        readOne oneExpression_rest =  readBracket oneExpression_rest <|> readVariable oneExpression_rest <|> readAbstracts oneExpression_rest

        readMany = readApplications

        readBracket open_body_close_rest = do {
            (       "(" ,body_close_rest) <- lex         open_body_close_rest ;
            (            body,close_rest) <- readsPrec d      body_close_rest ;
            (                  ")" ,rest) <- lex                   close_rest ; return ( body , rest ) }

        readVariable spaces_var_rest = do {
            (          _   ,var_rest) <- pure $ span isSpace spaces_var_rest ;
            (               var,rest) <- readsPrec d                var_rest ; return ( Variable var , rest ) }

        readAbstracts lambda_arg_args_dot_m_rest = do {
            (          "\\" ,arg_args_dot_m_rest) <- lex                                           lambda_arg_args_dot_m_rest ;
            (Variable        arg,args_dot_m_rest) <- readVariable                                         arg_args_dot_m_rest ;
                do {
            (                         ".",m_rest) <- lex                                                      args_dot_m_rest ;
            (                             m,rest) <- filter unextendable $ readsPrec d                                 m_rest ; return ( Abstracts [ arg        ] m , rest ) }
            <|> do {
            (Abstracts           args     m,rest) <- filter unextendable $ readAbstracts $ "\\" ++            args_dot_m_rest ; return ( Abstracts ( arg : args ) m , rest ) } }

        readApplications    m_ns_rest = do {                                             ;
            (               m,ns_rest ) <- readOne                       m_ns_rest ;
                do {
            (                 n ,rest ) <- filter unextendable $ readOne   ns_rest ; return ( Applications [ n  ,    m ]   ,   rest ) }
            <|> do {
            (Applications     ns,rest ) <- readApplications                ns_rest ; return ( Applications ( ns ++ [ m ] ) ,   rest ) } }

        unextendable ( parsed , rest ) = null $ readsPrec d rest `asTypeOf` [ ( parsed , rest ) ]


instance Show var => Show (AbbreviatedΛ var) where

    show :: Show var => AbbreviatedΛ var -> String

    show = deBracket . preShow where

        deBracket ('(':' ':s) = init $ init s ; deBracket s = s -- (E) -> E

        preShow (Variable x) = show x

        preShow (Abstracts args m) = "( \\ " ++ unwords (show <$> args) ++ " . " ++ show m ++ " )" -- \x.(E) -> \x.E

        preShow (Applications ( m@(Abstracts _ _) : ms ) ) = "( " ++ unwords (preShow <$> reverse ms) ++ " " ++ show m ++ " )" -- Ms (\x.E) -> Ms\x.e

        preShow (Applications ms) = "( " ++ unwords (preShow <$> reverse  ms) ++ " )"


-- | apply abbreviation rule

abr :: Show var => AbbreviatedΛ var -> AbbreviatedΛ var

abr (Abstracts [arg] (Abstracts args m)) = Abstracts (arg:args) m -- \x.\ys.M -> \x ys.M

abr (Abstracts [arg] e) = Abstracts [arg] e

abr (Applications [n,Applications ms]) = Applications (n:ms) -- (Ms)N -> Ms N

abr (Applications ms) = Applications ms

abr x = error ( "no abr rule for expression : " ++ show x )


abbreviate :: Show var => L.Λ var -> AbbreviatedΛ var

abbreviate (L.Variable x) = Variable x

abbreviate (L.Abstract arg m) = abr ( Abstracts [arg] (abbreviate m) )

abbreviate (L.Apply m n) = abr ( Applications [abbreviate n, abbreviate m] )


formalize :: Show var => AbbreviatedΛ var -> L.Λ var

formalize (Variable x) = L.Variable x

formalize (Abstracts [        ] m) = error ( "no arguments given in abstractions : " ++ show (Abstracts [        ] m) )

formalize (Abstracts [arg     ] m) = L.Abstract arg (formalize                  m)

formalize (Abstracts (arg:args) m) = L.Abstract arg (formalize $ Abstracts args m)

formalize (Applications [    ]) = error "no expressins in applications"

formalize (Applications [n   ]) = error ( "omly one expression in applications : " ++ show (Applications [n   ]) )

formalize (Applications [n,m ]) = L.Apply (formalize                m ) (formalize n)

formalize (Applications (n:ms)) = L.Apply (formalize $ Applications ms) (formalize n)