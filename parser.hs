module Parser where

import Lexer

--data Token = Keyword String
--               | Symbol String 
--	       | IntConst Int
--	       | SConst String
--	       | Id String

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (x:xs) = x

accept :: [Token] -> [Token]
accept [] = error "fuck"
accept (x:xs) = xs

 

