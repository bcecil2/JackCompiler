module Lexer where

import Data.List
import Data.Char

data Token = Keyword String
               | Symbol String 
	       | IntConst Int
	       | SConst String
	       | Id String deriving(Eq,Read)

instance Show Token where
    show (Keyword s) = "<keyword> "++s++" </keyword>"
    show (Symbol "<") = "<symbol> "++"&lt;"++" </symbol>"
    show (Symbol ">") = "<symbol> "++"&gt;"++" </symbol>"
    show (Symbol "\"") = "<symbol> "++"&qot;"++" </symbol>"
    show (Symbol "&") = "<symbol> "++"&amp;"++" </symbol>"
    show (Symbol s) = "<symbol> "++s++" </symbol>"
    show (IntConst i) = "<integerConstant> "++(show i)++" </integerConstant>"
    show (SConst s) = "<stringConstant> "++s++" </stringConstant>"
    show (Id s) = "<identifier> "++s++" </identifier>"

type File = [String]


isKeyword :: String -> Bool
isKeyword x = x `elem` ["class","constructor","function",
			"method","field","static","var",
			"int","char","boolean","void",
		        "true","false","null","this",
			"let","do","if","else","while",
			"return"]
isSymb :: String -> Bool
isSymb x = x `elem` ["{","}","(",")","[","]",
                       ".",",",";","+","-","/",
		       "&","|","<",">","=","-","~","*"]


isId :: String -> Bool
isId "" = False
isId s = not (isDigit $ head s) && s /= "" && (and $ map (isAlphaNum) s)

isIntConst :: String -> Bool
isIntConst s = s /= "" && (and $ map (isDigit) s)

isStringLit :: String -> Bool
isStringLit s = s /= "" && head s == '\"' && last s == '\"' && length s > 1

toToken :: String -> Token
toToken processed
   | isSymb processed = Symbol processed
   | isKeyword processed = Keyword processed
   | isId processed = Id processed
   | isIntConst processed = IntConst (read processed)
   | isStringLit processed = SConst (reverse $ tail $ reverse $ tail processed)
   | otherwise = Symbol ("FUCKED UP" ++ processed) 

toTokens :: (String,String,[Token]) -> [Token]
toTokens (processed,left,toks)
   | null processed && null left = toks
   | null left = toks ++ [toToken processed] 
   | isSymb processed = toTokens ("",left,toks++[Symbol processed])
   | isKeyword processed && ((head left `elem` " ;)")) = toTokens ("",left, toks++[Keyword processed])
   | isId processed && not (isAlphaNum (head left)) = toTokens ("", left, toks++[Id processed]) 
   | isIntConst processed && not (isDigit (head left)) = toTokens ("",left,toks++[IntConst (read processed)])
   | isStringLit processed = toTokens("",left,toks++[SConst (reverse $ tail $ reverse $ tail processed)])
   | head left == ' ' && not ('\"' `elem` processed) = toTokens(processed,tail left, toks)
   | otherwise = toTokens(processed++[head left],tail left,toks)

lexF :: File -> [Token]
lexF = concatMap (\s -> toTokens ("",s,[]))

