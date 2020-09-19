module TokenParser where

import Lexer
import Control.Monad 
import Control.Applicative 

newtype Parser a = Parser {run :: [Token] -> [(a,[Token])]}

data ParseTree  = Empty | Lit {tok :: Token} | Node {name :: String, children :: [ParseTree]} deriving(Read) 
       

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure a = Parser (\toks -> [(a,toks)])
    (<*>) = ap

instance Monad Parser where
    return = pure 
    p >>= f = Parser (\toks -> concat [run (f a) toks' | (a,toks') <- run p toks])   

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Parser where
    mzero = Parser (\toks -> [])
    mplus p q  = Parser (\toks -> run p toks ++ run q toks)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\toks -> case run (p `mplus` q) toks of
                               []     -> []
			       (x:xs) -> [x])

sat :: (Token -> Bool) -> Parser Token 
sat p = do
          t <- singleTok
	  if p t then return t 
	  else mzero

singleTok :: Parser Token 
singleTok = Parser f
         where f [] = []
	       f (x:xs) = [(x,xs)]
	
many0 :: Parser a -> Parser [a]
many0 p = many1 p +++ return []

many1 p = do
            a <- p
	    as <- many0 p
	    return (a:as)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do a <- p
                    as <- many0 (do{sep;p})
		    return (a:as)

parseLit :: Token -> Parser ParseTree  
parseLit t = Lit <$> sat (==t)


isType :: Token -> Bool
isType (Keyword "int") = True
isType (Keyword "char") = True
isType (Keyword "boolean") = True
isType (Id _) = True
isType _ = False

isName :: Token -> Bool
isName (Id _) = True
isName _ = False

parseName :: Parser ParseTree
parseName = Lit <$> sat isName


parseSemiC :: Parser ParseTree
parseSemiC = parseLit (Symbol ";")

parseType :: Parser ParseTree
parseType = Lit <$> sat isType

parseInt :: Parser ParseTree
parseInt = Lit <$> sat (f)
       where f (IntConst _) = True
             f _ = False
parseString :: Parser ParseTree
parseString = Lit <$> sat (f)
       where f (SConst _) = True
             f _ = False

