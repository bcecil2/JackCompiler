module Expression where

import TokenParser
import Lexer
import Control.Monad 
import Control.Applicative 


isKeywordConst :: Token -> Bool
isKeywordConst t = t `elem` [(Keyword "true")
		    ,(Keyword "false")
		    ,(Keyword "null")
		    ,(Keyword "this")]

isBinOp :: Token -> Bool
isBinOp t = t `elem` [   (Symbol "+")
			,(Symbol "-")
			,(Symbol "*")
			,(Symbol "/")
			,(Symbol "&")
			,(Symbol "|")
			,(Symbol "<")
			,(Symbol ">")
			,(Symbol "<")
			,(Symbol "=")]


isUnOp :: Token -> Bool
isUnOp t = t `elem` [Symbol "-", Symbol "~"]

parseKeywordConst :: Parser ParseTree
parseKeywordConst = Lit <$> sat isKeywordConst 

parseBinOp :: Parser ParseTree
parseBinOp = Lit <$> sat isBinOp

parseUOp :: Parser ParseTree
parseUOp = Lit <$> sat isUnOp


parseSimpleTerm :: Parser ParseTree
parseSimpleTerm = parseInt <|>
                  parseString <|>
		  parseKeywordConst <|>
		  parseName 

parseArrayTerm :: Parser ParseTree
parseArrayTerm = do 
                   s <- parseName
		   openB <- parseLit (Symbol "[")
		   expr <- parseExpr
		   closeb <- parseLit (Symbol "]")
		   return Node {name = "", children = [s,openB,expr,closeb]}

parseUOpTerm :: Parser ParseTree
parseUOpTerm = do
                 uOp <- parseUOp
		 t <- parseTerm 
		 return Node {name = "", children = [uOp,t]}


parseSCall :: Parser ParseTree
parseSCall = do 
                 sName <- parseName
		 oP <- parseLit (Symbol "(")
		 exprL <- parseExprList
                 cP <- parseLit (Symbol ")")
                 return Node {name ="", children = [sName,oP,exprL,cP]}

parseCCall :: Parser ParseTree
parseCCall = do
               name <- parseName
	       dot <- parseLit (Symbol ".")
	       sName <- parseName
	       oP <- parseLit (Symbol "(")
	       exprL <- parseExprList
               cP <- parseLit (Symbol ")")
               return Node {name ="", children = [name,dot,sName,oP,exprL,cP]}




parseSubroutineCall :: Parser ParseTree 
parseSubroutineCall = parseSCall <|> parseCCall

parseExprTerm :: Parser ParseTree
parseExprTerm = do
		  oP <- parseLit (Symbol "(")
	          expr <- parseExpr
                  cP <- parseLit (Symbol ")")
		  return  Node {name = "", children = [oP,expr,cP]}

parseTerm :: Parser ParseTree
parseTerm = do 
            e <- parseArrayTerm <|> parseSubroutineCall <|> parseSimpleTerm <|> parseExprTerm <|> parseUOpTerm
	    return Node {name = "term", children = [e]}

parseExprList :: Parser ParseTree
parseExprList = eList <|> pure (Node "expressionList" [])
             where eList = do
	                     expr <- parseExpr
			     exprs <- many0 (parseLit (Symbol ",") `mplus` parseExpr)
			     return Node {name = "expressionList", children = [expr, Node {name = "", children = exprs}]}

parseExpr :: Parser ParseTree
parseExpr = do 
               t <- parseTerm
               ts <- many0 (parseBinOp `mplus` parseTerm)
	       return Node {name = "expression", children = [t, Node{name = "", children = ts}]}
