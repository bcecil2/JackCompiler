module Structure where

import TokenParser 
import Lexer
import Control.Monad 
import Control.Applicative 
import Statement

parseVarList :: Parser ParseTree 
parseVarList =  do 
                   list <- many0 $ parseName `mplus` parseLit (Symbol ",")
                   return $ Node {name = "", children = list} -- leaving name blank for now
parseVarDec :: Parser ParseTree 
parseVarDec = do
                start <- parseLit (Keyword "var")
		t <- parseType
		v <- parseName
		list <- parseVarList
		end <- parseSemiC
		return Node {name = "varDec", children = [start,t,v,list,end]}

pList :: Parser ParseTree
pList = do
          t <- parseType 
	  name <- parseName  
	  xs <- many0 (parseLit (Symbol ",") `mplus` parseType `mplus` parseName)
	  return Node {name = "parameterList", children = [t,name, Node{name = "", children = xs}]}

parseParameterList :: Parser ParseTree
parseParameterList = pList <|> pure Node {name = "parameterList", children = []} 

parseSDStart :: Parser ParseTree
parseSDStart = parseLit (Keyword "constructor") <|>
               parseLit (Keyword "function") <|>
	       parseLit (Keyword "method")

parseSubroutineDec :: Parser ParseTree
parseSubroutineDec = do
                       start <- parseSDStart
		       t <- (parseLit (Keyword "void") <|> parseType)
		       name <- parseName
		       oP <- parseLit (Symbol "(")
		       paramL <- parseParameterList 
		       cP <- parseLit (Symbol ")")
		       body <- parseSubroutineBody
		       return Node {name = "subroutineDec", children = [start,t,name,oP,paramL,cP,body]}



parseSubroutineBody :: Parser ParseTree
parseSubroutineBody = do 
                        s <- parseLit (Symbol "{")
			varDecs <- many0 parseVarDec
			stmts <- parseStatements
			e <- parseLit (Symbol "}")
			return Node {name ="subroutineBody", children = [s,Node {name ="", children = varDecs},stmts,e]}

parseClassVarDec :: Parser ParseTree
parseClassVarDec = do 
                     s <- parseLit (Keyword "static") <|> parseLit (Keyword "field")
		     t <- parseType 
		     name <- parseName
		     vs <- many0 (parseLit (Symbol ",") `mplus` parseName)
		     e <- parseLit (Symbol ";")
		     return Node {name = "classVarDec", children = [s,t,name,Node{name = "", children = vs},e]}

parseClass :: Parser ParseTree
parseClass = do
               s <- parseLit (Keyword "class")
	       name <- parseName
	       oB <- parseLit (Symbol "{") 
	       cVarDec <- many0 parseClassVarDec
	       subroutineDec <- many0 parseSubroutineDec
	       e <- parseLit (Symbol "}")
	       return Node {name ="class", children = [s,name,oB,Node {name ="", children = cVarDec}, Node{name ="", children = subroutineDec},e]}


