module Statement where

import TokenParser
import Expression
import Lexer
import Control.Monad 
import Control.Applicative 

parseReturn :: Parser ParseTree
parseReturn = do
                r <- parseLit (Keyword "return")
		expr <- parseExpr <|> pure Empty
		s <- parseLit (Symbol ";")
		return Node {name = "returnStatement", children = [r,expr,s]}

parseDo :: Parser ParseTree
parseDo = do
            d <- parseLit (Keyword "do")
            sCall <- parseSubroutineCall 
            s <- parseLit (Symbol ";")
	    return Node {name = "doStatement", children = [d,sCall,s]}

parseWhile :: Parser ParseTree
parseWhile = do
               w <- parseLit (Keyword "while")
               oP <- parseLit (Symbol "(")
	       expr <- parseExpr
	       cP <- parseLit (Symbol ")")
	       oB <- parseLit (Symbol "{")
	       stmts <- parseStatements
	       cB <- parseLit (Symbol "}")
               return Node {name = "whileStatement", children = [w,oP,expr,cP,oB,stmts,cB]}

parseElse :: Parser ParseTree
parseElse = do 
              el <- parseLit (Keyword "else")
              oB <- parseLit (Symbol "{")
	      stmts <- parseStatements
	      cB <- parseLit (Symbol "}")
              return Node {name = "", children = [el,oB,stmts,cB]}

parseIf :: Parser ParseTree
parseIf = do
            i <- parseLit (Keyword "if") 
	    oP <- parseLit (Symbol "(")
	    expr <- parseExpr
	    cP <- parseLit (Symbol ")")
	    oB <- parseLit (Symbol "{")
	    stmts <- parseStatements
	    cB <- parseLit (Symbol "}")
            el <- parseElse <|> pure Empty
	    return Node {name = "ifStatement", children = [i,oP,expr,cP,oB,stmts,cB,el]}


parseLetArray :: Parser ParseTree
parseLetArray = do
                  oB <- parseLit (Symbol "[")
	          stmts <- parseExpr
	          cB <- parseLit (Symbol "]")
		  return Node {name = "", children = [oB,stmts,cB]}

parseLet :: Parser ParseTree
parseLet = do
             lt <- parseLit (Keyword "let") 
             name <- parseName
	     a <- parseLetArray <|> pure Empty
	     eq <- parseLit (Symbol "=") 
	     expr <- parseExpr
	     s <- parseLit (Symbol ";")
	     return Node {name = "letStatement", children = [lt,name,a,eq,expr,s]}


parseStatement :: Parser ParseTree
parseStatement = parseLet <|>
                  parseIf <|>
		  parseWhile <|>
		  parseDo <|>
		  parseReturn

parseStatements :: Parser ParseTree
parseStatements = do
                    stmts <- many0 parseStatement
		    return Node {name = "statements", children = stmts}


