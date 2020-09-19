module Tree where

import Lexer
import TokenParser
import Structure
import Statement
import Expression
import qualified Data.Map.Strict as Map

type TableEntry = (String,String,Int)

type SymbolTable = Map.Map String TableEntry


buildTree :: [Token] -> [(ParseTree,[Token])]
buildTree = run parseClass 

-- Turning off show helps for debugging because the pretty printing
-- hides a bunch of nested trees that have name ""
--
-- might not be possible to lift these trees so be careful
-- when writing functions that map over the tree since 
-- it might accidentally stop before collecting from these 
-- nameless subtrees

instance Show ParseTree where
   show Empty = ""
   show (Lit x) = show x ++ "\n"
   show (Node "" c) = concatMap show c
   show (Node n c) = ("<"++n++">\n") ++ (concatMap show c) ++ "</"++n++">\n"


subTreesByName :: (String -> Bool) -> ParseTree -> [ParseTree]
subTreesByName p t@(Node s l)
             | p s = t:(concatMap (subTreesByName p) l)
	     | otherwise = concatMap (subTreesByName p) l
subTreesByName _  _ = []  

nodesByToken :: (Token -> Bool) -> ParseTree -> [ParseTree]
nodesByToken p (Lit x)
           | p x = [Lit x]
	   | otherwise = []
nodesByToken p (Node _ l) = concatMap (nodesByToken p) l
nodesByToken _ _ = []

-- Multiple subtelties:
-- 
-- any parameterList that belongs to a method has to have
-- a "this" appear along with the rest of the args and we need
-- to deduce the class (probably shouldnt be hard)
-- 
-- we need to know the types of each variable were putting in
--
--var int x,y,z we will have to infer the type somehow

getIds :: ParseTree -> [Token] 
getIds Empty = []
getIds (Lit (Id s)) = [Id s]
getIds (Lit _) = []
getIds (Node _ c) = concatMap (getIds) c

toString (Id s) = s
toString (Keyword s) = s

-- expects a varDec
-- decType type name 
getVars :: ParseTree -> [(String,String,String)]
getVars (Node s ((Lit var):(Lit t):xs))
        | s == "varDec" || s == "classVarDec" = map (\x -> (toString var,toString t,x)) vars
        where vars = map toString $ concatMap (getIds) xs  

isSymbol (Lit (Symbol _)) = True
isSymbol _ = False

toPairs :: [ParseTree] -> [(String,String)]
toPairs [] = []
toPairs ((Lit (Id x)):(Lit (Id y)):xs) = (x,y):toPairs xs
toPairs ((Lit (Keyword x)):(Lit (Id y)):xs) = (x,y):toPairs xs
toPairs ((Lit (Symbol _)):xs) = toPairs xs
toPairs ((Node "" c):xs) = (toPairs c)++(toPairs xs)
toPairs _ = []

-- expects a parameterlist as a tree
toArgs :: Int -> ParseTree -> [(String,String,Int)]
toArgs idx (Node "parameterList" l) = map (\((x,y),i) -> (x,y,i)) withCount 
                                 where vars = toPairs l  
				       withCount = zip vars [idx..(idx + (length vars))]

maxIndx :: [(String,String,Int)] -> Int
maxIndx [] = 0
maxIndx l = let (_,_,i) = last l in i

-- expects a parameterlist as a tree
isMethod :: ParseTree -> Bool
isMethod (Node "parameterList" ((Lit (Keyword "method"):_))) = True
isMethod _ = False

-- expects a subrotuine as a tree 
subroutineToTable :: String -> ParseTree -> SymbolTable
subroutineToTable s t = let 
			   varDecs = subTreesByName (=="varDec") t
			   args = subTreesByName (=="parameterList") t
                           
			   vars = concatMap (getVars) varDecs
			   vis = map (\((d,t,n),i) -> (d,t,n,i)) $ zip vars [0..(length vars - 1)]
			   a = foldl (\x y -> if isMethod y then x++((s,"this",0):(toArgs 1 y)) 
			                         else x++(toArgs 0 y)) [] args  
			   varST = Map.fromList $ map (\(d,t,n,i) -> (n,(t,d,i)))  vis
			   argST = Map.fromList $ map (\(t,n,i) -> (n,(t,"argument",i)))  a
		       in
		          Map.union varST argST 

                       
getClassName :: ParseTree -> String
getClassName (Node "class" (x:(Lit(Id s)):_)) = s

-- expects the full parse tree 
getSubroutineVars :: ParseTree -> [SymbolTable]
getSubroutineVars t = map (subroutineToTable (getClassName t)) $ subTreesByName (=="subroutineDec") t

getClassVars :: ParseTree -> SymbolTable
getClassVars t = let
                     vars = concatMap (getVars) $ subTreesByName(=="classVarDec") t
		     vis = map (\((d,t,n),i) -> (d,t,n,i)) $ zip vars [0..(length vars - 1)]
		     varST = Map.fromList $ map (\(d,t,n,i) -> (n,(t,d,i)))  vis
		 in
		    varST 


