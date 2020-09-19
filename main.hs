import System.Environment
import System.Directory
import Control.Monad
import Cleaner
import Lexer
import System.IO
import TokenParser
import Tree
import Statement
import Structure
endsWith :: String -> Bool
endsWith s@(x:xs)
       | length s > 5 = endsWith xs
       | otherwise = s == ".jack"   

deSpace :: [String] -> [String]
deSpace = filter (not . null)

prepend :: String -> [String] -> [String]
prepend p = map (p++)

tokens :: String -> [Token]
tokens file = lexF $ deSpace $ filter (removeLine)  $ map (cleanLine) (lines file)

tokFile :: String -> (String,[Token])
tokFile file = ("<tokens>\n"++(unlines $ map (show) t)++"</tokens>\n",t)
           where t = tokens file 

parse :: [Token] -> (String, ParseTree)
parse toks = (show t,t)
      where t = fst $ head $ buildTree toks 


main = do
         (x:_) <- getArgs
	 isDir <- doesDirectoryExist x
	 if isDir
	 then do
	            files <- listDirectory x
		    let jackFiles =  filter (endsWith) $ prepend (x++"\\") files
		    contents <- mapM (readFile) jackFiles
		    let f = map (tokFile) contents
		        strings = map fst f
			toks = map snd f
			trees = map parse toks
			tString = map fst trees
		    mapM_ (\(f,s) -> writeFile ((reverse $ drop 5 $ reverse f) ++ "MT.xml") s) $ zip jackFiles strings  
		    mapM_ (\(f,s) -> writeFile ((reverse $ drop 5 $ reverse f) ++ "M.xml") s) $ zip jackFiles tString 
		    else do 
                    file <- readFile x
		    let (s,t) = tokFile file
		        (stree,tree) = parse t
			cName = getClassName tree
		    --print tree
                    writeFile ((reverse $ drop 5 $ reverse x) ++ "MT.xml") s
		    writeFile ((reverse $ drop 5 $ reverse x) ++ "M.xml") stree
		    print $ subTreesByName (=="expression") tree

