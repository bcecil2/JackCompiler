import Lexer
import TokenParser
import Structure
import Statement
import Expression
import Tree
import qualified Data.Map.Strict as Map


push :: String -> Int -> String
push s i = "push "++s++" "++show i++"\n"

pop :: String -> Int -> String
pop s i = "pop "++s++" "++show i++"\n"

call :: String -> Int -> String
call s i = "call "++s++" "++show i++"\n"

return = "return\n"

--subroutineToTable
--getClassVars





