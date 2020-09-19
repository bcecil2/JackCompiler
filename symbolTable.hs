module SymbolTable where


import qualified Data.Map.Strict as Map

type TableEntry = (String,String,Int)

type SymbolTable = Map.Map String TableEntry



