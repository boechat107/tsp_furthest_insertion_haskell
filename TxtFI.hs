{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where
import System.Environment
import ReadPoint
import FurthestInsertion

getPoints args =
   if (length args == 0)
   then stdinReadPoints
   else readPoints (args!!0)  

main =
    getArgs >>= 
    getPoints >>=
    startFurthestInsertion >>=
    mapM_ (\ (x, y) -> putStrLn (show  x ++ " " ++ show y)) 

-- Para imprimir distancias
{-
main =
    getArgs >>= 
    getPoints >>=
    startFurthestInsertion >>=
   \ x -> putStrLn (show  x)
-}
{-
vim:sw=4:ts=4:sm:et:autoindent:backspace=eol,start,indent:lines=35:columns=120
-}
