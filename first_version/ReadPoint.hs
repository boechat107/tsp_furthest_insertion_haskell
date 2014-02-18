{-# OPTIONS_GHC -fglasgow-exts #-}
module ReadPoint where
import System.IO
type Point = (Double,Double)

parseDouble:: (ReadS Double) = reads

readPoint:: String -> Point
readPoint str = 
	let (x,str0) = head.parseDouble $ str
	    (y,_) = head.parseDouble $ str0
	in (x,y)

readPoints file = 
	readFile file >>=
	return.lines >>=
	return.map readPoint

stdinReadPoints = 
    hGetContents stdin >>=
    return . lines >>=
    return . map readPoint

dist (x0,y0) (x1,y1) = sqrt $ (x0-x1)**2.0 + (y0-y1)**2.0
