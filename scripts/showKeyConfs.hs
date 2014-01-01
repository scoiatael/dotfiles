#!/bin/runghc

import Data.List
import System.Directory

xconfigPath=".xmonad/xmonad.hs"

newtype RString = RString { toString :: String } 
instance Read RString where
  readsPrec _ x = map (\(a,b) -> (RString a, b)) $ map (flip selectN x) [length x - 1 .. 0]  

selectN 0 x  = ([], x)
selectN a (x:xs) = let (p,t) = selectN (a-1) xs in (x:p, t) 

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a, f b)

getKeys = concatMap (formatPairs.makePairs) . lines . findKeyConf
findKeyConf = snd . foldl (\(x,text) ln -> let 
    x' = if ln == "--EndOfKeyConfig" then False else if ln == "--KeyConfig" then True else x;
    text' = (if x then ln++"\n" else []) ++ text
  in (x',text')) (False,[]) . lines
makePairs :: String -> (String,String)
makePairs = (\x -> both toString $ (read x :: (RString, RString))) . getTextBetweenParen . drop 1 .dropWhile (/='(')
getTextBetweenParen = snd . foldl (\(x,text) char -> if x==0 
  then (x,text) 
  else (if char=='(' 
    then x+1 
    else if char==')' 
      then x-1
      else x, char:text)) (1,[])
formatPairs (k,v) = " " ++ k ++ " -> " ++ v ++ " \n"

main = do
  f <- readFile xconfigPath
  putStrLn $ getKeys f
