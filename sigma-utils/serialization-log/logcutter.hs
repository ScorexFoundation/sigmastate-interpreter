import System.Environment
import Data.String.Utils

maxNumber = -1
startHeader = "[Start][Serialize]"
endHeader = "[End][Serialize]"
-- use (==) for exact equality, TODO: add this option to CLI
tagComp = (==) --startswith


getNextLines :: Int -> String -> String -> [String] -> ([String], [String])
getNextLines _ _ _ [] = ([], [])
getNextLines sn fh s (x:xs) = if ((tagComp (fh++s) (strip x)) && (getHeadingSpaces x == sn)) then ([x], xs)
                              else let (y, xs') = getNextLines sn fh s xs in
                                   (x:y, xs')

getHeadingSpaces :: String -> Int
getHeadingSpaces s = let s' = takeWhile (== ' ') s in
                     let n = length s' in n

findAllTag :: Integer -> String -> String -> String -> [String] -> [[String]]
findAllTag _ _ _ _ [] = []
findAllTag n sh fh s (x:xs) = if (n == maxNumber) then [] else
                                if (tagComp (sh++s) (strip x)) then                                  
                                  let (fs, r) = getNextLines (getHeadingSpaces x) fh s xs in
                                      (x:fs):(findAllTag (n+1) sh fh s r)
                                else
                                  findAllTag n sh fh s xs

trimMin :: [String] -> [String]
trimMin [] = []
trimMin ss@(s:xs) = let s' = takeWhile (== ' ') s in
                    let n = length s' in
                     map (drop n) ss

main = do
   args <- getArgs
   let fname  = args !! 0
   let tag = args !! 1
   content <- readFile fname
   let l = lines content
   let tl = map trimMin $ findAllTag 0 startHeader endHeader tag l
   mapM_ putStrLn $ concat tl
