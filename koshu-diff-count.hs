{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Data.Algorithm.Diff           as D
import qualified Data.List                     as L
import qualified System.FilePath               as S
import qualified System.Directory              as S
import qualified System.Environment            as S
import qualified Koshucode.Baala.Base          as K
import qualified Koshucode.Baala.Core          as K
import qualified Koshucode.Baala.Type.Vanilla  as K


-- --------------------------------------------  Main

main :: IO ()
main =
    do args <- S.getArgs
       case args of
         [old, new] -> pathDiffMain old new
         _ -> error "USAGE: koshu-diff-count OLD NEW"

pathDiffMain :: FilePath -> FilePath -> IO ()
pathDiffMain old new =
    do oldFile <- S.doesFileExist old
       newFile <- S.doesFileExist new
       oldDir  <- S.doesDirectoryExist old
       newDir  <- S.doesDirectoryExist new
       dispatch (oldFile && newFile) (oldDir && newDir)
    where
      dispatch True False  = fileDiffMain old new
      dispatch False True  = dirDiffMain  old new
      dispatch _ _         = error $ "No such files or directory: "
                                     ++ show old ++ " or " ++ show new


-- --------------------------------------------  Counter

data DiffCount =
    DiffCount { diffCountFirst  :: Int
              , diffCountSecond :: Int
              , diffCountBoth   :: Int
              } deriving (Show, Eq, Ord)

diffCount ::  [D.Diff a] -> DiffCount
diffCount = result . foldr count (0, 0, 0) where
    count (D.Both _ _) (f, s, b) = (f, s, b + 1)
    count (D.First _)  (f, s, b) = (f + 1, s, b)
    count (D.Second _) (f, s, b) = (f, s + 1, b)

    result (f, s, b) = DiffCount f s b


-- --------------------------------------------  File

fileDiffMain :: FilePath -> FilePath -> IO ()
fileDiffMain old new =
    do oldc <- readFile old
       newc <- readFile new
       let d = fileDiff oldc newc
       putJudge $ diffCountJudge old new $ diffCount d

fileDiff :: String -> String -> [D.Diff String]
fileDiff old new = fileLines old `D.getDiff` fileLines new

fileLines :: String -> [String]
fileLines = filter (/= "") . map K.trimBoth . lines


-- --------------------------------------------  Directory

dirDiffMain :: FilePath -> FilePath -> IO ()
dirDiffMain old new =
    do oldFiles <- getRecursiveContents old
       newFiles <- getRecursiveContents new
       let diff    = D.getDiffBy eq oldFiles newFiles
           eq o n  = L.stripPrefix old o == L.stripPrefix new n
       mapM_ dirDiffSub diff

dirDiffSub :: D.Diff String -> IO ()
dirDiffSub (D.Both old new)  = fileDiffMain old new
dirDiffSub (D.First old)     = fileOld old
dirDiffSub (D.Second new)    = fileNew new

fileOld :: FilePath -> IO ()
fileOld old = do del <- countLines old
                 putJudge $ sideCountJudge (K.pText old) K.empty del 0

fileNew :: FilePath -> IO ()
fileNew new = do add <- countLines new
                 putJudge $ sideCountJudge K.empty (K.pText new) 0 add

countLines :: FilePath -> IO Int
countLines path =
    do s <- readFile path
       return $ length $ lines s

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents dir =
    do names <- S.getDirectoryContents dir
       let properNames = filter (`notElem` [".", ".."]) names
       paths <- mapM rec properNames
       return $ concat paths
    where
      rec name =
          do let path = dir S.</> name
             isDirectory <- S.doesDirectoryExist path
             case isDirectory of
               True  -> getRecursiveContents path
               False -> return [path]

-- --------------------------------------------  Judgement

diffCountJudge :: String -> String -> DiffCount -> K.JudgeC
diffCountJudge old new d = countJudge old' new' cnt where
    old' = K.pText old
    new' = K.pText new
    cnt  = ( K.pInt $ diffCountBoth d
           , K.pInt $ diffCountFirst d
           , K.pInt $ diffCountSecond d)

sideCountJudge :: K.VContent -> K.VContent -> Int -> Int -> K.JudgeC
sideCountJudge old new delete add = countJudge old new cnt where
        cnt = (K.pInt 0, K.pInt delete, K.pInt add)

countJudge :: c -> c -> (c, c, c) -> K.Judge c
countJudge old new (unchange, delete, add) = K.affirm "DIFF-COUNT" xs where
    xs = [ term "unchange" unchange
         , term "delete"   delete
         , term "add"      add
         , term "old"      old
         , term "new"      new ]

writeJudge :: K.JudgeC -> String
writeJudge = K.writeDownJudge K.shortEmpty

putJudge :: K.JudgeC -> IO ()
putJudge = putStrLn . writeJudge

term :: String -> c -> (String, c)
term n c = (n, c)

