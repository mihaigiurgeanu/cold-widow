module System.Console.ConfigFile
  (
    readFromFile
  ) where


import System.Console.Command
import System.Console.Internal

import           Control.Applicative    ((<$>))
import           Control.Exception      (tryJust)
import           Control.Monad          (guard)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Data.List              (isPrefixOf,concat,break)
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import           Data.Map               (Map)
import           Data.Maybe             (isJust)
import qualified Data.Tree       as Tree
import           System.Directory       (getAppUserDataDirectory)
import           System.IO.Error        (isDoesNotExistError)


readFromFile :: (MonadIO m) => Commands m -> UserCommand -> m [Setting]
readFromFile commands command = liftIO $ do
  dataDir <- getAppUserDataDirectory $ name (Tree.rootLabel commands)
  let configFile = dataDir ++ "/" ++ "config"
  fileContents <- either (const "") id <$> tryJust (guard . isDoesNotExistError) (readFile configFile)
  return . map parseSetting $ relevantLines command fileContents

relevantLines :: UserCommand -> String -> [String]
relevantLines c = concat . map snd . filter (flip isPrefixOf c . fst) . map parseSection . s . lines
 where
  
  s :: [String] -> [[String]]
  s = Split.split . Split.keepDelimsL $ Split.whenElt (isJust . header)  

  header :: String -> Maybe UserCommand
  header ('[' : xs) = Just . words . takeWhile (/= ']') $ xs
  header _          = Nothing
  
  parseSection :: [String] -> (UserCommand,[String])
  parseSection (h : rest) = case header h of
    Just c  -> (c,rest)
    Nothing -> ([],h : rest)
  parseSection []         = ([],[])

parseSetting :: String -> Setting
parseSetting s = let (i,v) = break (== '=') s in
  (
    Long i
  , case v of
      []         -> Nothing
      '=' : rest -> Just rest
  )
