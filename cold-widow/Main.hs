module Main where

import System.Console.Program
import System.Console.Command
import qualified System.Console.Argument as Arg
import Control.Applicative ((<$))
import Control.Monad.IO.Class

import System.Directory (getAppUserDataDirectory)
import qualified Data.Tree as Tree

main :: IO ()
main = single program

program :: Commands IO

program = Node (command "cold-widow" "Transfer files via QRCodes" . io $ putStrLn "Use 'cold-widow help' for displaying usage information.")
          [
            Node (command "help" "Show usage information" . io $ showUsage program) [],
            Node (command "make-codes" "Generate qr-codes from the content of files, folders or standard input" makeCodesAction) [],
            Node (command "mk-codes" "Synonym for make-codes command" makeCodesAction) [],
            Node (command "mc" "Synonym for make-codes command" makeCodesAction) [],
            Node (command "receive" "Restore the content of files sent with qr-codes" receiveAction) [],
            Node (command "rcv" "Synonym for receive command" receiveAction)[],
            Node (command "r" "Synonym for receive command" receiveAction) [],
            Node (command "interactive" "Enter interactive mode" . io $ interactive program) []
          ]

makeCodesAction :: Action IO
makeCodesAction = withOption singleStepOption $
                  \ singleStep -> withOption tempFileOption $
                                  \ tempFile -> withOption blockSizeOption $
                                                \ blockSize -> withOption qrVersionOption $
                                                               \ qrVersion -> withOption levelOption $
                                                                              \ level -> withNonOptions Arg.file $
                                                                                        \ files -> io $ makeCodes singleStep tempFile blockSize qrVersion level files

receiveAction = io $ putStrLn "receive not implemented"


singleStepOption = Arg.option ['1'] ["fast", "single-step"] Arg.boolean False "Force single step execution, in memory"
tempFileOption = Arg.option ['t'] ["temporary-file"] Arg.boolean False "Force single step execution using a temporary file"
blockSizeOption = Arg.option ['s'] ["bs", "block-size"] maybeNatural Nothing "Use the specified block size instead of calculating the optimal blocksize"
qrVersionOption = Arg.option ['q'] ["qr-version"]  version AutoVersion "Use the specified qr code version instead of automatically determine the version"
levelOption = Arg.option ['l'] ["error-level"] errorLevel AutoLevel "Use the specified error-level instead of automatically determine the optimal level"

data VersionOption = Version Int | AutoVersion deriving Show
data LevelOption = LevelL | LevelM | LevelQ | LevelH | AutoLevel deriving Show

maybeNatural :: Arg.Type (Maybe Integer)
maybeNatural = Arg.Type (\ n -> Just <$> (Arg.parser Arg.natural) n ) "INT (natural)" Nothing 

version :: Arg.Type VersionOption
version = Arg.Type (parseVersionOption . Arg.parser Arg.natural) "INT" Nothing

parseVersionOption (Right x) | x >= 1 && x <= 40 = Right . Version $ fromInteger x
parseVersionOption (Right x) | otherwise = Left "QR code version must be between 1 and 40"
parseVersionOption (Left x) = Left x

errorLevel :: Arg.Type LevelOption
errorLevel = Arg.Type parseLevelOption "CHAR" Nothing

parseLevelOption "L" = Right LevelL
parseLevelOption "M" = Right LevelM
parseLevelOption "Q" = Right LevelQ
parseLevelOption "H" = Right LevelH
parseLevelOption _ = Left "Only L, M, Q or H levels are accepted for error-level argument"

makeCodes :: Bool -> Bool -> Maybe Integer -> VersionOption -> LevelOption -> [FilePath] -> IO ()
makeCodes singleStep tempFile blockSize qrVersion errLevel files = do
  configDir <- getAppUserDataDirectory $ name (Tree.rootLabel program)
  putStrLn $ "Config file location: " ++ configDir
  putStrLn "makeCodes called with:"
  putStrLn $ "   " ++ (show singleStep)
  putStrLn $ "   " ++ (show tempFile)
  putStrLn $ "   " ++ (show blockSize)
  putStrLn $ "   " ++ (show qrVersion)
  putStrLn $ "   " ++ (show errLevel)
  putStrLn $ "   " ++ (show files)
  

