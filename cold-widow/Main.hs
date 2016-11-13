module Main where

import System.Console.Program
import System.Console.Command
import System.Console.Argument 
import Control.Applicative ((<$))

main :: IO ()
main = single program

program :: Commands IO

program = Node (command "cold-widow" "Transfer files via QRCodes" . io $ interactive program)
          [
            Node (command "help" "Show usage information" . io $ showUsage program) [],
            Node (command "make-codes" "Generate qr-codes from the content of files, folders or standard input" makeCodesAction) [],
            Node (command "mk-codes" "Synonym for make-codes command" makeCodesAction) [],
            Node (command "mc" "Synonym for make-codes command" makeCodesAction) [],
            Node (command "receive" "Restore the content of files sent with qr-codes" receiveAction) [],
            Node (command "rcv" "Synonym for receive command" receiveAction)[],
            Node (command "r" "Synonym for receive command" receiveAction) []
          ]

makeCodesAction = io $ putStrLn "make-codes not implemented"
receiveAction = io $ putStrLn "receive not implemented"


singleStepOption = option ['1'] ["fast", "single-step"] boolean False "Force single step execution, in memory"
tempFileOption = option ['t'] ["temporary-file"] boolean False "Force single step execution using a temporary file"
blockSizeOption = option ['s'] ["bs", "block-size"] maybeNatural Nothing "Use the specified block size instead of calculating the optimal blocksize"
qrVersionOption = option ['q'] ["qr-version"]  version AutoVersion "Use the specified qr code version instead of automatically determine the version"
levelOption = option ['l'] ["error-level"] errorLevel AutoLevel "Use the specified error-level instead of automatically determine the optimal level"

data VersionOption = Version Int | AutoVersion
data LevelOption = LevelL | LevelM | LevelQ | LevelH | AutoLevel

maybeNatural :: Type (Maybe Integer)
maybeNatural = Type (\ n -> Just <$> (parser natural) n ) "INT (natural)" Nothing 

version :: Type VersionOption
version = Type (parseVersionOption . parser natural) "QR Code Version" Nothing

parseVersionOption (Right x) | x >= 1 && x <= 40 = Right . Version $ fromInteger x
parseVersionOption (Right x) | otherwise = Left "QR code version must be between 1 and 40"
parseVersionOption (Left x) = Left x

errorLevel :: Type LevelOption
errorLevel = Type parseLevelOption "QR Code Error correction level" Nothing

parseLevelOption "L" = Right LevelL
parseLevelOption "M" = Right LevelM
parseLevelOption "Q" = Right LevelQ
parseLevelOption "H" = Right LevelH
parseLevelOption _ = Left "Only L, M, Q or H levels are accepted for error-level argument"

