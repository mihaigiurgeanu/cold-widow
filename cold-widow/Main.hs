module Main where

import System.Console.Program
import System.Console.Command
import System.Console.Argument 

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
