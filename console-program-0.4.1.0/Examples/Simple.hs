module Simple where


import System.Console.Command (Commands,Tree(Node),command,io)
import System.Console.Program (single)


myCommands :: Commands IO
myCommands = Node
  (command "say" "" . io $ putStrLn "No command given.")
  [
    Node
      (command "yes" "" . io $ putStrLn "Yes!")
      []
  , Node
      (command "no" "" . io $ putStrLn "No!")
      []
  ]

main :: IO ()
main = single myCommands
