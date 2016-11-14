module Full where


import           System.Console.Command
  (
    Commands,Tree(Node),Command,command,withOption,withNonOption,io
  )
import           System.Console.Program (single,showUsage)
import qualified System.Console.Argument as Argument


main :: IO ()
main = single myCommands

myCommands :: Commands IO
myCommands = Node
  (command "count" "A program for counting" . io $ putStrLn "No command given; try \"count help\".")
  [
    Node countUp []
  , Node countDown []
  , Node help []
  ]

countUp,countDown,help :: Command IO
countUp = command
  {
    name        = "up"
  , description = "Count up"
  , action      = withOption verboseOpt $ \ v -> io $ if v == Quiet
      then putStrLn "Counting quietly!"
      else mapM_ print [1 ..]
  }
countDown = command
  {
    name        = "down"
  , description = "Count down from INT."
  , action      = withNonOption Argument.natural $ \ upperBound -> io $ mapM_ print [upperBound, pred upperBound .. 1]
  }
help = command "help" "Show usage info" $ io (showUsage myCommands)


-- Custom data type to describe a verbosity level.
data Verbosity
  = Quiet
  | Normal
  | Verbose
  deriving (Eq)

verboseOpt :: Argument.Option Verbosity
verboseOpt = Argument.option ['v'] ["verbose"] verbosity Normal "Specify the verbosity of the program."

verbosity :: Argument.Type Verbosity
verbosity = Argument.Type
  {
    Argument.parser       = \ x -> case x of
      "verbose" -> Right Verbose
      "normal"  -> Right Normal
      "quiet"   -> Right Quiet
      s         -> Left $ "The argument " ++ show s ++ " could not be recognised as a verbosity."
  , Argument.name         = "VERBOSITY"
  , Argument.defaultValue = Just Verbose
  }
