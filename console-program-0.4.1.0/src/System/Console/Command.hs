-- |
-- A 'Command' provides a mode of operation of your program.
-- This allows a single program to provide many different pieces of
-- functionality. The first argument to the program (or the first few, if it
-- has subcommands) determines which command should be executed.
-- (@darcs@ and @cabal@ are examples of programs with this behaviour.)
-- 
-- An 'Action' represents an IO action, together with information about
-- applicable options and non-option arguments.
module System.Console.Command
  (
    Commands,Tree.Tree(Tree.Node)
  , Command(Command,name,description,action,shorten)
  , command
  
  , Action
  , io
  , withNonOption
  , withNonOptions
  , withOption
  , ignoreOption
  ) where


import           System.Console.Internal
import qualified System.Console.Argument as Argument

import           Control.Exception      (throwIO)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Data.Map                as Map
import qualified Data.Tree               as Tree
import           System.Exit            (exitFailure)


-- | @Commands m@ is a tree of commands (with action in the monad @m@).
-- It represents the whole set of possible commands of a program.
type Commands m
  = Tree.Tree (Command m)

-- | Create a new command having a given name and action.
command :: String -> String -> Action m -> Command m
command n d a = Command { name = n, description = d, action = a, shorten = True }

-- | Specify whether abbreviated subcommands are allowed.
-- If not, (sub)commands that do not match exactly are interpreted as
-- non-option arguments.
allowShort :: Bool -> Command m -> Command m
allowShort b c = c { shorten = b }

-- | A simple action, taking no argument, and having no options.
io :: (MonadIO m) => m () -> Action m
io h = Action r [] [] [] where
  r []   _ = h
  r rest _ = liftIO . throwIO . UnknownCommand $ unwords rest

-- | Create an action that takes an argument (non-option).
-- 
-- The type of argument is specified by the first parameter; such values can
-- be obtained from the module "System.Console.Argument".
withNonOption :: (MonadIO m) => Argument.Type x -> (x -> Action m) -> Action m
withNonOption argumentType f = Action
  {
    run = \ nonOpts opts -> case nonOpts of
      (x : xs) -> case Argument.parser argumentType x of
        Left e  -> liftIO $ do -- Show errors and exit.
          putStrLn e
          exitFailure
        Right y -> run (f y) xs opts -- Argument parsing succeeded; run the action.
      []       -> case Argument.defaultValue argumentType of
        Nothing -> liftIO $ do
          putStrLn $ "Error: missing argument of type " ++ Argument.name argumentType
          exitFailure
        Just y  -> run (f y) [] opts
  , nonOptions = Argument.name argumentType : nonOptions (f undefined)
  , options = options (f undefined)
  , ignoringOptions = ignoringOptions (f undefined)
  }

-- | Create an action that takes all remaining non-option arguments.
--
--  The type of arguments is specified by the first parameter; such  values can
--  be obtained from the module "System.Console.Argument".
withNonOptions :: (MonadIO m) => Argument.Type x -> ([x] -> Action m) -> Action m
withNonOptions argumentType f = Action
   {
     run = \ nonOpts opts -> let runWithArgs args [] = run (f $ reverse args) [] opts
                                 runWithArgs args (x:xs) = case Argument.parser argumentType x of
                                   Left e -> liftIO $ do
                                     putStrLn e
                                     exitFailure
                                   Right y -> runWithArgs (y:args) xs
                             in runWithArgs [] nonOpts
   , nonOptions = ("[" ++ Argument.name argumentType ++ "...]") : nonOptions (f [])
   , options = options (f [])
   , ignoringOptions = ignoringOptions (f [])
   }

-- | Create an action that takes an option.
-- 
-- The first parameter is a description of the option; such a value can be
-- constructed using 'System.Console.Argument.option'.
withOption :: (MonadIO m) => Option a -> (a -> Action m) -> Action m
withOption (Option names optDescr def p) f = Action
  {
    run = \ nonOpts opts -> case maybe (Right def) p $ Map.lookup (identify names) opts of
      Left e  -> liftIO $ putStrLn e >> exitFailure
      Right a -> run (f a) nonOpts opts
  , nonOptions = nonOptions (f undefined)
  , options = ((identify names,names),optDescr) : options (f undefined)
  , ignoringOptions = ignoringOptions (f undefined)
  }

-- | Create an action that allows, but ignores, the given option.
-- 
-- This is especially useful if this option is given in the configuration
-- file, but is meant for other commands; then this action will not give an
-- error message about an unrecognised option.
ignoreOption :: Option a -> Action m -> Action m
ignoreOption (Option _ g _ _) a = a
  {
    ignoringOptions = g : ignoringOptions a
  }
