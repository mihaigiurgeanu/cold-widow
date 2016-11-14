{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module contains functions to build a console program, that parses
-- the command line (and a configuration file), divides it into commands,
-- options and non-options, and executes the corresponding action from a tree
-- of available commands.
-- 
-- These commands can be constructed using the module "System.Console.Command".
module System.Console.Program
  (
  -- * Using a command tree to construct a program
    single
  , interactive
  , interactiveWithPrompt
  , showUsage
  
  -- * Configuration file
  -- $configfile
  ) where


import           System.Console.Command    (Commands,Command,shorten)
import           System.Console.ConfigFile (readFromFile)
import           System.Console.Internal   
  (
    run
  , UserCommand
  , options
  , nonOptions
  , ignoringOptions
  , action
  , description
  , name
  , ConsoleProgramException(UnknownCommand)
  )

import           Control.Applicative       (Applicative,(<|>),(*>))
import           Control.Arrow             ((&&&),first)
import           Control.Concurrent        (myThreadId)
import           Control.Exception         (throwTo,AsyncException(UserInterrupt))
import           Control.Monad             (when,void)
import           Control.Monad.IO.Class    (MonadIO,liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.List                 (isPrefixOf,concatMap)
import qualified Data.Map                     as Map
import           Data.Traversable          (traverse)
import qualified Data.Tree                    as Tree
import qualified System.Console.ANSI          as ANSI
import qualified System.Console.GetOpt        as GetOpt
import qualified System.Console.Haskeline     as Haskeline
import           System.Environment        (getArgs)
import           System.Exit               (exitSuccess)
import           System.IO                 (readFile)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Signals         as Sig
#endif
import qualified Text.Parsec                  as P
import qualified Text.PrettyPrint.ANSI.Leijen as PP


-- $configfile
-- The configuration file is assumed to be in the user's home directory, and
-- to be named \".foobar/config\", where \"foobar\" is the name of the
-- root of the command tree (usually the name of the program).
-- 
-- Settings in this file are of the form
-- @
--   option-name=option-value
-- @
-- The format of the \"option-value\" part depends on the type of the option
-- argument; see "System.Console.Argument".
-- 
-- Sections can be defined for settings applying to a single command,
-- using the name of a command, enclosed in square brackets, as section header:
-- @
--   [command1]
--   option-for-command1=true
-- @.


-- Parse the given list of strings into a command, non-options and options.
parse :: (MonadIO m,Applicative m) => Commands m -> [String] -> m ()
parse commands args = do
  let (commandString,command,restArgs) = select commands args
  fileSettings <- readFromFile commands commandString
  let (opts,nonOpts,errors) = GetOpt.getOpt
        GetOpt.Permute
        (map snd (options $ action command) ++ ignoringOptions (action command))
        restArgs
  when (not $ null errors) . void $
    traverse (liftIO . putStrLn) errors
  let commandLineSettings = Map.fromList opts
      optionIndex = Map.fromList .
        concatMap (\ ((i,names),_) -> map (flip (,) i) names) .
        options $ action command
      lookupSetting i = maybe
        (error $ "Unknown option: " ++ show i)
        id
        $ Map.lookup i optionIndex
      identifiedFileSettings = Map.fromList $ map (first lookupSetting) fileSettings
      settings = commandLineSettings `Map.union` identifiedFileSettings
  run (action command) nonOpts settings

-- Select the right command from the command tree, and return the rest of the command line.
select :: Commands m -> [String] -> (UserCommand,Command m,[String])
select (Tree.Node root _   ) []       = ([],root,[])
select (Tree.Node root subs) (x : xs) = case lookup x subcommands of
  -- There is an exactly matching subcommand.
  Just cs -> descend cs
  -- There is no exactly matching subcommand.
  Nothing -> case shorten root of
    True  -> case filter (isPrefixOf x . fst) subcommands of
      [(_,cs)] -> descend cs
      _        -> commit
    False -> commit
 where
  subcommands = map (name . Tree.rootLabel &&& id) subs
  descend cs = let (xs',c,rest) = select cs xs in (x : xs',c,rest)
  commit = ([],root,x : xs)

-- | Load the configuration file (if present), and run the command given on
-- the command line. Settings on the command line override the configuration
-- file.
-- 
-- You may use this function, applied to your tree of available commands,
-- as your @main@ function.
single :: (MonadIO m,Applicative m) => Commands m -> m ()
single commands = parse commands =<< liftIO getArgs

-- | Start an interactive session. Arguments to the program are ignored;
-- instead, the user may repeatedly enter a command, possibly with options,
-- which will be executed.
interactive :: (MonadIO m,Haskeline.MonadException m,Applicative m) => Commands m -> m ()
interactive commands = interactiveWithPrompt (return $ name $ Tree.rootLabel commands) commands

-- | Same as 'interactive', but with a custom monadic function specifying the
-- text of the prompt.
interactiveWithPrompt :: (MonadIO m,Haskeline.MonadException m,Applicative m)
  => m String -> Commands m -> m ()
interactiveWithPrompt prompt commands = do
  tid <- liftIO myThreadId
#ifndef mingw32_HOST_OS
  liftIO $ Sig.installHandler Sig.keyboardSignal (Sig.Catch $ throwTo tid UserInterrupt) Nothing
#endif
  Haskeline.runInputT Haskeline.defaultSettings $ sequence_ . repeat $ one
 where
  one = getLine' >>= \ line -> case words' line of
    Left e   -> liftIO $ putStrLn e
    Right ws -> lift (parse commands ws)
      `Haskeline.catch` (\ (e :: ConsoleProgramException) -> liftIO $ print e)
      `Haskeline.catch` (\ (e :: AsyncException) -> if e == UserInterrupt
        then liftIO $ print e
        else Haskeline.throwIO e)
  getLine' = do
    putStrBold =<< lift prompt
    maybe (liftIO exitSuccess) return =<< Haskeline.getInputLine ": "

words' :: String -> Either String [String]
words' = either (Left . show) Right . P.parse p "" where
  p = P.optional space *> P.sepEndBy (quoted <|> unquoted) space
  unquoted = P.many1 $ P.noneOf [' ']
  space = P.many1 $ P.char ' '
  quoted = P.between quote quote . P.many $
    P.try (escaped quote) <|> escaped escape <|> P.noneOf ['"','\\']
  quote = P.char '"'
  escape = P.char '\\'
  escaped x = escape *> x

putStrBold :: (MonadIO m) => String -> m ()
putStrBold x = liftIO $ do
  ANSI.setSGR [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  putStr x
  ANSI.setSGR [ANSI.Reset]

-- | Print usage info for the program to stdout.
showUsage :: (MonadIO m) => Commands n -> m ()
showUsage = liftIO . PP.putDoc . (PP.<> PP.line) . usage
 where
  usage (Tree.Node c ns) = subcs ns . (PP.<> PP.line) . opts c . descr c . nonOpts c $
    PP.bold (PP.text $ name c)
  descr c
    | null d    = id
    | otherwise = flip (PP.<$>) $ PP.string d
    where
      d = description c
  nonOpts c = let n = nonOptions $ action c in if null n
    then id
    else flip (PP.<+>) $ PP.cat . PP.punctuate PP.space . map PP.text $ n
  opts c = let o = options $ action c in if null o
    then id
    else flip (PP.<$>) . PP.indent 2 . PP.vsep . map (opt . snd) $ o
  opt (GetOpt.Option short long a descr) = list 5 "-" (arg id) (map (: []) short)
    PP.<+> list 20 "--" (arg (PP.equals PP.<>)) long PP.<+> PP.string descr where
    arg maybeEq = case a of
      GetOpt.NoArg _    -> PP.empty
      GetOpt.ReqArg _ x -> maybeEq $ PP.string x
      GetOpt.OptArg _ x -> PP.brackets (maybeEq $ PP.string x)
  list i p a = PP.fill i . PP.cat . PP.punctuate PP.comma . map (\ x -> PP.text p PP.<> PP.text x PP.<> a)
  subcs ns = if null ns then id else flip (PP.<$>) $ PP.indent 2 (PP.vsep $ map usage ns)
