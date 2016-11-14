{-# LANGUAGE DeriveDataTypeable #-}
module System.Console.Internal where


import           Control.Exception (Exception)
import           Data.Map          (Map)
import           Data.Typeable     (Typeable)
import qualified System.Console.GetOpt  as GetOpt


type UserCommand = [String]

-- | An @Action m@ is an action (in the monad @m@), which may take arguments
-- (\"non-options\") and options from the command line.
data Action m
  = Action
  {
    run             :: [String] -> Settings -> m ()
  , nonOptions      :: [String]
  , options         :: [((Identifier,[OptionName]),GetOpt.OptDescr CanonicalSetting)]
  , ignoringOptions :: [GetOpt.OptDescr CanonicalSetting]
  }

-- | A value of type @Option a@ describes an option, that delivers a value
-- to the program of type @a@.
data Option a = Option
  [OptionName]
  (GetOpt.OptDescr (Identifier,Maybe String))
  a
  (Maybe String -> Either String a)

data OptionName
  = Short Char
  | Long String
  deriving (Eq,Ord)
instance Show OptionName where
  show (Short c) = '-' : c : []
  show (Long i)  = "--" ++ i

-- The @Identifier@ of an option should identify it uniquely.
newtype Identifier
  = Id OptionName
  deriving (Eq,Ord)

type Setting
  = (OptionName,Maybe String)

type CanonicalSetting
  = (Identifier,Maybe String)

type Settings
  = Map Identifier (Maybe String)

identify :: [OptionName] -> Identifier
identify (n : _) = Id n
identify _       = error "System.Console.Internal.identify: option without option names."


-- | A @Command m@ is an action (in the monad @m@), together with some
-- descriptive information.
data Command m
  = Command
    {
      -- | This determines which command is executed.
      name :: String
      -- | For usage info.
    , description :: String
      -- | The actual action performed by this command.
    , action :: Action m
      -- | Prefer shortened subcommands over non-option arguments.
    , shorten :: Bool
    }

data ConsoleProgramException
  = UnknownCommand String
  deriving (Typeable)
instance Show ConsoleProgramException where
  show (UnknownCommand c) = "Error: unused non-option or unrecognised command: " ++ c
instance Exception ConsoleProgramException
