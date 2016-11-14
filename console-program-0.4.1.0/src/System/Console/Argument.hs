-- | This module contains functions to create option descriptions, together
-- with their argument types.
module System.Console.Argument
  (
  -- * Option descriptions
    Option
  , option

  , Type(Type,parser,name,defaultValue)
  
  -- * Argument types
  , optional
  , string
  , boolean
  , directory
  , file
  , device
  , natural
  , integer  
  ) where


import System.Console.Internal hiding (name)

import           Data.Char        (toLower)
import           Data.List.HT     (viewR)
import qualified Data.Map              as Map
import qualified System.Console.GetOpt as GetOpt
import qualified Text.Parsec.Extra     as P


-- | A @Type a@ represents the type of an option or argument.
-- 
-- Further below you can find some common types of option arguments.
data Type a
  = Type
  {
    parser       :: String -> Either String a
    -- ^ Parse the option argument into a value (@Right@) or signal a parsing
    -- error (@Left@).
  , name         :: String
    -- ^ A name for this type of option argument (for usage info).
  , defaultValue :: Maybe a
    -- ^ The default value when the option occurs without option argument.
    -- @Nothing@ means that an argument is required for this type of option.
  }

instance Functor Type where
  fmap f t = t { parser = fmap f . parser t, defaultValue = fmap f (defaultValue t) }

-- | Create an option description.
-- 
-- Options can have arguments, as in @myprogram --foo=bar@, where @bar@
-- is the argument to @foo@. These arguments have types, dictated by the
-- particular option; this type is the third parameter to @option@.
option
  :: [Char]     -- ^ List of short option characters.
  -> [String]   -- ^ List of long option strings.
  -> Type a     -- ^ Type of option argument.
  -> a          -- ^ Default value when the option is not specified by the user.
  -> String     -- ^ Description.
  -> Option a   -- ^ The resulting option description.
option short long t def description = let
  names = map Short short ++ map Long long
  identifier = identify names
 in Option
  names
  (GetOpt.Option
    short
    long
    (maybe
      (GetOpt.ReqArg ((,) identifier . Just) $ name t)
      (const $ GetOpt.OptArg ((,) identifier) $ name t)
      (defaultValue t))
    description)
  def
  (maybe (maybe (Left "Option argument missing.") Right $ defaultValue t) (parser t))

-- Common argument types.

optional
  :: a      -- ^ Default value.
  -> Type a
  -> Type a
optional x t = t { defaultValue = Just x }

-- | A plain string.
string :: Type String
string = Type Right "STRING" Nothing

-- | A boolean. Argument can be \"1\",\"0\",\"true\",\"false\",\"on\",\"off\",\"yes\",\"no\".
boolean :: Type Bool
boolean = Type
  {
    name    = "BOOL"
  , parser  = \ y -> maybe (e y) Right . flip Map.lookup m . map toLower $ y
  , defaultValue = Just True
  }
 where
  m = Map.fromList [("1",True),("0",False),("true",True),("false",False),("on",True),("off",False),("yes",True),("no",False)]
  e y = Left $ "Argument " ++ show y ++ " could not be recognised as a boolean."

-- | A natural number (in decimal).
natural :: Type Integer
natural = Type { name = "INT (natural)", parser = P.parseM P.natural "", defaultValue = Nothing }

-- | An integer number (in decimal).
integer :: Type Integer
integer = Type { name = "INT", parser = P.parseM P.integer "", defaultValue = Nothing }

-- | A directory path. A trailing slash is stripped, if present.
directory :: Type FilePath
directory = Type { name = "DIR", parser = Right . stripTrailingSlash, defaultValue = Nothing }
 where
  stripTrailingSlash x = case viewR x of
    Nothing       -> ""
    Just (i,l)
      | l == '/'  -> i
      | otherwise -> x

-- | A file path.
file :: Type FilePath
file = string { name = "FILE" }

-- | A device path.
device :: Type FilePath
device = string { name = "DEVICE" }
