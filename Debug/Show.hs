{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, BangPatterns, NoImplicitPrelude, StandaloneDeriving,
             MagicHash, UnboxedTuples, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- | A wrapper type 'V' which has alternative instances of Show for
-- certain types whose official Show instances produce strings that
-- are not valid Haskell expressions.  For example:
-- @
--   > :m +System.IO.Error GHC.IO.Exception
--   > show (userError "hello")
--   user error (hello)
--   > show (V (userError "hello"))
--   "IOError {ioe_handle = Nothing, ioe_type = UserError, ioe_location = \"\", ioe_description = \"foo\", ioe_errno = Nothing, ioe_filename = Nothing}"
--   > putStrLn (show (V (userError "hello")))
--   IOError {ioe_handle = Nothing, ioe_type = UserError, ioe_location = "", ioe_description = "foo", ioe_errno = Nothing, ioe_filename = Nothing}
--   > IOError {ioe_handle = Nothing, ioe_type = UserError, ioe_location = "", ioe_description = "foo", ioe_errno = Nothing, ioe_filename = Nothing}
--   user error (hello)
-- @

module Debug.Show
    ( Please.Show(..)
    , V(V)
    ) where

import Control.Exception
import Data.Maybe
import GHC.Base
import Foreign.C.Types
import GHC.IO.Exception
import GHC.IO.Handle
import GHC.Show
import qualified Debug.CopyOfShow as Please
import Text.Parsec.Error
import Text.Parsec.Pos

newtype V a = V a

instance Eq a => Eq (V a) where
    (V a) == (V b) = a == b

-- Slip in our alternative show to show something wrapped in V.
instance Please.Show a => Show (V a) where
    show (V a) = Please.show a

-- Alternative show instances

instance Please.Show CInt where show = show
instance Please.Show Handle where show = show

instance Please.Show IOException where
    show e =
        "IOError {" ++
          "ioe_handle = " ++ Please.show (ioe_handle e) ++ ", " ++
          "ioe_type = " ++ Please.show (ioe_type e) ++ ", " ++
          "ioe_location = " ++ Please.show (ioe_location e) ++ ", " ++
          "ioe_description = " ++ Please.show (ioe_description e) ++ ", " ++
          "ioe_errno = " ++ Please.show (ioe_errno e) ++ ", " ++
          "ioe_filename = " ++ Please.show (ioe_filename e) ++ "}"

instance Please.Show IOErrorType where
    show AlreadyExists = "AlreadyExists"
    show NoSuchThing = "NoSuchThing"
    show ResourceBusy = "ResourceBusy"
    show ResourceExhausted = "ResourceExhausted"
    show EOF = "EOF"
    show IllegalOperation = "IllegalOperation"
    show PermissionDenied = "PermissionDenied"
    show UserError = "UserError"
    show UnsatisfiedConstraints = "UnsatisfiedConstraints"
    show SystemError = "SystemError"
    show ProtocolError = "ProtocolError"
    show OtherError = "OtherError"
    show InvalidArgument = "InvalidArgument"
    show InappropriateType = "InappropriateType"
    show HardwareFault = "HardwareFault"
    show UnsupportedOperation = "UnsupportedOperation"
    show TimeExpired = "TimeExpired"
    show ResourceVanished = "ResourceVanished"
    show Interrupted = "Interrupted"

-- instance Please.Show ControlFileError where show = show

instance Please.Show ParseError where
    show e = "newErrorMessage " ++ Please.show (errorPos e) ++ " " ++ Please.show (errorMessages e)

instance Please.Show Message where
    show (SysUnExpect s) = "SysUnExpect " ++ show s
    show (UnExpect s) = "UnExpect " ++ show s
    show (Expect s) = "Expect " ++ show s
    show (Message s) = "Message " ++ show s

instance Please.Show SourcePos where
    show pos =
        "(newPos " ++ show (sourceName pos) ++ " " ++ show (sourceLine pos) ++ " " ++ show (sourceColumn pos) ++ ")"

instance Please.Show SomeException where
    show e =
        "toException (" ++
            (maybeFrom (v (fromException e :: Maybe ArithException)) $
             maybeFrom (v (fromException e :: Maybe ArrayException)) $
             maybeFrom (v (fromException e :: Maybe AssertionFailed)) $
             maybeFrom (v (fromException e :: Maybe AsyncException)) $
             maybeFrom (v (fromException e :: Maybe BlockedIndefinitelyOnMVar)) $
             maybeFrom (v (fromException e :: Maybe BlockedIndefinitelyOnSTM)) $
             maybeFrom (v (fromException e :: Maybe Deadlock)) $
             -- maybeFrom (v (fromException e :: Maybe Dynamic)) $
             maybeFrom (v (fromException e :: Maybe ErrorCall)) $
             maybeFrom (v (fromException e :: Maybe ExitCode)) $
             maybeFrom (v (fromException e :: Maybe IOException)) $
             maybeFrom (v (fromException e :: Maybe NestedAtomically)) $
             maybeFrom (v (fromException e :: Maybe NoMethodError)) $
             maybeFrom (v (fromException e :: Maybe NonTermination)) $
             maybeFrom (v (fromException e :: Maybe PatternMatchFail)) $
             maybeFrom (v (fromException e :: Maybe RecConError)) $
             maybeFrom (v (fromException e :: Maybe RecSelError)) $
             maybeFrom (v (fromException e :: Maybe RecUpdError)) $
             maybeFrom (v (fromException e :: Maybe SomeAsyncException)) $
             ("No Debug.Show instance for " ++ show e)) ++ ")"

-- I don't know exactly what these do - they may need to be implemented
-- in more detail, but at least we can tell they name of their type now.
instance Please.Show ArithException where show e = "ArithException: " ++ show e
instance Please.Show ArrayException where show e = "ArrayException: " ++ show e
instance Please.Show AssertionFailed where show e = "AssertionFailed: " ++ show e
instance Please.Show AsyncException where show e = "AsyncException: " ++ show e
instance Please.Show BlockedIndefinitelyOnMVar where show e = "BlockedIndefinitelyOnMVar: " ++ show e
instance Please.Show BlockedIndefinitelyOnSTM where show e = "BlockedIndefinitelyOnSTM: " ++ show e
instance Please.Show Deadlock where show e = "Deadlock: " ++ show e
-- instance Please.Show Dynamic where show e = "Dynamic: " ++ show e
instance Please.Show ErrorCall where show e = "ErrorCall: " ++ show e
instance Please.Show ExitCode where show e = "ExitCode: " ++ show e
-- instance Please.Show IOException where show e = "IOException: " ++ show e -- defined above
instance Please.Show NestedAtomically where show e = "NestedAtomically: " ++ show e
instance Please.Show NoMethodError where show e = "NoMethodError: " ++ show e
instance Please.Show NonTermination where show e = "NonTermination: " ++ show e
instance Please.Show PatternMatchFail where show e = "PatternMatchFail: " ++ show e
instance Please.Show RecConError where show e = "RecConError: " ++ show e
instance Please.Show RecSelError where show e = "RecSelError: " ++ show e
instance Please.Show RecUpdError where show e = "RecUpdError: " ++ show e
instance Please.Show SomeAsyncException where show e = "SomeAsyncException: " ++ show e

v :: (Please.Show a, Functor f) => f a -> f String
v = fmap Please.show

maybeFrom :: Maybe c -> c -> c
maybeFrom = flip fromMaybe
