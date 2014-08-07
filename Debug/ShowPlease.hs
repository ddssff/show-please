{-# LANGUAGE CPP, FlexibleInstances, BangPatterns, NoImplicitPrelude, StandaloneDeriving,
             MagicHash, UnboxedTuples #-}
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

module Debug.ShowPlease
    ( V(V)
    ) where

import GHC.Base
import Foreign.C.Types
import GHC.IO.Exception
import GHC.IO.Handle
import GHC.Show
import qualified Debug.ShowPrime as Please

newtype V a = V a

-- Slip in our alternative show to show something wrapped in V.
instance Please.Show a => Show (V a) where
    show (V a) = Please.show a

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
