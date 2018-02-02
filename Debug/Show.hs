-- | A wrapper type and associated Show instances that generate
-- correct haskell code, especially for exception types.

{-# LANGUAGE CPP, FlexibleInstances #-}
{-# OPTIONS -Wall -Werror #-}

module Debug.Show (V(V)) where
import Control.Exception
import Data.Maybe
import Data.Time
-- import Foreign.C.Types
-- import GHC.Base
import GHC.IO.Exception
-- import GHC.IO.Handle
-- import GHC.Show
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax
import Text.Parsec.Error
import Text.Parsec.Pos

newtype V a = V a

instance Show (V IOException) where
    show (V e) =
        "IOError {" ++
          "ioe_handle = " ++ show (ioe_handle e) ++ ", " ++
          "ioe_type = " ++ show (V (ioe_type e)) ++ ", " ++
          "ioe_location = " ++ show (ioe_location e) ++ ", " ++
          "ioe_description = " ++ show (ioe_description e) ++ ", " ++
          "ioe_errno = " ++ show (ioe_errno e) ++ ", " ++
          "ioe_filename = " ++ show (ioe_filename e) ++ "}"

instance Show (V IOErrorType) where
    show (V AlreadyExists) = "AlreadyExists"
    show (V NoSuchThing) = "NoSuchThing"
    show (V ResourceBusy) = "ResourceBusy"
    show (V ResourceExhausted) = "ResourceExhausted"
    show (V EOF) = "EOF"
    show (V IllegalOperation) = "IllegalOperation"
    show (V PermissionDenied) = "PermissionDenied"
    show (V UserError) = "UserError"
    show (V UnsatisfiedConstraints) = "UnsatisfiedConstraints"
    show (V SystemError) = "SystemError"
    show (V ProtocolError) = "ProtocolError"
    show (V OtherError) = "OtherError"
    show (V InvalidArgument) = "InvalidArgument"
    show (V InappropriateType) = "InappropriateType"
    show (V HardwareFault) = "HardwareFault"
    show (V UnsupportedOperation) = "UnsupportedOperation"
    show (V TimeExpired) = "TimeExpired"
    show (V ResourceVanished) = "ResourceVanished"
    show (V Interrupted) = "Interrupted"

instance Show (V Message) where
    show (V (SysUnExpect s)) = "SysUnExpect " ++ show s
    show (V (UnExpect s)) = "UnExpect " ++ show s
    show (V (Expect s)) = "Expect " ++ show s
    show (V (Message s)) = "Message " ++ show s

instance Show (V SourcePos) where
    show (V pos) =
        "(newPos " ++ show (sourceName pos) ++ " " ++ show (sourceLine pos) ++ " " ++ show (sourceColumn pos) ++ ")"

instance Show (V SomeException) where
    show (V e) =
        "toException (" ++
            (maybeFrom (fmap ((++ " :: ArithException") . show . V) (fromException e :: Maybe ArithException)) $
             maybeFrom (fmap ((++ " :: ArrayException") . show . V) (fromException e :: Maybe ArrayException)) $
             maybeFrom (fmap ((++ " :: AssertionFailed") . show . V) (fromException e :: Maybe AssertionFailed)) $
             maybeFrom (fmap ((++ " :: AsyncException") . show . V) (fromException e :: Maybe AsyncException)) $
             maybeFrom (fmap ((++ " :: BlockedIndefinitelyOnMVar") . show . V) (fromException e :: Maybe BlockedIndefinitelyOnMVar)) $
             maybeFrom (fmap ((++ " :: BlockedIndefinitelyOnSTM") . show . V) (fromException e :: Maybe BlockedIndefinitelyOnSTM)) $
             maybeFrom (fmap ((++ " :: Deadlock") . show . V) (fromException e :: Maybe Deadlock)) $
             -- maybeFrom (fmap ((++ " :: Dynamic") . show . V) (fromException e :: Maybe Dynamic)) $
             maybeFrom (fmap ((++ " :: ErrorCall") . show . V) (fromException e :: Maybe ErrorCall)) $
             maybeFrom (fmap ((++ " :: ExitCode") . show . V) (fromException e :: Maybe ExitCode)) $
             maybeFrom (fmap ((++ " :: IOException") . show . V) (fromException e :: Maybe IOException)) $
             maybeFrom (fmap ((++ " :: NestedAtomically") . show . V) (fromException e :: Maybe NestedAtomically)) $
             maybeFrom (fmap ((++ " :: NoMethodError") . show . V) (fromException e :: Maybe NoMethodError)) $
             maybeFrom (fmap ((++ " :: NonTermination") . show . V) (fromException e :: Maybe NonTermination)) $
             maybeFrom (fmap ((++ " :: PatternMatchFail") . show . V) (fromException e :: Maybe PatternMatchFail)) $
             maybeFrom (fmap ((++ " :: RecConError") . show . V) (fromException e :: Maybe RecConError)) $
             maybeFrom (fmap ((++ " :: RecSelError") . show . V) (fromException e :: Maybe RecSelError)) $
             maybeFrom (fmap ((++ " :: RecUpdError") . show . V) (fromException e :: Maybe RecUpdError)) $
             maybeFrom (fmap ((++ " :: SomeAsyncException") . show . V) (fromException e :: Maybe SomeAsyncException)) $
             ("No Show instance for V " ++ show e ++ ", report this as a bug at https://github.com/seereason/show-please")) ++ ")"

-- I don't know exactly what these do - they may need to be implemented
-- in more detail, but at least we can tell they name of their type now.
instance Show (V ArithException) where show (V e) = "ArithException: " ++ show e
instance Show (V ArrayException) where show (V e) = "ArrayException: " ++ show e
instance Show (V AssertionFailed) where show (V e) = "AssertionFailed: " ++ show e
instance Show (V AsyncException) where show (V e) = "AsyncException: " ++ show e
instance Show (V BlockedIndefinitelyOnMVar) where show (V e) = "BlockedIndefinitelyOnMVar: " ++ show e
instance Show (V BlockedIndefinitelyOnSTM) where show (V e) = "BlockedIndefinitelyOnSTM: " ++ show e
instance Show (V Deadlock) where show (V e) = "Deadlock: " ++ show e
-- instance Show (V Dynamic) where show (V e) = "Dynamic: " ++ show e
#if MIN_VERSION_base(4,9,0)
instance Show (V ErrorCall) where show (V (ErrorCallWithLocation s t)) = "ErrorCallWithLocation " ++ show s ++ " " ++ show t
#else
instance Show (V ExitCode) where show (V e) = "ExitCode: " ++ show e
#endif
-- instance Show (V IOException) where show (V e) = "IOException: " ++ show e -- defined above
instance Show (V NestedAtomically) where show (V e) = "NestedAtomically: " ++ show e
instance Show (V NoMethodError) where show (V e) = "NoMethodError: " ++ show e
instance Show (V NonTermination) where show (V e) = "NonTermination: " ++ show e
instance Show (V PatternMatchFail) where show (V e) = "PatternMatchFail: " ++ show e
instance Show (V RecConError) where show (V e) = "RecConError: " ++ show e
instance Show (V RecSelError) where show (V e) = "RecSelError: " ++ show e
instance Show (V RecUpdError) where show (V e) = "RecUpdError: " ++ show e
instance Show (V SomeAsyncException) where show (V e) = "SomeAsyncException: " ++ show e

maybeFrom :: Maybe c -> c -> c
maybeFrom = flip fromMaybe

instance Show (V UTCTime) where
    show (V t) = "(read " ++ show (show t) ++ " :: UTCTime)"

instance Show (V Name) where
    show (V (Name o f)) = "Name (" ++ show o ++ ") (" ++ show (V f) ++ ")"
instance Show (V NameFlavour) where
    show (V NameS) = "NameS"
    show (V (NameQ m)) = "NameQ " ++ show m
    show (V (NameU n)) = "NameU " ++ show n
    show (V (NameL n)) = "NameL " ++ show n
    show (V (NameG s p m)) = "NameG (" ++ show (s) ++ ") (" ++ show (p) ++ ") (" ++ show (m) ++ ")"
