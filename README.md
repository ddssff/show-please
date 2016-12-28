Debugging Tools
===============

Debug.Show
----------

This module exports a type V(V), along with a collection of Show instances
(initially for the types in Control.Exception) that actually display
the values as a Haskell expression, so that they can be fully
understood and used by the Read class.

    > show (userError "foo")
    "user error (foo)"
    > show (V (userError "foo"))
    "IOError {ioe_handle = Nothing, ioe_type = UserError, ioe_location = \"\", ioe_description = \"foo\", ioe_errno = Nothing, ioe_filename = Nothing}"

Some exception types have less detailed Show implementations, but at
least they include the type name:

    > Left (e :: SomeException) <- (try (error "abc"))
    > putStrLn (show e)
    abc
    > putStrLn (show (V e))
    toException (ErrorCall: abc :: ErrorCall)

Now you can see what the real type of a SomeException is:

    > error "foo" `catch` (\(e :: SomeException) -> pure (show (V e))) >>= putStrLn
    toException (ErrorCall "foo" :: ErrorCall)
    > readProcess "false" [] "" `catch` (\(e :: SomeException) -> pure (show (V e))) >>= putStrLn
    .toException (IOError {ioe_handle = Nothing, ioe_type = OtherError, ioe_location = "readCreateProcess: false (exit 1)", ioe_description = "", ioe_errno = Nothing, ioe_filename = Nothing} :: IOException)
    > (forkIO (hClose stdout) >>= (putStrLn . show)) `catch` (\(e :: SomeException) -> putStrLn (show (V e)))
    ghc: <stdout>: hFlush: illegal operation (handle is closed)
