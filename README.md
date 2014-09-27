Debugging Tools
===============

Debug.Show
----------

This module exports a class Show, which is a duplicate of the
standard system Show, along with a collection of Please.Show instances
(initially for the types in Control.Exception) that actually display
the values as a Haskell expression, so that they can be fully
understood and used by the Read class.  There is also a wrapper type V
which is used to cause the regular show to use the Debug.Show instance:

    > Prelude.show (userError "foo")
    "user error (foo)"
    > Debug.Show.show (userError "foo")
    "IOError {ioe_handle = Nothing, ioe_type = UserError, ioe_location = \"\", ioe_description = \"foo\", ioe_errno = Nothing, ioe_filename = Nothing}"

Some exception types have less detailed Show implementations, but at
least they include the type name:

    > Prelude.show (SomeException (ErrorCall "abc"))
    "abc"
    > Debug.Show.show (SomeException (ErrorCall "abc"))
    "toException (ErrorCall: abc)"

Debug.Loc
---------

This module exports __LOC__, a template haskell function which expands
to an expression containing file location information about the place
it was used.  The expression of type Loc from module
Language.Haskell.TH.Syntax.  It may be used for constructing error
messages, e.g.

    throw $ MyException $__LOC__

(Submitted for inclusion in https://github.com/gregwebs/FileLocation.hs)

Debug.Console
-------------
This module exports ePutStr and ePutStrLn, which atomically write
messages to standard error.  A multi-threaded program can call these
to write debugging output to the console without the messages getting
garbled together by concurrency.
