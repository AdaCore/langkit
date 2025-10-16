SIGILL handler
==============

On GNU/Linux, the GNAT runtime installs a SIGILL signal handler, to turn
illegal instructions error into `Program_Error` exceptions. This may be
incompatible with the runtime of other languages, such as the JVM, which use
this signal for its internal use.

The purpose of this dummy dynamic library is to prevent the GNAT runtime from
installing this signal handler to avoid the conflict with other languages own.
This library needs to be loaded before any other Ada library for this trick to
work.
