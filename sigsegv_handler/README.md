SIGSEGV handler
===============

On GNU/Linux, the GNAT runtime installs a SIGSEGV signal handler, so turn
memory issues into `Storage_Error` exceptions. This may be incompatible with
the runtime of other languages, such as OCaml's, which tries to do the same
thing.

The purpose of this dummy dynamic library is to prevent the GNAT runtime from
installing this signal handler to avoid the conflict with OCaml's own. This
library needs to be loaded before the Langkit-generated library for this trick
to work.
