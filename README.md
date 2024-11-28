Langkit
=======

Langkit (nickname for language kit) is a tool whose purpose is to make it easy
to create syntactic and semantic analysis engines. Write a language
specification in our Python DSL and Langkit will generate for you an Ada
library with bindings for the C and Python programming languages.

The generated library is meant to provide a basis to write tooling, including
tools working on potentially changing and incorrect code, such as IDEs.

The currently main Langkit user is
[Libadalang](https://github.com/AdaCore/libadalang), a high performance
semantic engine for the Ada programming language.

Dependencies
------------

To use Langkit, you will need:

* A Python 3.11 interpreter (or more recent). Python2 is no longer supported.
* Some Python libraries, including the Mako template system for Python (see
  `requirements-pypi.txt` and `requirements-github.txt` for the full list).
* A recent version of the GNAT Ada compiler, either from your OS's packages, or
  use [Alire](https://alire.ada.dev/docs/#toolchain-management) to get one.
* The [gnatcoll-core](https://github.com/AdaCore/gnatcoll-core) library.
* Ada bindings for GMP and Libiconv, from
  [gnatcoll-bindings](https://github.com/AdaCore/gnatcoll-bindings).
* The [VSS](https://github.com/AdaCore/VSS) library.
* The [Prettier-Ada](https://github.com/AdaCore/prettier-ada) library.
* The [AdaSAT](https://github.com/AdaCore/adasat) library.

For all Ada libraries (GNATcoll, VSS, Prettier-Ada, AdaSAT), make sure to
install the version that corresponds to the version of Langkit that is built.
For instance, build all `24.1` branches, or all `master` branches. Mixing
versions is not supported.

Install
-------

We assume below that all the Ada dependencies are installed under the
``$PREFIX`` directory, and that the environment is properly set up to use it:

* GPRbuild has access to project files (``$PREFIX/share/gpr`` is in
  ``GPR_PROJECT_PATH``, for example).

* The dynamic linker has access to shared libraries (``$PREFIX/lib`` is in
  ``LD_LIBRARY_PATH``, for exmaple).

First, clone the `adasat` repository in the `langkit` subdirectory of the
`langkit` repository:

    $ (cd langkit; git clone https://github.com/AdaCore/adasat)

Then, install the Langkit Python package itself:

    $ pip install .

Build the Libpythonlang and Liblktlang support libraries:

    $ python manage.py make --no-mypy --library-types=static,static-pic,relocatable

Install the `Langkit_Support` library:

    $ python manage.py install-langkit-support $PREFIX --library-types=static,static-pic,relocatable

Install the Libpythonlang and Liblktlang support libraries:

    $ (cd contrib/python && ./manage.py install $PREFIX --library-types=static,static-pic,relocatable --disable-all-mains)
    $ (cd contrib/lkt && ./manage.py install $PREFIX --library-types=static,static-pic,relocatable --disable-all-mains)
    $ pip install contrib/python/build/python
    $ pip install contrib/lkt/build/python

If you are interested in shared (`relocatable`) libraries only, you can omit
the `--library-types` arguments.

Testing
-------

In order to run the testsuite, launch the following command from the top-level
directory:

    $ python manage.py test

This is just a wrapper passing convenient options to the real testsuite
driver that is in `testsuite/testsuite.py`.

Note that even though the testsuite framework requires Python 3.11, it is
possible to run the tests themselves using a different Python interpreter. For
instance, to run them using Python 3.7, run:

    $ python manage.py test --with-python=python3.7

If you want to learn more about this test driver's options (for instance to run
tests under Valgrind), add a `-h` flag.

Documentation
-------------

The developer and user's documentation for Langkit is in `langkit/doc`. You can
consult it as a text files or you can build it. For instance, to generate HTML
documents, run from the top directory:

    $ make -C doc html

And then open the following file in your favorite browser:

    doc/_build/html/index.html

Bootstrapping a new language engine
-----------------------------------

Nothing is more simple than getting an initial project skeleton to work on a
new language engine. Imagine you want to create an engine for the Foo language,
run from the top-level directory:

    $ python scripts/create-project.py Foo

And then have a look at the created `foo` directory: you have minimal lexers
and parsers and a `manage.py` script you can use to build this new engine:

    $ python foo/manage.py make

Here you are!

Developer tools
---------------

Langkit uses mako templates generating Ada, C and Python code. This can be hard
to read. To ease development, Vim syntax files are available under the `utils`
directory (see `makoada.vim`, `makocpp.vim`). Install them in your
`$HOME/.vim/syntax` directory to get automatic highlighting of the template
files.
