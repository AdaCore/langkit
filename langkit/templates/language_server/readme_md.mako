${ctx.config.library.language_name.lower}ls
${'=' * len(ctx.config.library.language_name.lower)}==

This package contains a language server for
${ctx.config.library.language_name.lower}

Build and install
-----------------

This project uses `Maven` for the build and install process and the required
Java version is 1.8 or later.

### 1) Install the Java bindings

Firstly you need to build and install the **Java bindings** for
${ctx.short_name_or_long}. Their instructions are available in the `java`
folder of your build directory.

### 2) Install the Langkit generic interface

You must then build and install the Langkit generic interface in the langkit
repository:

```sh
$[langkit]> mvn install -f langkit/java_support/
```

### 3) Install the Langkit LSP library

```sh
$[langkit-language-server]> mvn install -f lsp/
```

### 4) Build the Language server for ${ctx.short_name_or_long}

```sh
$> lkm make --enable-lsp
```

Usage
-----

In order to run the language server, you can use the
`build/lklsp${ctx.config.library.language_name.lower}ls.py` script. It makes
use of environment variables in order to find your library. You can set them
using:

```sh
$> eval `./manage.py setenv`
```

