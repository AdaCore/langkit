${ctx.lib_name.camel} Java API
${'=' * len(ctx.lib_name.camel)}=========

This package contains the Java bindings for the ${ctx.lib_name.lower} library.

Build and install
-----------------

This project use `Maven` for the build and install process and the required
Java version is 1.8 or later.
The building process also uses `GCC` and `Make` to build the JNI library so it
must be installed on your system and in your path.

Firstly you need to set the `JAVA_HOME` environment variable to your JDK
installation.

```sh
$> export JAVA_HOME=/path/to/jdk
```

Then you can build and install the Java API.

```sh
$> mvn clean install
```

Usage
-----

You can use the Java API in any Java code.

```java
import com.adacore.${ctx.lib_name.lower}.${ctx.lib_name.camel};

public class Main {
    public static void main(String[] args) {

        // Parse the wanted file
        String myFile = "path/to/file/to/parse"
        ${ctx.lib_name.camel}.AnalysisContext ctx =
            ${ctx.lib_name.camel}.AnalysisContext.create();
        ${ctx.lib_name.camel}.AnalysisUnit unit = ctx.getUnitFromFile(myFile);

        // Dump the parsed tree
        System.out.println(unit.root().dump());

    }
}
```
