***********************
Langkit's Java bindings
***********************

Introduction
============

Langkit can generate and build bindings for the Java environment for any
language definition. Those bindings use
`JNI <https://docs.oracle.com/javase/8/docs/technotes/guides/jni/>`_ and
`Native Image C API
<https://www.graalvm.org/22.1/reference-manual/native-image/C-API/>`_
(decided at runtime) to implement a Java API on top of the library's
generated C API.

Java bindings are mainly used in the
`LKQL <https://github.com/AdaCore/langkit-query-language>`_ JIT implementation
but you can use it for whatever you want. Due to the usage of Native Image
C API, you can compile the bindings using Java application with Graal's
native-image and benefit of high speed native calls.

Generate, build and install
==================

Building Java bindings requires:

* JDK 8 or later
* ``JAVA_HOME`` environment variable to be set to your JDK installation
  directory
* Maven

Java bindings are always generated during the Langkit's code generation
so to emit them you only have to use the ``make`` command of your language
manage script, Java sources are available in the ``build/java`` directory.
To build the Java bindings use the ``--enable-java`` option with the ``make``
command.

.. code-block::

    ./manage.py make --enable-java

By default, Maven uses your default local repository (generally ``~/.m2``) to
download dependencies. You can use the ``--maven-local-repo`` option to
change this behaviour and set a custom local Maven repository.

.. code-block::

    ./manage.py make --enable-java --maven-local-repo my/repo/path/

Once you have built the Java bindings, you can find a JAR file under the
``build/java/target`` directory which is named ``libfoolang-X.X.jar``. It
contains all required Java classes to use the Java bindings in any Java
project. Additionally you can install the Java bindings in your local Maven
repository with the ``install`` command of the manage script and the
``--enable-java`` option. As the ``make`` command, you can use the
``--maven-local-repo`` option to set the Maven repository to install the
bindings in.

.. code-block::

    ./manage.py install my_install_dir --enable-java [--maven-local-repo my/repo/path]

Test Java bindings
==================

To test the Java bindings generation, building and usage you can run the
Langkit testsuite with the ``--enable-java`` flag.

.. attention::

    Requirements for the Java tests running are the same as to build the Java
    bindings.

.. code-block::

    $[langkit/testsuite]> ./testsuite.py java_api --enable-java

Usage example
=============

Let's say that your language is named ``Foo`` and you have generated the
language shared library (``libfoolang``) and the Java bindings. You can create
a Maven project and add the following dependency.

.. code-block::

    <dependency>
        <groupId>com.adacore</groupId>
        <artifactId>libfoolang</artifactId>
        <version>X.X</version>
    </dependency>

Then you can use the ``Libfoolang`` class which contains all the needed
components to parse a foo file, visit its parsing tree and access the nodes'
fields and properties.

.. code-block:: java

    import com.adacore.libfoolang.Libfoolang;

    public class Main {
        public static void main(String[] args) {
            Libfoolang.AnalysisContext ctx =
                Libfoolang.AnalysisContext.create();
            Libfoolang.AnalysisUnit unit = ctx.getUnitFromFile("my_file.foo");

            // Visit the AST and print every node
            Libfoolang.FooNode root = unit.root();
            System.out.println(root);
            for(Libfoolang.FooNode child : root.children()) {
                System.out.println(child);
            }

            // Call a property on the root which returns a text
            Libfoolang.Text = root.pMyProperty();
        }
    }

There is a visitor Java interface that you can use to easily implement
a tree traversal.

.. code-block:: java

    import com.adacore.libfoolang.Libfoolang;

    public class MyExampleVisitor
    implements Libfoolang.BasicVisitor<String> {
        public String visit(Libfoolang.FooNode fooNode) {...}
        public String visit(Libfoolang.OtherNode otherNode) {...}
    }
