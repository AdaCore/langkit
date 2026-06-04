import os
import subprocess

from pathlib import Path


class JavaApiHelper:
    """
    Helper to work with Java bindings of Langkit-generated libraries.
    """

    def __init__(self, lang_name: str, bindings_dir: os.PathLike):
        self.lang_name = lang_name
        self._bindings_dir = Path(bindings_dir).absolute()

    @property
    def java_exe(self) -> str:
        """
        Absolute path to the ``java`` executable.
        """
        return os.path.join(os.environ["JAVA_HOME"], "bin", "java")

    @property
    def javac_exe(self) -> str:
        """
        Absolute path to the ``javac`` executable.
        """
        return os.path.join(os.environ["JAVA_HOME"], "bin", "javac")

    @property
    def native_image_exe(self) -> str:
        """
        Absolute path to the ``native-image`` compiler executable.
        """
        return os.path.join(
            os.environ["GRAAL_HOME"],
            "bin",
            ("native-image.cmd" if os.name == "nt" else "native-image"),
        )

    def bindings_dir(self, *args) -> str:
        """
        Build an absolute filename in the Java bindings directory.
        """
        return str(self._bindings_dir / Path(*args))

    @property
    def bindings_jar_file(self) -> str:
        """
        Absolute filename of the JAR for the library Java bindings.
        """
        return self.bindings_dir("target", f"lib{self.lang_name}lang.jar")

    @property
    def base_classpath(self) -> list[str]:
        """
        Java classpath required to use the library Java bindings.
        """

        def lib_jar_file(lib_name: str) -> str:
            return self.bindings_dir("target", "lib", lib_name)

        return [
            self.bindings_jar_file,
            lib_jar_file("langkit_support.jar"),
            lib_jar_file("truffle-api.jar"),
            lib_jar_file("polyglot.jar"),
        ]

    def javac(
        self,
        java_file: str,
        output_dir: str | None = None,
        classpath: list[str] | None = None,
    ) -> None:
        """
        Run the "javac" tool on the provided ``main_class`` with the dependency
        closure required to use the Java bindings.

        Place the resulting ".class" files in the provided ``output_dir``.

        If provided, the items in the ``classpath`` argument are passed to
        javac as additional classpath arguments.
        """
        classpath = classpath or []
        try:
            subprocess.check_output(
                [
                    self.javac_exe,
                    "-cp",
                    os.pathsep.join(self.base_classpath + classpath),
                    "-encoding",
                    "utf8",
                    java_file,
                ]
                + (["-d", output_dir] if output_dir is not None else []),
                stderr=subprocess.STDOUT,
            )
        except subprocess.CalledProcessError as e:
            print("Error in javac call:")
            print(e.stdout.decode())
            raise e

    def native_image(
        self,
        main_class: str,
        output_file: str,
        classpath: list[str],
        env: dict[str, str] | None = None,
    ) -> None:
        """
        Run the native-image compiler on the provided ``main_class`` with the
        closure required to use the Java bindings.

        Setup native-image to produce the ``output_file`` executable file if
        the compilation succeeds.

        If provided, the items in the ``classpath`` argument are passed to
        native-image as additional classpath arguments.
        """
        env = env or dict(os.environ)

        def find_file_in_env(file_name: str, env_var: str) -> str:
            """
            Find the file with the provided ``file_name`` in directories listed
            in the environment variable corresponding to ``env_var``. This
            function raises an error if the file cannot be found.
            """
            for directory in env.get(env_var, "").split(os.pathsep):
                if os.path.isfile(os.path.join(directory, file_name)):
                    return directory
            raise FileNotFoundError(f"Cannot find {file_name} in {env_var}")

        os_specific_options = []
        if os.name != "nt":
            # Find the directory that contains the library header file
            header_dir = find_file_in_env(
                f"lib{self.lang_name}lang.h", "C_INCLUDE_PATH"
            )

            # Find directories that contain the required shared libraries
            lib_dirs = [
                find_file_in_env(
                    f"lib{self.lang_name}lang.so", "LIBRARY_PATH"
                ),
                find_file_in_env("libz.so", "LIBRARY_PATH"),
            ]

            # We also need to provide rpath-links to the compiler to allow it
            # to find libraries during linking phase. This is required because
            # native-image is spawning the C compiler in an empty environment
            # on Linux systems.
            ld_library_path = env.get("LD_LIBRARY_PATH")
            rpaths = (
                [
                    f"-Wl,-rpath-link={p}"
                    for p in ld_library_path.split(os.pathsep)
                ]
                if ld_library_path
                else []
            )

            # Create native-image options to provide required information when
            # spawning GCC.
            os_specific_options.extend(
                [
                    f"--native-compiler-options=-I{header_dir}",
                    *[
                        f"--native-compiler-options=-L{lib_dir}"
                        for lib_dir in lib_dirs
                    ],
                    *[f"--native-compiler-options={rp}" for rp in rpaths],
                ]
            )
        else:
            # Ensure the compiler isn't emitting warnings about CPU features
            os_specific_options.append("-march=native")

        # Run the native-image compiler
        try:
            subprocess.check_output(
                [
                    self.native_image_exe,
                    "-cp",
                    os.pathsep.join(self.base_classpath + classpath),
                    "--no-fallback",
                    "-Ob",
                    "--enable-native-access=org.graalvm.truffle",
                    "--enable-native-access=ALL-UNNAMED",
                    "-J--sun-misc-unsafe-memory-access=allow",
                    "-H:+UnlockExperimentalVMOptions",
                    "-H:-StrictQueryCodeCompilation",
                    *os_specific_options,
                    main_class,
                    output_file,
                ],
                stderr=subprocess.STDOUT,
                env=env,
            )
        except subprocess.CalledProcessError as e:
            print("Error in native-image call:")
            print(e.stdout.decode())
            raise e
