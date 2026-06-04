from enum import StrEnum
import os
import subprocess

from drivers.base_driver import BaseDriver
from e3.testsuite import logger
from python_support.java_api_helper import JavaApiHelper


class JavaMode(StrEnum):
    """
    Mode to run a Java application in.
    """

    JNI = "jni"
    GRAAL_C_API = "graal_c_api"


class JavaDriver(BaseDriver):
    """
    Driver to test Liblktlang's Java bindings.
    """

    def set_up(self):
        super().set_up()
        self.java_helper = JavaApiHelper("lkt", self.env.lkt_java_bindings)

    @property
    def mode(self) -> JavaMode:
        """
        Return the Java execution mode for this test ("graal_c_api" or "jni").
        """
        return JavaMode(self.test_env["mode"])

    @staticmethod
    def build_lkt_java_bindings(
        maven_executable: str | None,
        maven_local_repo: str | None,
        lkt_java_bindings_dir: str,
    ) -> None:
        """
        Run Maven to build the Liblktlang's Java bindings.
        """
        logger.info("Compile Lkt Java bindings...")

        # Create arguments list
        args = [
            maven_executable or "mvn",
            "-q",
            "-f",
            lkt_java_bindings_dir,
            "package",
        ]

        # If a Maven local repository is provided, set Maven to offline mode
        if maven_local_repo is not None:
            args.extend(["-o", f"-Dmaven.repo.local={maven_local_repo}"])

        # Then spawn the Maven process
        try:
            subprocess.check_output(args, stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as e:
            logger.error(f"Error in Maven call:\n{e.stdout.decode()}")
            raise e

    def run(self) -> None:
        main_class = "Test"
        classpath = [self.working_dir()]

        # Compile the Test.java file
        self.java_helper.javac(
            self.working_dir(f"{main_class}.java"),
            self.working_dir(),
            classpath,
        )

        if self.mode == JavaMode.GRAAL_C_API:
            # Compile the Test class to an executable with native image and run
            # it.
            test_exe = self.working_dir("test")
            self.java_helper.native_image(main_class, test_exe, classpath)
            self.run_and_check([test_exe])

        else:
            # Get the java.library.path from LD_LIBRARY_PATH and compiled JNI
            # stubs.
            java_library_path = os.pathsep.join(
                [
                    self.java_helper.bindings_dir("jni"),
                    self.java_helper.bindings_dir("jni_stubs"),
                    os.environ[
                        "LD_LIBRARY_PATH" if os.name != "nt" else "PATH"
                    ],
                ],
            )

            # Run the Java Test class
            self.run_and_check(
                [
                    self.java_helper.java_exe,
                    "-cp",
                    os.pathsep.join(
                        self.java_helper.base_classpath + classpath
                    ),
                    "-Dfile.encoding=UTF-8",
                    "--enable-native-access=ALL-UNNAMED",
                    f"-Djava.library.path={java_library_path}",
                    main_class,
                ]
            )
