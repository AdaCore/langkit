import os

from drivers.base_driver import BaseDriver


class AdalogDriver(BaseDriver):
    """
    Driver to test the Adalog solver framework.

    The goal of this driver is to run a test twice: once exercizing the old
    solver and another time exercizing the new one. To achieve this, testcases
    are expected to provide a "Main" subprogram (main.adb) to run a solver
    using the generic interface, i.e. the Solver_Ifc generic package
    instantiation, declared in Langkit_Support.Adalog.Generic_Main_Support.

    By default, tests are expected to use the
    Langkit_Support.Adalog.Main_Support package. If they need to use their own
    Generic_Main_Support instantiation, they must add their fully qualified
    name as the "main_support_name" key in their "test.yaml".

    The "solver-cfg" key in "test.yaml" controls which solver is exercized, and
    with which optimization. It is a string (considered empty if omitted), see
    the Langkit_Support.Adalog.Generic_Main_Support package for its usage.
    """

    default_process_timeout = 300

    def run(self):
        solver_cfg = self.test_env.get("solver-cfg", "")
        env = dict(os.environ)
        env["ADALOG_SOLVER_CFG"] = solver_cfg

        self.create_project_file("p.gpr", ["adalog_main.adb"])
        main = "adalog_main.adb"

        main_support_name = self.test_env.get("main_support_name")
        if main_support_name:
            solver_with_clause = "with {};".format(
                ".".join(main_support_name.split(".")[:-1])
            )
        else:
            main_support_name = "Langkit_Support.Adalog.Main_Support"
            solver_with_clause = f"with {main_support_name};"

        with open(self.working_dir(main), "w") as f:
            f.write(
                f"""
with Main;
{solver_with_clause}

procedure Adalog_Main is
begin
    {main_support_name}.Run_Main (Main'Access);
end Adalog_Main;
                """
            )
        self.gprbuild("p.gpr")
        self.run_and_check(
            [self.program_path(main)],
            env=env,
            for_coverage=True,
            memcheck=True,
        )
