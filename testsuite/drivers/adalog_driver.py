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
    """

    default_process_timeout = 300

    def run(self):
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
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog; use Langkit_Support.Adalog;

with Main;
{solver_with_clause}

procedure Adalog_Main is
    use {main_support_name}.T_Solver;
begin
    {main_support_name}.Setup_Traces;

    {main_support_name}.T_Solver.Set_Kind (Symbolic);
    Put_Line ("Solving with new solver");
    Put_Line ("=======================");
    Put_Line ("");
    Main;
    New_Line;

    begin
        Set_Kind (State_Machine);
        Put_Line ("Solving with old solver");
        Put_Line ("=======================");
        Put_Line ("");
        Main;
        New_Line;
    exception
        when Langkit_Support.Adalog.Early_Binding_Error =>
            Put_Line ("Resolution failed with Early_Binding_Error");
    end;
    {main_support_name}.Finalize;
end Adalog_Main;
                """
            )
        self.gprbuild("p.gpr")
        self.run_and_check(
            [self.program_path(main)],
            for_coverage=True,
            memcheck=True,
        )
