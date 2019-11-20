from __future__ import absolute_import, division, print_function

from drivers.base_driver import BaseDriver

DEFAULT_SOLVER_FQN = "Langkit_Support.Adalog.Main_Support"


class AdalogDriver(BaseDriver):
    TIMEOUT = 300

    def run(self):
        self.create_project_file('p.gpr', ["adalog_main.adb"])
        main = "adalog_main.adb"

        main_support_name = self.test_env.get(
            'main_support_name', DEFAULT_SOLVER_FQN
        )

        solver_with_clause = ""
        if main_support_name != DEFAULT_SOLVER_FQN:
            solver_pkg = ".".join(main_support_name.split(".")[:-1])
            if solver_pkg:
                solver_with_clause = f"with {solver_pkg};"

        with open(self.working_dir(main), "w") as f:
            f.write(
                f"""
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog; use Langkit_Support.Adalog;
with Langkit_Support.Adalog.Main_Support;

with Main;
{solver_with_clause}

procedure Adalog_Main is
    use {main_support_name}.T_Solver;
begin
    Set_Kind (Symbolic);
    Put_Line ("Solving with new solver");
    Put_Line ("=======================");
    Put_Line ("");
    Main;

    begin
        Put_Line ("Solving with old solver");
        Put_Line ("=======================");
        Put_Line ("");
        Set_Kind (State_Machine);
        Main;
    exception
        when Langkit_Support.Adalog.Early_Binding_Error =>
            Put_Line ("Resolution failed with Early_Binding_Error");
    end;
    Langkit_Support.Adalog.Main_Support.Finalize;
end Adalog_Main;
                """
            )
        self.gprbuild('p.gpr')
        argv = [self.program_path(main)]
        self.run_and_check(argv, for_coverage=True, memcheck=True)
