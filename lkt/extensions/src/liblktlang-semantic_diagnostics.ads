with Liblktlang.Analysis;     use Liblktlang.Analysis;
with Liblktlang_Support.Text; use Liblktlang_Support.Text;

package Liblktlang.Semantic_Diagnostics is

   procedure Set_Solver_Debug_Mode (Enable : Boolean);
   --  Enable or disable the solver traces for debugging purposes

   procedure Print_Solver_Diagnostics_In_Unit (Unit : Analysis.Analysis_Unit);
   --  Traverse all nodes in ``Unit`` and print all solver diagnostics their
   --  name resolution finds.

   procedure Print_Solver_Diagnostics_In_Node (Node : Lkt_Node);
   --  Print solver diagnostics for the name resolution of all nodes in the
   --  tree rooted at Node.

   procedure Print_Solver_Diagnostic (Diag : Solver_Diagnostic);
   --  Print a ``Solver_Diagnostic`` to the standard output with the GNU format

   function Render_Solver_Diagnostic
     (Diag : Solver_Diagnostic) return Unbounded_Text_Type;
   --  Create the error message of a ``Solver_Diagnostic`` from its template
   --  and arguments.

end Liblktlang.Semantic_Diagnostics;
