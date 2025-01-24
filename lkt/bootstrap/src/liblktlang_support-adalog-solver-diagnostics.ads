with Liblktlang_Support.Vectors;

private generic
package Liblktlang_Support.Adalog.Solver.Diagnostics is
   use Solver_Ifc;
   use Logic_Vars;

   type Diagnostic_Accumulator is private;

   procedure Register_Failure
     (Accumulator : in out Diagnostic_Accumulator;
      Failed_Atom : Atomic_Relation_Type;
      Contexts    : Logic_Context_Vectors.Vector;
      Round       : Natural);
   --  Report the reason why a candidate solution was rejected by giving the
   --  atom which evaluation failed, together with the set of contexts which
   --  were used to explain the failure, and the current solver round. These
   --  additional informations are used by the accumulator to remove
   --  redundant diagnostics. Note that ``Contexts`` will be copied here if
   --  needed, therefore ownership stays in the caller's hands.

   procedure Emit_All_And_Destroy
     (Accumulator : in out Diagnostic_Accumulator;
      Emitter     : Diagnostic_Emitter);
   --  Emit all the diagnostics that were deemed relevant by the accumulator
   --  by forwarding them to the ``Emitted`` callback. Free all memory
   --  allocated to store internal diagnostics.

   procedure Destroy (Accumulator : in out Diagnostic_Accumulator);
   --  Free all memory allocated to store internal diagnostics
private
   type Value_Array_Access is access all Value_Array;

   type Internal_Diagnostic is record
      Atom     : Atomic_Relation_Type;
      Values   : Value_Array_Access;
      Contexts : Logic_Context_Vectors.Vector;
      Round    : Natural;
   end record;

   package Internal_Diagnostic_Vectors is new Vectors
     (Internal_Diagnostic);

   type Diagnostic_Accumulator is record
      Diagnostics : Internal_Diagnostic_Vectors.Vector;
      --  Holds all the diagnostic from all rounds

      Last_Round  : Integer := -1;
      --  The round during which the last diagnostic was emitted

      Last_Diag_Index_Of_Previous_Round : Natural := 0;
      --  The index of the last diagnostic emitted during the previous round
   end record;
end Liblktlang_Support.Adalog.Solver.Diagnostics;
