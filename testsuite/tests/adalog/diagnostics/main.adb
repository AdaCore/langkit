--  Check that basic diagnostic generation upon solver resolution failures
--  works as expected, and in particular that all relevant logic contexts
--  (and only those) are forwarded to the predicate's diagnostic generation
--  procedure.
--  Note that in this test we print the diagnostics to stdout directly after
--  creating them without using the ``Diagnostic_Emitter`` callback, for
--  simplicity.

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Adalog.Solver;
with Langkit_Support.Adalog.Solver_Interface;
with Langkit_Support.Images;

procedure Main is
   ------------------------------------------------------
   -- Custom diagnostics and logic context definitions --
   ------------------------------------------------------

   type Logic_Context is record
      Description: Unbounded_String;
   end record;

   type Logic_Context_Access is access all Logic_Context;

   function Image (Ctx : Logic_Context_Access) return String is
     (To_String (Ctx.Description));

   procedure Free is new Ada.Unchecked_Deallocation
     (Logic_Context, Logic_Context_Access);

   type Diagnostic is record
      Msg  : Unbounded_String;
   end record;

   --------------------------
   -- Solver instantiation --
   --------------------------

   package Refs is new Langkit_Support.Adalog.Logic_Var
     (Integer, Value_Image => Langkit_Support.Images.Stripped_Image);

   procedure Free is new Ada.Unchecked_Deallocation
     (Refs.Logic_Var_Record, Refs.Logic_Var);

   package Solver_Ifc is new Langkit_Support.Adalog.Solver_Interface
     (Refs, Logic_Context, Logic_Context_Access, Image, "=", Free, Diagnostic);

   package Solver is new Langkit_Support.Adalog.Solver (Solver_Ifc);

   ---------------------------------
   -- Custom predicate definition --
   ---------------------------------

   type Is_Congruent_Predicate is new Solver_Ifc.Predicate_Type with record
      Var_Name : Unbounded_String;
      Modulus  : Natural;
      Value    : Integer;
   end record;

   overriding function Call
     (Self : Is_Congruent_Predicate; Val : Integer) return Boolean
   is (Val mod Self.Modulus = Self.Value mod Self.Modulus);

   overriding procedure Failed
     (Self     : Is_Congruent_Predicate;
      Val      : Integer;
      Ctxs     : Solver_Ifc.Logic_Context_Array;
      Round    : Natural;
      Emitter  : Solver_Ifc.Diagnostic_Emitter)
   is
      I : Natural := 0;
   begin
      for Ctx of reverse Ctxs loop
         Put ("with " & To_String (Ctx.Description));
         if I /= Ctxs'Last then
            Put (", ");
         end if;
         I := I + 1;
      end loop;
      Put_Line
        (To_String (Self.Var_Name)
         & " is not congruent to"
         & Self.Value'Image
         & " modulo"
         & Self.Modulus'Image);
   end Failed;

   ----------------------------------
   -- Custom propagator definition --
   ----------------------------------

   function Add_One (X : Integer) return Integer is (X + 1);

   Add_One_Converter : constant Solver_Ifc.Converter_Type'Class :=
     Solver_Ifc.Converter (Add_One'Access, "add_one");

   ----------------------
   -- Relation helpers --
   ----------------------

   package Relation_Vectors is new Ada.Containers.Vectors
     (Positive, Solver.Relation, Solver."=");

   All_Rels : Relation_Vectors.Vector;

   --------------
   -- Register --
   --------------

   function Register (Rel : Solver.Relation) return Solver.Relation is
   begin
      All_Rels.Append (Rel);
      return Rel;
   end Register;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      for Rel of All_Rels loop
         Solver.Dec_Ref (Rel);
      end loop;
   end Finalize;

   ------------
   -- Assign --
   ------------

   function Assign
     (Var : Refs.Logic_Var; Value : Integer) return Solver.Relation
   is (Register (Solver.Create_Assign
        (Var, Value,
         Logic_Ctx => new Logic_Context'(Description => To_Unbounded_String
            (Var.Dbg_Name.all & " set to" & Value'Image)))));

   -----------
   -- Unify --
   -----------

   function Unify
     (First, Second : Refs.Logic_Var) return Solver.Relation
   is (Register (Solver.Create_Unify
        (First, Second,
         Logic_Ctx => new Logic_Context'(Description => To_Unbounded_String
           (First.Dbg_Name.all & " unified with " & Second.Dbg_Name.all)))));

   ------------------------
   -- Propagate_Plus_One --
   ------------------------

   function Propagate_Plus_One
     (From, To : Refs.Logic_Var) return Solver.Relation
   is (Register (Solver.Create_Propagate
        (From, To, Conv => Add_One_Converter,
         Logic_Ctx => new Logic_Context'
          (Description => To_Unbounded_String
            (To.Dbg_Name.all & " set to " & From.Dbg_Name.all & " + 1")))));

   ----------------------
   -- Assert_Congruent --
   ----------------------

   function Assert_Congruent
     (Var : Refs.Logic_Var; Value : Integer; Modulus : Natural)
      return Solver.Relation
   is (Register (Solver.Create_Predicate
        (Var, Is_Congruent_Predicate'
           (Var_Name    => To_Unbounded_String (Var.Dbg_Name.all),
            Modulus     => Modulus,
            Value       => Value,
            Cache_Set   => False,
            Cache_Key   => <>,
            Cache_Value => <>,
            Ref_Count   => 1))));

   ------------------------
   -- Problem definition --
   ------------------------

   Var_X : Refs.Logic_Var :=
     new Refs.Logic_Var_Record'
       (Dbg_Name => new String'("X"), Value => 0, others => <>);

   Var_Y : Refs.Logic_Var :=
     new Refs.Logic_Var_Record'
       (Dbg_Name => new String'("Y"), Value => 0, others => <>);

   Attempt_1 : Solver.Relation := Register (Solver.Create_All
     ((Assert_Congruent (Var_X, 1, 2),
       Assign (Var_Y, 5),
       Assign (Var_X, 4))));

   Attempt_2 : Solver.Relation := Register (Solver.Create_All
     ((Assert_Congruent (Var_X, 0, 2),
       Assign (Var_Y, 5),
       Unify (Var_X, Var_Y))));

   Attempt_3 : Solver.Relation := Register (Solver.Create_All
     ((Assert_Congruent (Var_X, 1, 2),
       Propagate_Plus_One (Var_Y, Var_X),
       Assign (Var_Y, 5))));

   Problem : constant Solver.Relation := Register (Solver.Create_Any
     ((Attempt_1, Attempt_2, Attempt_3)));
begin
   GNATCOLL.Traces.Parse_Config_File;

   if Solver.Solve_First (Problem, (Report_Errors => True)) then
      raise Program_Error with "Should not resolve successfully!";
   end if;

   Finalize;
   Refs.Destroy (Var_X.all);
   Refs.Destroy (Var_Y.all);
   Free (Var_X);
   Free (Var_Y);
end Main;
