--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Images;
with Langkit_Support.Vectors;

package body Langkit_Support.Adalog.Generic_Main_Support is

   procedure Free is new Ada.Unchecked_Deallocation
     (Logic_Var_Record, Refs.Logic_Var);

   --  The following booleans determine which solver configurations to run in
   --  tests. The ``Setup_Traces`` procedure initializes them with default
   --  values, possibly modified depending on environment variables, for
   --  convenience when debugging tests.

   Run_Sym_Without_Opts : Boolean;
   Run_Sym_With_Opts    : Boolean;

   ------------
   -- Create --
   ------------

   function Create (Name : String) return Refs.Logic_Var is
   begin
      return R : constant Refs.Logic_Var := new Logic_Var_Record do
         Reset (R);
         R.Dbg_Name := new String'(Name);
         Variables.Append (R);
      end return;
   end Create;

   ---------
   -- "+" --
   ---------

   function "+" (R : Relation) return Relation is
   begin
      Relations.Append (R);
      return R;
   end "+";

   ---------------
   -- Solve_All --
   ---------------

   procedure Solve_All (Rel : Relation; Timeout : Natural := 0) is

      type Var_And_Val is record
         Var     : Refs.Logic_Var;
         Defined : Boolean;
         Val     : T;
      end record;
      --  We want to keep track of the various solutions found. Since solution
      --  values are stored in variables, this means that we need to save the
      --  values when a solution is found, as the next solution will override
      --  them in variables.

      function Image (Self : Var_And_Val) return String
      is
        (Refs.Image (Self.Var) & " = "
         & (if Self.Defined
            then Image (Self.Val)
            else "<undefined>"));

      type Solution is array (Positive range <>) of Var_And_Val;
      type Solution_Access is access all Solution;
      procedure Free is new Ada.Unchecked_Deallocation
        (Solution, Solution_Access);

      package Solution_Vectors is new Langkit_Support.Vectors
        (Solution_Access);
      procedure Free (Self : in out Solution_Vectors.Vector);
      --  Free all solutions in Self and destroy the vector

      function Equivalent
        (Left, Right : Solution_Vectors.Vector) return Boolean;
      --  Return whether the two vectors of solutions are equal

      Solutions              : Solution_Vectors.Vector :=
        Solution_Vectors.Empty_Vector;
      Solutions_Without_Opts : Solution_Vectors.Vector :=
        Solution_Vectors.Empty_Vector;

      function Solution_Callback (Vars : Logic_Var_Array) return Boolean;
      --  Callback for ``Solve``. Print the given solution and append it to
      --  ``Solutions``, then return True to continue looking for other
      --  solutions.

      function Image is new Langkit_Support.Images.Array_Image
        (Var_And_Val, Positive, Solution);

      procedure Run_Solve (Opts : Solve_Options_Type);
      --  Wrapper around the ``Solve`` procedure, to catch and display uncaught
      --  exceptions.

      ----------
      -- Free --
      ----------

      procedure Free (Self : in out Solution_Vectors.Vector) is
         S_Mut : Solution_Access;
      begin
         for S of Self loop
            S_Mut := S;
            Free (S_Mut);
         end loop;
         Self.Destroy;
      end Free;

      ----------------
      -- Equivalent --
      ----------------

      function Equivalent
        (Left, Right : Solution_Vectors.Vector) return Boolean
      is
      begin
         if Left.Length /= Right.Length then
            return False;
         end if;

         for I in 1 .. Left.Length loop
            declare
               S_L : Solution renames Left.Get (I).all;
               S_R : Solution renames Right.Get (I).all;
            begin
               if S_L'Length /= S_R'Length then
                  return False;
               end if;

               for J in S_L'Range loop
                  declare
                     L : Var_And_Val renames S_L (J);
                     R : Var_And_Val renames S_R (J);
                  begin
                     if Refs."/=" (L.Var, R.Var)
                        or else L.Defined /= R.Defined
                        or else (L.Defined and then L.Val /= R.Val)
                     then
                        return False;
                     end if;
                  end;
               end loop;
            end;
         end loop;

         return True;
      end Equivalent;

      -----------------------
      -- Solution_Callback --
      -----------------------

      function Solution_Callback (Vars : Logic_Var_Array) return Boolean is
         S : constant Solution_Access := new Solution (Vars'Range);
      begin
         for I in Vars'Range loop
            declare
               V : Refs.Logic_Var renames Vars (I);
            begin
               S (I) := (Var     => V,
                         Defined => Refs.Is_Defined (V),
                         Val     => Refs.Get_Value (V));
            end;
         end loop;
         Solutions.Append (S);

         Put_Line ("Solution: " & Image (S.all));
         return True;
      end Solution_Callback;

      ---------------
      -- Run_Solve --
      ---------------

      procedure Run_Solve (Opts : Solve_Options_Type) is
      begin
         Solve (Rel, Solution_Callback'Access, Opts, Timeout);
      exception
         when Early_Binding_Error =>
            Put_Line ("Resolution failed with Early_Binding_Error");
         when Timeout_Error =>
            Put_Line ("Resolution failed with Timeout_Error");
         when Exc : others =>
            Put_Line ("  -> " & Exception_Name (Exc)
                      & ": " & Exception_Message (Exc));
      end Run_Solve;

   begin
      Put_Line ("Solving relation:");
      Put_Line (Image (Rel));

      --  Solve both without and with optimizations

      if Run_Sym_Without_Opts then
         Put_Line ("... without optimizations:");
         Run_Solve ((others => False));
         New_Line;
         Solutions_Without_Opts := Solutions;
         Solutions := Solution_Vectors.Empty_Vector;
      end if;

      if Run_Sym_With_Opts then
         Put_Line ("... with all optimizations:");
         Run_Solve ((others => True));
         New_Line;
      end if;

      --  Check that we had the same results in both cases

      if Run_Sym_Without_Opts
         and then Run_Sym_With_Opts
         and then not Equivalent (Solutions_Without_Opts, Solutions)
      then
         Put_Line ("ERROR: solutions are not the same");
         New_Line;
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

      --  Clean up, we are done

      Free (Solutions_Without_Opts);
      Free (Solutions);

   exception
      when others =>
         Free (Solutions_Without_Opts);
         Free (Solutions);
         raise;
   end Solve_All;

   ---------
   -- "-" --
   ---------

   function "-" (S : String) return String_Access is
   begin
      return Result : constant String_Access := new String'(S) do
         Strings.Append (Result);
      end return;
   end "-";

   --------------
   -- Run_Main --
   --------------

   procedure Run_Main (Main : access procedure) is
   begin
      Setup_Traces;
      Main.all;
      Finalize;
   end Run_Main;

   ------------------
   -- Setup_Traces --
   ------------------

   procedure Setup_Traces is
      package Env renames Ada.Environment_Variables;
      Var_Name : constant String := "ADALOG_SOLVER_CFG";
      Cfg      : constant String := Env.Value (Var_Name, Default => "");
   begin
      GNATCOLL.Traces.Parse_Config_File;

      Run_Sym_Without_Opts := False;
      Run_Sym_With_Opts := False;

      if Cfg = "" then
         Run_Sym_Without_Opts := True;
         Run_Sym_With_Opts := True;
      elsif Cfg = "sym" then
         Run_Sym_Without_Opts := True;
      elsif Cfg = "sym-opts" then
         Run_Sym_With_Opts := True;
      else
         raise Program_Error
           with "Invalid value for env var """ & Var_Name & """";
      end if;
   end Setup_Traces;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      Used : Boolean := False;
   begin
      for R of Relations loop
         Used := True;
         Dec_Ref (R);
      end loop;

      for V of Variables loop
         Refs.Destroy (V.all);
         Free (V);
      end loop;

      for S of Strings loop
         Free (S);
      end loop;

      Relations := Relation_Vectors.Empty_Vector;
      Variables := Variable_Vectors.Empty_Vector;
      Strings := String_Access_Vectors.Empty_Vector;

      if Used then
         Put_Line ("Done.");
      end if;
   end Finalize;

end Langkit_Support.Adalog.Generic_Main_Support;
