--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Images;
with Langkit_Support.Vectors;

package body Langkit_Support.Adalog.Generic_Main_Support is

   procedure Free is new Ada.Unchecked_Deallocation
     (Logic_Var_Record, Refs.Logic_Var);

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

      Solutions : Solution_Vectors.Vector := Solution_Vectors.Empty_Vector;

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
         Solve (Rel, Solution_Callback'Access, Opts, null, Timeout);
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
      Run_Solve (Default_Options);
      New_Line;

      --  Clean up, we are done

      Free (Solutions);

   exception
      when others =>
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
      GNATCOLL.Traces.Parse_Config_File;
      Main.all;
      Finalize;
   end Run_Main;

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
