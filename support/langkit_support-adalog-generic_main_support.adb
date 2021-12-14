------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
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

   procedure Solve_All (Rel : Relation) is

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

      Solutions              : Solution_Vectors.Vector;
      Solutions_Without_Opts : Solution_Vectors.Vector;

      function Solution_Callback (Vars : Logic_Var_Array) return Boolean;
      --  Callback for ``Solve``. Print the given solution and append it to
      --  ``Solutions``, then return True to continue looking for other
      --  solutions.

      function Image is new Langkit_Support.Images.Array_Image
        (Var_And_Val, Positive, Solution);

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

   begin
      Put_Line ("Solving relation:");
      Print_Relation (Rel);

      case Kind is
      when Symbolic =>
         --  Solve both without and with optimizations

         Put_Line ("... without optimizations:");
         Solve (Rel, Solution_Callback'Access, (Cut_Dead_Branches => False));
         New_Line;
         Solutions_Without_Opts := Solutions;
         Solutions := Solution_Vectors.Empty_Vector;

         Put_Line ("... cut dead branches:");
         Solve (Rel, Solution_Callback'Access, (Cut_Dead_Branches => True));
         New_Line;

         --  Check that we had the same results in both cases

         if not Equivalent (Solutions_Without_Opts, Solutions) then
            Put_Line ("ERROR: solutions are not the same");
            New_Line;
         end if;

         --  Clean up, we are done

         Free (Solutions_Without_Opts);
         Free (Solutions);

      when State_Machine =>
         Solve (Rel, Solution_Callback'Access, (Cut_Dead_Branches => True));
         Free (Solutions);
         New_Line;
      end case;

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

   ------------------
   -- Setup_Traces --
   ------------------

   procedure Setup_Traces is
      package Env renames Ada.Environment_Variables;
   begin
      GNATCOLL.Traces.Parse_Config_File;
      if Env.Exists ("ADALOG_SOLVER_KIND") then
         if Env.Value ("ADALOG_SOLVER_KIND") = "SYM" then
            T_Solver.Set_Kind (Symbolic);
         elsif Env.Value ("ADALOG_SOLVER_KIND") = "SSM" then
            T_Solver.Set_Kind (State_Machine);
         else
            raise Program_Error
              with "Invalid value for env var ""ADALOG_SOLVER_KIND""";
         end if;
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
