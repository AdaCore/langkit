------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Unify_One_Side is

   use Var;

   function Create
     (Left    : Var.Var;
      Right   : R_Type;
      R_Data  : R_Convert_Data;
      Eq_Data : Equals_Data) return Unify_Rec;
   --  Helper for the public Create function

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out Unify_Rec) return Solving_State is
   begin
      Trace ("In Unify_One_Side");

      if Is_Defined (Self.Left) then
         Trace ("Left defined");

         declare
            Result : Solving_State;
            R_Val : L_Type := Convert (Self.R_Data, Self.Right);
            L_Val : L_Type := Get_Value (Self.Left);
         begin
            if Debug.Debug then
               Trace (L_Image (R_Val));
               Trace (L_Image (L_Val));
            end if;

            if Invert_Equals then
               Result := +Equals (Self.Eq_Data, R_Val, L_Val);
            else
               Result := +Equals (Self.Eq_Data, L_Val, R_Val);
            end if;

            if Debug.Debug then
               Trace ("Returning " & Result'Image);
            end if;

            L_Dec_Ref (R_Val);
            L_Dec_Ref (L_Val);
            return Result;
         end;

      else
         declare
            L_Val : L_Type := Convert (Self.R_Data, Self.Right);
         begin
            Trace ("Set Left from converted Right value");
            Set_Value (Self.Left, L_Val);
            L_Dec_Ref (L_Val);
            Self.Changed := True;
            return Satisfied;
         end;
      end if;

   end Apply;

   ------------
   -- Revert --
   ------------

   procedure Revert (Self : in out Unify_Rec) is
   begin
      if Self.Changed then
         Reset (Self.Left);
         Self.Changed := False;
      end if;
   end Revert;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Unify_Rec) is
   begin
      R_Dec_Ref (Self.Right);
   end Free;

   ------------------
   -- Custom_Image --
   ------------------

   function Custom_Image (Self : Unify_Rec) return String is
      C : constant String :=
        (if Convert_Image = "" then ""
         else " (convert: " & Convert_Image & ")");
      E : constant String :=
        (if Equals_Image = "" then ""
         else " (equals: " & Equals_Image & ")");
   begin
      return ("Unify " & Var.Image (Self.Left) & " <= " & R_Image (Self.Right)
              & C & E);
   end Custom_Image;

   ----------------
   -- Solve_Impl --
   ----------------

   function Solve_Impl
     (Self    : in out Member_T;
      Context : in out Solving_Context) return Solving_State
   is
      pragma Unreferenced (Context);
   begin
      Trace ("In Member");
      if Self.Current_Index in Self.Values.all'Range then
         if Is_Defined (Self.Left) and then not Self.Changed then

            if Self.Domain_Checked then
               Trace ("In Member: left already defined, domain is checked,"
                      & " returning Unsatisfied");
               return Unsatisfied;
            end if;

            Trace ("In Member: left already defined, checking domain");
            Self.Domain_Checked := True;
            for V of Self.Values.all loop
               declare
                  L     : L_Type := Get_Value (Self.Left);
                  R_Val : L_Type := Convert (Self.R_Data, V);
                  B     : constant Boolean := Equals (Self.Eq_Data, L, R_Val);
               begin
                  L_Dec_Ref (L);
                  L_Dec_Ref (R_Val);
                  if B then
                     Trace ("In Member: left already defined, satisfied");
                     return Satisfied;
                  end if;
               end;
            end loop;

            Trace ("In Member: left already defined, unsatisfied");
            return Unsatisfied;

         else
            Self.Current_Index := Self.Current_Index + 1;

            declare
               R_Val : L_Type := Convert
                 (Self.R_Data, Self.Values (Self.Current_Index - 1));
            begin
               Set_Value (Self.Left, R_Val);
               L_Dec_Ref (R_Val);
            end;

            Trace ("In Member: just defined left, satisfied");
            Self.Changed := True;
            return Satisfied;
         end if;

      else
         if Self.Changed then
            Trace ("In Member: changed, resetting left");
            Reset (Self.Left);
         else
            Trace ("In Member: not changed, unsatisfied");
         end if;
         return Unsatisfied;
      end if;
   end Solve_Impl;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Member_T) is
   begin
      Self.Domain_Checked := False;
      if Self.Changed then
         Reset (Self.Left);
         Self.Changed := False;
      end if;
      Self.Current_Index := 1;
   end Reset;

   ------------
   -- Create --
   ------------

   function Create
     (Left    : Var.Var;
      Right   : R_Type;
      R_Data  : R_Convert_Data;
      Eq_Data : Equals_Data) return Unify_Rec is
   begin
      R_Inc_Ref (Right);
      return (Left    => Left,
              Right   => Right,
              Changed => False,
              R_Data  => R_Data,
              Eq_Data => Eq_Data);
   end Create;

   function Create
     (Left      : Var.Var;
      Right     : R_Type;
      R_Data    : R_Convert_Data;
      Eq_Data   : Equals_Data;
      Sloc_Info : String_Access := null) return Relation is
   begin
      --  Don't inc-ref Right here as the call to Create below will do it for
      --  us.
      return new Unify'(Rel       => Create (Left, Right, R_Data, Eq_Data),
                        Sloc_Info => Sloc_Info,
                        others    => <>);
   end Create;

   ------------
   -- Member --
   ------------

   function Member
     (R       : Var.Var;
      Vals    : R_Type_Array;
      R_Data  : R_Convert_Data;
      Eq_Data : Equals_Data) return Relation
   is
   begin

      for V of Vals loop
         R_Inc_Ref (V);
      end loop;

      return new Member_T'
        (Left           => R,
         Values         => new R_Type_Array'(Vals),
         Current_Index  => 1,
         Changed        => False,
         Domain_Checked => False,
         R_Data         => R_Data,
         Eq_Data        => Eq_Data,
         others         => <>);
   end Member;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Member_T) is
      procedure Unchecked_Free
      is new Ada.Unchecked_Deallocation (R_Type_Array, R_Type_Array_Access);
   begin
      for V of Self.Values.all loop
         R_Dec_Ref (V);
      end loop;
      Unchecked_Free (Self.Values);
   end Cleanup;

   ------------------
   -- Custom_Image --
   ------------------

   overriding function Custom_Image (Self : Member_T) return String is
      Res : Unbounded_String;
   begin
      Res := To_Unbounded_String ("Member ");

      Append (Res, Image (Self.Left));
      Append (Res, " {");

      for I in Self.Values.all'Range loop
         Append (Res, R_Image (Self.Values.all (I)));
         if I /= Self.Values.all'Last then
            Append (Res, ", ");
         end if;
      end loop;

      Append (Res, "}");

      return To_String (Res);
   end Custom_Image;

end Langkit_Support.Adalog.Unify_One_Side;
