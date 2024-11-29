--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;

package body Langkit_Support.Slocs is

   -------------
   -- Compare --
   -------------

   function Compare
     (Reference, Compared : Source_Location) return Relative_Position
   is
   begin
      --  First compare line numbers...

      if Compared.Line < Reference.Line then
         return Before;
      elsif Reference.Line < Compared.Line then
         return After;

      --  Past this point, we know that both are on the same line, so now
      --  compare column numbers.

      elsif Compared.Column < Reference.Column then
         return Before;
      elsif Reference.Column < Compared.Column then
         return After;
      else
         return Inside;
      end if;
   end Compare;

   -------------
   -- Compare --
   -------------

   function Compare
     (Sloc_Range : Source_Location_Range;
      Sloc       : Source_Location) return Relative_Position
   is
      Inclusive_End_Sloc : Source_Location := End_Sloc (Sloc_Range);
   begin
      --  End_Sloc returns an exclusive end sloc. Switch to an inclusive
      --  representation for computation.

      Inclusive_End_Sloc.Column := Inclusive_End_Sloc.Column - 1;

      return (case Compare (Start_Sloc (Sloc_Range), Sloc) is
                 when Before => Before,
                 when Inside | After =>
                   (if Compare (Inclusive_End_Sloc, Sloc) = After
                    then After
                    else Inside));
   end Compare;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Source_Location_Range) return Boolean is
   begin
      if L.Start_Line < R.Start_Line then
         return True;
      elsif L.Start_Line > R.Start_Line then
         return False;
      end if;

      pragma Assert (L.Start_Line = R.Start_Line);
      if L.Start_Column < R.Start_Column then
         return True;
      elsif L.Start_Column > R.Start_Column then
         return False;
      end if;

      pragma Assert (L.Start_Column = R.Start_Column);
      if L.End_Line > R.End_Line then
         return True;
      elsif L.End_Line < R.End_Line then
         return False;
      end if;

      pragma Assert (L.End_Line = R.End_Line);
      if L.End_Column > R.End_Column then
         return True;
      elsif L.End_Column < R.End_Column then
         return False;
      end if;

      pragma Assert (L.End_Column = R.End_Column);
      return False;
   end "<";

   -----------
   -- Value --
   -----------

   function Value (T : Text_Type) return Source_Location is
      Colon_Index  : constant Natural := Index (T, ":");
      Line_Slice   : Text_Type renames T (T'First .. Colon_Index - 1);
      Column_Slice : Text_Type renames T (Colon_Index + 1 .. T'Last);
      Line         : Line_Number;
      Column       : Column_Number;
   begin
      if Colon_Index = 0 then
         raise Constraint_Error with "invalid source location";
      end if;

      begin
         Line := Line_Number'Wide_Wide_Value (Line_Slice);
      exception
         when Constraint_Error =>
            raise Constraint_Error with
               "invalid line number: "
               & Image (Line_Slice, With_Quotes => True);
      end;

      begin
         Column := Column_Number'Wide_Wide_Value (Column_Slice);
      exception
         when Constraint_Error =>
            raise Constraint_Error with
               "invalid column number: "
               & Image (Column_Slice, With_Quotes => True);
      end;

      return (Line, Column);
   end Value;

   -----------
   -- Value --
   -----------

   function Value (T : Text_Type) return Source_Location_Range is
      Dash_Index  : constant Natural := Index (T, "-");
      Start_Slice : Text_Type renames T (T'First .. Dash_Index - 1);
      End_Slice   : Text_Type renames T (Dash_Index + 1 .. T'Last);
   begin
      return Make_Range (Value (Start_Slice), Value (End_Slice));
   end Value;

   ------------------
   -- Column_Count --
   ------------------

   function Column_Count
     (Line     : Text_Type;
      Tab_Stop : Positive := Default_Tab_Stop;
      Start    : Column_Number := 1) return Column_Number
   is
      TS     : constant Column_Number := Column_Number (Tab_Stop);
      Result : Column_Number := Start - 1;
   begin
      --  Make horizontal tabulations move by stride of Tab_Stop columns, as
      --  usually implemented in code editors.

      for C of Line loop
         if C = Chars.HT then
            Result := (Result + TS) / TS * TS;
         else
            Result := Result + 1;
         end if;
      end loop;

      return Result - Start + 1;
   end Column_Count;

end Langkit_Support.Slocs;
