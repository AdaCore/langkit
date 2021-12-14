------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Solver_Interface;
with Langkit_Support.Adalog.Unify;

--  Convenience wrapper generic package that, from a type implementing
--  equality, will instantiate all the needed stuff to create logical
--  equations containing that type, namely:
--
--  * A Logic_Ref instantiation so that you can have logical variables holding
--  values of the type.
--
--  * An instantiation of Unify, so that you can do logical equality comparison
--  between logic references and real instances of the type.

generic
   with package Solver_Ifc
     is new Langkit_Support.Adalog.Solver_Interface (<>);
package Langkit_Support.Adalog.Eq_Same is

   use Solver_Ifc;
   subtype Value_Type is Solver_Ifc.Logic_Vars.Value_Type;
   use type Solver_Ifc.Logic_Vars.Value_Type;

   type Dummy_Convert_Data is null record;
   No_Data : constant Dummy_Convert_Data := (null record);

   function Convert
     (C_Data : Dummy_Convert_Data; From : Value_Type) return Value_Type
     with Inline;

   function Image (Dummy : Dummy_Convert_Data) return String is ("");

   type Dummy_Equals_Data is null record;
   No_Equals_Data : constant Dummy_Equals_Data := (null record);

   function Equals
     (Dummy_Data : Dummy_Equals_Data; L, R : Value_Type) return Boolean
   is (L = R);

   function Image (Dummy_Data : Dummy_Equals_Data) return String is ("");

   procedure Inc_Ref (Val : Value_Type) is null;
   procedure Dec_Ref (Val : in out Value_Type) is null;
   procedure Converter_Inc_Ref (Self : Dummy_Convert_Data) is null;
   procedure Converter_Dec_Ref (Self : in out Dummy_Convert_Data) is null;
   procedure Equals_Data_Inc_Ref (Self : Dummy_Equals_Data) is null;
   procedure Equals_Data_Dec_Ref (Self : in out Dummy_Equals_Data) is null;

   package Raw_Impl is new Unify
     (L_Type => Value_Type,
      R_Type => Value_Type,

      Left_C_Data  => Dummy_Convert_Data,
      Right_C_Data => Dummy_Convert_Data,
      No_L_Data    => No_Data,
      No_R_Data    => No_Data,

      Equals_Data    => Dummy_Equals_Data,
      No_Equals_Data => No_Equals_Data,

      Left_Var  => Solver_Ifc.Logic_Vars,
      Right_Var => Solver_Ifc.Logic_Vars,

      L_Inc_Ref            => Inc_Ref,
      R_Inc_Ref            => Inc_Ref,
      Left_C_Data_Inc_Ref  => Converter_Inc_Ref,
      Right_C_Data_Inc_Ref => Converter_Inc_Ref,
      Equals_Data_Inc_Ref  => Equals_Data_Inc_Ref,
      L_Dec_Ref            => Dec_Ref,
      R_Dec_Ref            => Dec_Ref,
      Left_C_Data_Dec_Ref  => Converter_Dec_Ref,
      Right_C_Data_Dec_Ref => Converter_Dec_Ref,
      Equals_Data_Dec_Ref  => Equals_Data_Dec_Ref);

   subtype Raw_Member_Array is Raw_Impl.Unify_Left.R_Type_Array;

   --  This package can be used to provide custom bind operations, with a
   --  custom conversion from Value_Type to Value_Type.

   generic
      type Converter is private;
      No_Data : Converter;

      with procedure Converter_Inc_Ref (Self : Converter);
      with procedure Converter_Dec_Ref (Self : in out Converter);

      with function Convert
        (Data : Converter; From : Value_Type) return Value_Type;

      with function Image (Data : Converter) return String is <>;

      type Equals_Data is private;
      No_Equals_Data : Equals_Data;

      with procedure Equals_Data_Inc_Ref (Self : Equals_Data);
      with procedure Equals_Data_Dec_Ref (Self : in out Equals_Data);

      with function Equals
        (Eq_Data : Equals_Data; L, R : Value_Type) return Boolean is <>;

      with function Image (Eq_Data : Equals_Data) return String is <>;

      One_Side_Convert : Boolean := False;

   package Raw_Custom_Bind is

      package Impl is new Unify
        (Value_Type,  --  L_Type
         Value_Type,  --  R_Type

         Converter,  --  Left_C_Data
         Converter,  --  Right_C_Data
         No_Data,    --  No_L_Data
         No_Data,    --  No_R_Data

         Equals_Data,
         No_Equals_Data,

         Convert,
         Convert,

         Image,
         Image,
         Equals,
         Equals,
         Image,

         Left_Var  => Logic_Vars,
         Right_Var => Logic_Vars,

         L_Inc_Ref            => Inc_Ref,
         R_Inc_Ref            => Inc_Ref,
         Left_C_Data_Inc_Ref  => Converter_Inc_Ref,
         Right_C_Data_Inc_Ref => Converter_Inc_Ref,
         Equals_Data_Inc_Ref  => Equals_Data_Inc_Ref,
         L_Dec_Ref            => Dec_Ref,
         R_Dec_Ref            => Dec_Ref,
         Left_C_Data_Dec_Ref  => Converter_Dec_Ref,
         Right_C_Data_Dec_Ref => Converter_Dec_Ref,
         Equals_Data_Dec_Ref  => Equals_Data_Dec_Ref,
         One_Side_Convert     => One_Side_Convert);

      function Create
        (L, R      : Logic_Vars.Logic_Var;
         Data      : Converter;
         Eq_Data   : Equals_Data;
         Sloc_Info : String_Access := null) return Relation
      is
        (Impl.Equals (L, R, Data, Data, Eq_Data, Sloc_Info));

      function Create
        (L         : Logic_Vars.Logic_Var;
         R         : Value_Type;
         Data      : Converter;
         Eq_Data   : Equals_Data;
         Sloc_Info : String_Access := null) return Relation
      is
        (Impl.Equals (L, R, Data, Eq_Data, Sloc_Info));

      function Create
        (L         : Value_Type;
         R         : Logic_Vars.Logic_Var;
         Data      : Converter;
         Eq_Data   : Equals_Data;
         Sloc_Info : String_Access := null) return Relation
      is
        (Impl.Equals (L, R, Data, Eq_Data, Sloc_Info));

   end Raw_Custom_Bind;

end Langkit_Support.Adalog.Eq_Same;
