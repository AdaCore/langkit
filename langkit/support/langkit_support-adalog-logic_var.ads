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

pragma Warnings (Off);

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;

--  This package is meant to be used as a generic formal package, representing
--  the interface to a logic variable. It is used so that the interface and the
--  implementation of a logic variable can be decoupled. In practice this is
--  only used so we can have two different implementations that vary only in
--  terms of memory management:
--
--  - One is a refcounted, controlled-object based logic variable, so its
--  memory management is automatic.
--
--  - The other is a naked access, so memory management is left to the user.
--
--  As many things in Adalog, this choices hinges on the hypothesis that Adalog
--  could be used directly, and not in a code generation context, which is
--  still unclear.

generic
   type Logic_Var_Type is private;
   type Element_Type is private;

   with procedure Inc_Ref (E : Element_Type);
   with procedure Dec_Ref (E : in out Element_Type);

   with procedure Reset (Self : Logic_Var_Type) is <>;
   --  Reset the logic variable to an undefined state with no value

   with function Is_Defined (Self : Logic_Var_Type) return Boolean is <>;
   --  Checks whether the logic variable has a value or not

   with procedure Set_Value (Self : Logic_Var_Type; Data : Element_Type)
     is <> with Inline => True;
   --  Set the value of the logic variable to Data. Low level function, not for
   --  use by clients.

   with function Get_Value (Self : Logic_Var_Type) return Element_Type
     is <> with Inline => True;
   --  Get the value stored in Self

   with function Create return Logic_Var_Type is <>;
   --  Return a new logic variable

   with function Image (Self : Logic_Var_Type) return String is <>;
   --  Return a string image of Self

   with function Element_Image (Self : Element_Type) return String is <>;
   --  Return a string image of Self

   with function Id (Self : Logic_Var_Type) return Natural is <>;
   --  Return the Id of this variable. Variables have an Id of 0 by default, >
   --  0 when set by the solver.

   with procedure Set_Id (Self : Logic_Var_Type; Id : Natural) is <>;
   --  Set the Id for logic variable. If 0, resets the Id of logic variable.
   --  Setting to 0 will also unalias the variable, if it was aliased.

   with procedure Alias (Self, Other : Logic_Var_Type) is <>;
   --  Alias this variable to another variable. This

   with procedure Unalias (Self : Logic_Var_Type) is <>;

   with function Get_Alias (Self : Logic_Var_Type) return Logic_Var_Type is <>;
   --  Get the alias for this logic variable, if there is one.

   No_Var : Logic_Var_Type;
package Langkit_Support.Adalog.Logic_Var is
   subtype Var is Logic_Var_Type;
   --  Subtype so that the variable type is visible from the outside, since
   --  formals are not visible.

   type Var_Array is array (Positive range <>) of Var;
   type Val_Array is array (Positive range <>) of Element_Type;
   --  Array types for array of variables and array of values of this variable,
   --  for convenience. To be used in other generic packages taking a formal
   --  Logic_Var package as argument.

end Langkit_Support.Adalog.Logic_Var;
