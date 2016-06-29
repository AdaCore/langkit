------------------------------------------------------------------------------
--                               A D A L O G                                --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Logic_Var_Predicate; use Adalog.Logic_Var_Predicate;

generic
   type Logic_Var_Type is private;
   type Element_Type is private;

   with procedure Reset (Self : in out Logic_Var_Type) is <>;
   with function Is_Defined (Self : Logic_Var_Type) return Boolean is <>;

   with function SetL
     (Self : in out Logic_Var_Type; Data : Element_Type) return Boolean
     is <> with Inline => True;

   with function GetL (Self : Logic_Var_Type) return Element_Type
     is <> with Inline => True;

   with function Create return Logic_Var_Type is <>;

   with function Get_Pending_Predicates
     (Self : Logic_Var_Type) return Pred_Sets.Set is <>;
   --  Get the predicates associated to this logic variables

   with procedure Add_Predicate (Self : Logic_Var_Type; Pred : Var_Predicate)
     is <>;
   --  Add a new predicate to the predicates associated to this logic variable

   with procedure Remove_Predicate
     (Self : Logic_Var_Type; Pred : Var_Predicate) is <>;
   --  Remove the predicate Pred from the set of predicates associated to the
   --  logic variable.

package Adalog.Logic_Var is
   subtype Var is Logic_Var_Type;

   type Var_Array is array (Natural range <>) of Var;
   type Val_Array is array (Natural range <>) of Element_Type;
   --  Array types for array of variables and array of values of this variable,
   --  for convenience. To be used in other generic packages taking a formal
   --  Logic_Var package as argument.

end Adalog.Logic_Var;
