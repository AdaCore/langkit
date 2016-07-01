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

with GNATCOLL.Refcount;

with Adalog.Abstract_Relation;   use Adalog.Abstract_Relation;
with Adalog.Logic_Var;
with Adalog.Logic_Var_Predicate; use Adalog.Logic_Var_Predicate;

generic
   type Element_Type is private;
package Adalog.Logic_Ref is

   type Var is record
      Reset             : Boolean := True;
      El                : Element_Type;

      --  List of relations which applications are pending on this variable
      --  being defined. When the variable will be set, relations will be
      --  evaluated.
      Pending_Relations : Pred_Sets.Set;

      Dbg_Name          : String_Access;
   end record;

   procedure Reset (Self : in out Var);
   function Is_Defined (Self : Var) return Boolean;
   function Set_Value (Self : access Var; Data : Element_Type) return Boolean;
   function GetL (Self : Var) return Element_Type;

   --  Var predicates functions

   procedure Add_Predicate (Self : in out Var; Pred : Var_Predicate);
   function Get_Pending_Predicates (Self : Var) return Pred_Sets.Set
   is (Self.Pending_Relations);
   procedure Remove_Predicate (Self : in out Var; Pred : Var_Predicate);

   type Refcounted_El is new GNATCOLL.Refcount.Refcounted with record
      Content : Var;
   end record;

   package Refs is new GNATCOLL.Refcount.Shared_Pointers (Refcounted_El);
   type Ref is new Refs.Ref with null record;

   procedure Reset (Self : in out Ref);
   function Is_Defined (Self : Ref) return Boolean;
   function SetL (Self : in out Ref; Data : Element_Type) return Boolean;
   function GetL (Self : Ref) return Element_Type;

   function Get_Pending_Predicates (Self : Ref) return Pred_Sets.Set
   is (Get_Pending_Predicates (Self.Unchecked_Get.Content));

   procedure Remove_Predicate (Self : Ref; Pred : Var_Predicate);
   procedure Add_Predicate (Self : Ref; Pred : Var_Predicate);

   function Create return Ref;

   type Raw_Var is access all Var;
   procedure Reset (Self : in out Raw_Var);
   function Is_Defined (Self : Raw_Var) return Boolean;
   function SetL
     (Self : in out Raw_Var; Data : Element_Type) return Boolean;
   function GetL (Self : Raw_Var) return Element_Type;
   function Create return Raw_Var;

   function Get_Pending_Predicates (Self : Raw_Var) return Pred_Sets.Set
   is (Get_Pending_Predicates (Self.all));

   procedure Remove_Predicate (Self : Raw_Var; Pred : Var_Predicate);
   procedure Add_Predicate (Self : Raw_Var; Pred : Var_Predicate);

   package Refcounted_Logic_Var is new Adalog.Logic_Var
     (Ref, Element_Type);
   package Raw_Logic_Var is new Adalog.Logic_Var (Raw_Var, Element_Type);

end Adalog.Logic_Ref;
