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

with Adalog.Logic_Var;
with GNATCOLL.Refcount;

generic
   type Element_Type is private;
package Adalog.Logic_Ref is

   type Var is record
      Reset : Boolean := True;
      El    : Element_Type;
   end record;

   procedure Reset (Self : in out Var);
   function Is_Defined (Self : Var) return Boolean;
   procedure SetL (Self : in out Var; Data : Element_Type);
   function GetL (Self : Var) return Element_Type;

   type Refcounted_El is new GNATCOLL.Refcount.Refcounted with record
      Content : Var;
   end record;

   package Refs is new GNATCOLL.Refcount.Shared_Pointers (Refcounted_El);
   type Ref is new Refs.Ref with null record;

   procedure Reset (Self : in out Ref);
   function Is_Defined (Self : Ref) return Boolean;
   procedure SetL (Self : in out Ref; Data : Element_Type);
   function GetL (Self : Ref) return Element_Type;
   function Create (El : Element_Type) return Ref;
   function Create return Ref;

   type Raw_Var is access all Var;
   procedure Reset (Self : in out Raw_Var);
   function Is_Defined (Self : Raw_Var) return Boolean;
   procedure SetL (Self : in out Raw_Var; Data : Element_Type);
   function GetL (Self : Raw_Var) return Element_Type;
   function Create return Raw_Var;

   package Refcounted_Logic_Var is new Adalog.Logic_Var (Ref, Element_Type);
   package Raw_Logic_Var is new Adalog.Logic_Var (Raw_Var, Element_Type);

end Adalog.Logic_Ref;
