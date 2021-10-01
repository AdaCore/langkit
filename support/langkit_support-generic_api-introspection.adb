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

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Internal.Introspection;
use Langkit_Support.Internal.Introspection;

--  Even though we don't directly use entities from the Internal.Descriptor
--  package, we still need to import it to get visibility over the
--  Language_Descriptor type (and access its components).

pragma Unreferenced (Langkit_Support.Internal.Descriptor);

package body Langkit_Support.Generic_API.Introspection is

   use Langkit_Support.Errors.Introspection;

   procedure Check_Value_Type (Id : Language_Id; T : Value_Type);
   --  If ``T`` is not a valid value type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   procedure Check_Node_Type (Id : Language_Id; Node : Value_Type);
   --  If ``Node`` is not a valid node type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   ---------------------
   -- Last_Value_Type --
   ---------------------

   function Last_Value_Type (Id : Language_Id) return Value_Type is
   begin
      return Id.Value_Types.all'Last;
   end Last_Value_Type;

   ----------------------
   -- Check_Value_Type --
   ----------------------

   procedure Check_Value_Type (Id : Language_Id; T : Value_Type) is
   begin
      if T > Last_Value_Type (Id) then
         raise Precondition_Failure with "invalid value type";
      end if;
   end Check_Value_Type;

   ----------------
   -- Debug_Name --
   ----------------

   function Debug_Name (Id : Language_Id; T : Value_Type) return String is
   begin
      Check_Value_Type (Id, T);
      return Id.Value_Types (T).Debug_Name.all;
   end Debug_Name;

   ------------------
   -- Is_Node_Type --
   ------------------

   function Is_Node_Type (Id : Language_Id; T : Value_Type) return Boolean is
   begin
      Check_Value_Type (Id, T);
      return T in Id.Node_Types.all'Range;
   end Is_Node_Type;

   ---------------------
   -- Check_Node_Type --
   ---------------------

   procedure Check_Node_Type (Id : Language_Id; Node : Value_Type) is
   begin
      if not Is_Node_Type (Id, Node) then
         raise Precondition_Failure with "invalid node type";
      end if;
   end Check_Node_Type;

   --------------------
   -- Root_Node_Type --
   --------------------

   function Root_Node_Type (Id : Language_Id) return Value_Type is
   begin
      return Id.Node_Types.all'First;
   end Root_Node_Type;

   --------------------
   -- Node_Type_Name --
   --------------------

   function Node_Type_Name
     (Id : Language_Id; Node : Value_Type) return Name_Type is
   begin
      Check_Node_Type (Id, Node);
      return Create_Name (Id.Node_Types.all (Node).Name.all);
   end Node_Type_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract
     (Id : Language_Id; Node : Value_Type) return Boolean is
   begin
      Check_Node_Type (Id, Node);
      return Id.Node_Types.all (Node).Is_Abstract;
   end Is_Abstract;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type
     (Id : Language_Id; Node : Value_Type) return Value_Type is
   begin
      Check_Node_Type (Id, Node);
      if Node = Root_Node_Type (Id) then
         raise Bad_Type_Error with "trying to get base type of root node";
      end if;
      return Id.Node_Types.all (Node).Base_Type;
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types
     (Id : Language_Id; Node : Value_Type) return Value_Type_Array is
   begin
      Check_Node_Type (Id, Node);
      return Id.Node_Types.all (Node).Derivations;
   end Derived_Types;

   -----------------------
   -- Last_Derived_Type --
   -----------------------

   function Last_Derived_Type
     (Id : Language_Id; Node : Value_Type) return Value_Type
   is
      --  Look for the last derivations's derivation, recursively

      Result : Value_Type := Node;
   begin
      Check_Node_Type (Id, Node);

      loop
         declare
            Desc : Node_Type_Descriptor renames Id.Node_Types.all (Result).all;
         begin
            exit when Desc.Derivations'Length = 0;
            Result := Desc.Derivations (Desc.Derivations'Last);
         end;
      end loop;
      return Result;
   end Last_Derived_Type;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From
     (Id : Language_Id; Node, Parent : Value_Type) return Boolean
   is
      Node_Types : Node_Type_Descriptor_Array renames Id.Node_Types.all;
      Cursor     : Any_Value_Type := Node;
   begin
      Check_Node_Type (Id, Node);
      Check_Node_Type (Id, Parent);

      while Cursor /= No_Value_Type loop
         if Cursor = Parent then
            return True;
         end if;

         Cursor := Node_Types (Cursor).Base_Type;
      end loop;
      return False;
   end Is_Derived_From;

end Langkit_Support.Generic_API.Introspection;
