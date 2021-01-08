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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Adalog.Logic_Var;

--  This package contains the implementation of logic variables. This is done
--  by implementing base simple types, and instantiating the Adalog.Logic_Var
--  formal package.
--
--  There are two different implementations that vary only in terms of memory
--  management:
--
--  - One is a refcounted, controlled-object based logic variable, so its
--  memory management is automatic.
--
--  - The other is a naked access, so memory management is left to the user.
--

generic
   type Element_Type is private;
   with procedure Inc_Ref (E : Element_Type);
   with procedure Dec_Ref (E : in out Element_Type);
   with function Element_Image (E : Element_Type) return String;
package Langkit_Support.Adalog.Logic_Ref is

   ------------------------------
   -- Base logic variable type --
   ------------------------------

   --  This type implements the common logic for a logic variable, and the
   --  common needed operations. See Adalog.Logic_Var for the documentation
   --  of those operations.

   --  This type, however, has by-value semantics, where we want the end
   --  implementations to have by-reference semantics.

   type Var is record
      Reset             : Boolean := True;
      --  Whether this variable is set or not. Reset is True when the variable
      --  has no value.

      Value             : Element_Type;
      --  The value of this logic variable, when it is set

      Dbg_Name          : String_Access;
      --  Access to a string representing the name of this variable. Using
      --  this, you can name your variable with human readable names, and
      --  the debugging facilities of Adalog will use it to display it in
      --  equations.
   end record;

   -------------------------------
   -- Base primitive operations --
   -------------------------------

   procedure Reset (Self : in out Var);
   function Is_Defined (Self : Var) return Boolean;
   procedure Set_Value (Self : in out Var; Data : Element_Type);
   function Get_Value (Self : Var) return Element_Type
      with Pre => Is_Defined (Self);
   procedure Destroy (Self : in out Var);
   function Image (Self : Var) return String is
     (if Self.Dbg_Name /= null then Self.Dbg_Name.all else "None");

   -----------------------
   -- Raw variable type --
   -----------------------

   --  This type is a reference to a logic variable implemented with a simple
   --  unsafe access. To use if you want maximum performance and are ready
   --  to manage your memory manually.

   type Raw_Var is access all Var;
   procedure Reset (Self : in out Raw_Var);
   function Is_Defined (Self : Raw_Var) return Boolean;
   procedure Set_Value (Self : in out Raw_Var; Data : Element_Type);
   function Get_Value (Self : Raw_Var) return Element_Type;
   function Create return Raw_Var;

   function Image (Self : Raw_Var) return String is
     (Image (Self.all));

   procedure Free is new Ada.Unchecked_Deallocation (Var, Raw_Var);

   ------------------------------------
   -- Formal packages instantiations --
   ------------------------------------

   package Raw_Logic_Var is new Adalog.Logic_Var
     (Raw_Var, Element_Type, Inc_Ref, Dec_Ref);

end Langkit_Support.Adalog.Logic_Ref;
