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

--  This package provides common implementation details for Langkit-generated
--  libraries. Even though it is not private (to allow Langkit-generated
--  libraries to use it), it is not meant to be used beyond this. As such, this
--  API is considered unsafe and unstable.

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;

package Langkit_Support.Internal.Conversions is

   --  Conversions between public and internal types.
   --
   --  These converters need visibility over the implementation details of the
   --  public types (for instance components of Lk_Node), but we do not want to
   --  expose them in the Langkit_Support.Generic_API package tree: publish
   --  here declarations as proxies to the implementations in
   --  Langkit_Support.Generic_API bodies.

   --  Converters for analysis units.  See the corresponding export declaration
   --  in Langkit_Support.Generic_API.Analysis.

   function Wrap_Unit
     (Id : Language_Id; Unit : Internal_Unit) return Lk_Unit
     with Import, External_Name => "lksp__wrap_unit";
   function Unwrap_Unit (Unit : Lk_Unit) return Internal_Unit
      with Import, External_Name => "lksp__unwrap_unit";

   --  Converters for nodes/entities.  See the corresponding export declaration
   --  in Langkit_Support.Generic_API.Analysis.

   function Wrap_Node
     (Id : Language_Id; Node : Internal_Entity) return Lk_Node
     with Import, External_Name => "lksp__wrap_node";
   function Unwrap_Node (Node : Lk_Node) return Internal_Entity
      with Import, External_Name => "lksp__unwrap_node";

end Langkit_Support.Internal.Conversions;
