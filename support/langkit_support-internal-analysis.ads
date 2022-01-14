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

with System;

with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;
with Langkit_Support.Types;        use Langkit_Support.Types;

--  This package provides common implementation details for Langkit-generated
--  libraries. Even though it is not private (to allow Langkit-generated
--  libraries to use it), it is not meant to be used beyond this. As such, this
--  API is considered unsafe and unstable.

package Langkit_Support.Internal.Analysis is

   --  Bare pointers to library-specific resources. For contexts, units and
   --  nodes, these correspond to the access types defined in $.Implementation.

   type Internal_Context is new System.Address;
   type Internal_Unit is new System.Address;
   type Internal_Node is new System.Address;

   No_Internal_Context : constant Internal_Context :=
     Internal_Context (System.Null_Address);
   No_Internal_Unit    : constant Internal_Unit :=
     Internal_Unit (System.Null_Address);
   No_Internal_Node    : constant Internal_Node :=
     Internal_Node (System.Null_Address);

   type Internal_Node_Metadata is new System.Address;
   --  The contents and size of the node metadata record is different from one
   --  Langkit-generated library to another, so this generic API needs to refer
   --  to it by reference, with ref-counting for lifetime handling. Null
   --  addresses mean "default metadata", and the language descriptor table
   --  provides ref-counting primitives.

   No_Internal_Node_Metadata : constant Internal_Node_Metadata :=
     Internal_Node_Metadata (System.Null_Address);

   --  As everywhere else, entities are made up of bare nodes and entity
   --  information, with regular types from Langkit_Support.Lexical_Envs. The
   --  metadata has a special representation: see above (Internal_Node_Metadata
   --  type).

   type Internal_Entity is record
      Node         : Internal_Node;
      Rebindings   : Env_Rebindings;
      From_Rebound : Boolean;
      Metadata     : Internal_Node_Metadata;
   end record;

   No_Internal_Entity : constant Internal_Entity :=
     (No_Internal_Node, null, False, No_Internal_Node_Metadata);

   type Internal_Token is record
      TDH   : Token_Data_Handler_Access;
      Index : Token_Or_Trivia_Index;
   end record;

   --  Safety nets keep track of information at "public reference value"
   --  creation so that later use can check whether the reference is still
   --  valid (used to ensure memory safety).

   type Node_Safety_Net is record
      Context         : Internal_Context;
      Context_Version : Version_Number;
      --  Analysis context and version number at the time this safety net was
      --  produced.

      Unit         : Internal_Unit;
      Unit_Version : Version_Number;
      --  Analysis unit and unit version at the time this safety net was
      --  produced.

      Rebindings_Version : Version_Number;
      --  Version of the associated rebinding at the time this safety net was
      --  procuded.
   end record;
   No_Node_Safety_Net : constant Node_Safety_Net :=
     (No_Internal_Context, 0, No_Internal_Unit, 0, 0);

   function Create_Node_Safety_Net
     (Id         : Language_Id;
      Context    : Internal_Context;
      Unit       : Internal_Unit;
      Rebindings : Env_Rebindings) return Node_Safety_Net;
   --  Return the safety net for a node given its owning context and unit, and
   --  its rebindings.

   type Token_Safety_Net is record
      Context         : Internal_Context;
      Context_Version : Version_Number;
      --  Analysis context and version number at the time this safety net was
      --  produced.

      TDH_Version : Version_Number;
      --  Version of the token data handler at the time this safety net was
      --  produced.
   end record;

   No_Token_Safety_Net : constant Token_Safety_Net :=
     (No_Internal_Context, 0, 0);

end Langkit_Support.Internal.Analysis;
