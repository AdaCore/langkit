--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
   --
   --  Converters for analysis contexts.  See the corresponding export
   --  declaration in Langkit_Support.Generic_API.Analysis.

   function Wrap_Context
     (Id : Language_Id; Context : Internal_Context) return Lk_Context
     with Import, External_Name => External_Name_Prefix & "wrap_context";
   function Unwrap_Context (Context : Lk_Context) return Internal_Context
      with Import, External_Name => External_Name_Prefix & "unwrap_context";

   --  Converters for analysis units.  See the corresponding export declaration
   --  in Langkit_Support.Generic_API.Analysis.

   function Wrap_Unit
     (Id : Language_Id; Unit : Internal_Unit) return Lk_Unit
     with Import, External_Name => External_Name_Prefix & "wrap_unit";
   function Unwrap_Unit (Unit : Lk_Unit) return Internal_Unit
      with Import, External_Name => External_Name_Prefix & "unwrap_unit";

   --  Converters for nodes/entities.  See the corresponding export declaration
   --  in Langkit_Support.Generic_API.Analysis.

   function Wrap_Node
     (Id : Language_Id; Node : Internal_Entity) return Lk_Node
     with Import, External_Name => External_Name_Prefix & "wrap_node";
   function Unwrap_Node (Node : Lk_Node) return Internal_Entity
      with Import, External_Name => External_Name_Prefix & "unwrap_node";

   --  Converters for tokens.  See the corresponding export declaration in
   --  Langkit_Support.Generic_API.Analysis.

   function Wrap_Token
     (Id         : Any_Language_Id;
      Token      : Internal_Token;
      Safety_Net : Token_Safety_Net) return Lk_Token
     with Import, External_Name => External_Name_Prefix & "wrap_token";
   procedure Unwrap_Token
     (Token      : Lk_Token;
      Id         : out Any_Language_Id;
      Data       : out Internal_Token;
      Safety_Net : out Token_Safety_Net)
      with Import, External_Name => External_Name_Prefix & "unwrap_token";

end Langkit_Support.Internal.Conversions;
