--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.
--
--  This package provides common implementation details for Langkit-generated
--  libraries. Even though it is not private (to allow Langkit-generated
--  libraries to use it), it is not meant to be used beyond this. As such, this
--  API is considered unsafe and unstable.

with System;

with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Generic_API.Rewriting;
use Liblktlang_Support.Generic_API.Rewriting;
with Liblktlang_Support.Internal.Analysis; use Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Internal.Descriptor;
use Liblktlang_Support.Internal.Descriptor;

package Liblktlang_Support.Internal.Conversions is

   --  Conversions between public and internal types.
   --
   --  These converters need visibility over the implementation details of the
   --  public types (for instance components of Lk_Node), but we do not want to
   --  expose them in the Liblktlang_Support.Generic_API package tree: publish
   --  here declarations as proxies to the implementations in
   --  Liblktlang_Support.Generic_API bodies.
   --
   --  Converters for analysis contexts.  See the corresponding export
   --  declaration in Liblktlang_Support.Generic_API.Analysis.

   function Wrap_Context
     (Id : Language_Id; Context : Internal_Context) return Lk_Context
     with Import, External_Name => External_Name_Prefix & "wrap_context";
   function Unwrap_Context (Context : Lk_Context) return Internal_Context
      with Import, External_Name => External_Name_Prefix & "unwrap_context";

   --  Converters for analysis units.  See the corresponding export declaration
   --  in Liblktlang_Support.Generic_API.Analysis.

   function Wrap_Unit
     (Id : Language_Id; Unit : Internal_Unit) return Lk_Unit
     with Import, External_Name => External_Name_Prefix & "wrap_unit";
   function Unwrap_Unit (Unit : Lk_Unit) return Internal_Unit
      with Import, External_Name => External_Name_Prefix & "unwrap_unit";

   --  Converters for nodes/entities.  See the corresponding export declaration
   --  in Liblktlang_Support.Generic_API.Analysis.

   function Wrap_Node
     (Id : Language_Id; Node : Internal_Entity) return Lk_Node
     with Import, External_Name => External_Name_Prefix & "wrap_node";
   function Unwrap_Node (Node : Lk_Node) return Internal_Entity
      with Import, External_Name => External_Name_Prefix & "unwrap_node";

   function Wrap_Node
     (Id : Language_Id; Node : Internal_Node) return Lk_Node
   is (Wrap_Node (Id, (Node, null, False, "+" (Id).Null_Metadata)));

   --  Converters for tokens.  See the corresponding export declaration in
   --  Liblktlang_Support.Generic_API.Analysis.

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

   --  Converters for rewriting handles. See the corresponding export
   --  declaration in Liblktlang_Support.Generic_API.Rewriting.

   type C_Rewriting_Handle is new System.Address;
   type C_Unit_Rewriting_Handle is new System.Address;
   type C_Node_Rewriting_Handle is new System.Address;

   No_C_Rewriting_Handle : constant C_Rewriting_Handle :=
     C_Rewriting_Handle (System.Null_Address);
   No_C_Unit_Rewriting_Handle : constant C_Unit_Rewriting_Handle :=
     C_Unit_Rewriting_Handle (System.Null_Address);
   No_C_Node_Rewriting_Handle : constant C_Node_Rewriting_Handle :=
     C_Node_Rewriting_Handle (System.Null_Address);

   function From_C_Rewriting_Handle
     (Handle : C_Rewriting_Handle) return Rewriting_Handle
   with
     Import, External_Name => External_Name_Prefix & "wrap_rewriting_handle";
   function To_C_Rewriting_Handle
     (Handle : Rewriting_Handle) return C_Rewriting_Handle
   with
     Import, External_Name => External_Name_Prefix & "unwrap_rewriting_handle";

   function From_C_Unit_Rewriting_Handle
     (Handle : C_Unit_Rewriting_Handle) return Unit_Rewriting_Handle
   with
     Import,
     External_Name => External_Name_Prefix & "wrap_unit_rewriting_handle";
   function To_C_Unit_Rewriting_Handle
     (Handle : Unit_Rewriting_Handle) return C_Unit_Rewriting_Handle
   with
     Import,
     External_Name => External_Name_Prefix & "unwrap_unit_rewriting_handle";

   function From_C_Node_Rewriting_Handle
     (Handle : C_Node_Rewriting_Handle) return Node_Rewriting_Handle
   with
     Import,
     External_Name => External_Name_Prefix & "wrap_node_rewriting_handle";
   function To_C_Node_Rewriting_Handle
     (Handle : Node_Rewriting_Handle) return C_Node_Rewriting_Handle
   with
     Import,
     External_Name => External_Name_Prefix & "unwrap_node_rewriting_handle";

end Liblktlang_Support.Internal.Conversions;
