--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;
with System;

with GNATCOLL.VFS;

with Langkit_Support.Bump_Ptr;     use Langkit_Support.Bump_Ptr;
with Langkit_Support.Diagnostics;  use Langkit_Support.Diagnostics;
with Langkit_Support.Hashes;       use Langkit_Support.Hashes;
with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;
with Langkit_Support.Types;        use Langkit_Support.Types;

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.
--
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

   --  Record type to have direct access to analysis context data regardless of
   --  the owning language.

   type Internal_Context_Stable_ABI is limited record
      Version : Version_Number;
      --  Serial number that is incremented each time this context allocation
      --  is released.
   end record;
   pragma No_Component_Reordering (Internal_Context_Stable_ABI);
   type Internal_Context_Stable_API_Access is
     access all Internal_Context_Stable_ABI;

   function Version (Context : Internal_Context) return Version_Number;
   --  Return the serial number of the given context

   type Internal_Node_Metadata is new System.Address;
   --  The contents and size of the node metadata record is different from one
   --  Langkit-generated library to another, so this generic API needs to refer
   --  to it by reference, with ref-counting for lifetime handling. Null
   --  addresses mean "default metadata", and the language descriptor table
   --  provides ref-counting primitives.

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

   function "=" (L, R : Internal_Entity) return Boolean is abstract;
   --  We hide the equal operator on internal entities, because since null
   --  metadata can be either null or a pointer to language specific null
   --  metadata, we generally don't want our implementation to compare the
   --  whole Internal_Entity, but rather individual fields.

   No_Internal_Entity : constant Internal_Entity :=
     (No_Internal_Node, null, False,
      Internal_Node_Metadata (System.Null_Address));

   type Internal_Entity_Array is array (Positive range <>) of Internal_Entity;

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
      --  produced.
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

   type Node_Or_Token (Is_Node : Boolean := False) is record
      case Is_Node is
         when False => Token : Internal_Token;
         when True  => Node  : Internal_Node;
      end case;
   end record;

   type Node_Or_Token_Array is array (Positive range <>) of Node_Or_Token;
   type Node_Or_Token_Array_Access is access all Node_Or_Token_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Node_Or_Token_Array, Node_Or_Token_Array_Access);

   --  Contexts, units and token data handlers are implementing with big
   --  records, at least 256 bytes long, so we can ignore the 8 least
   --  significant bits of their addresses.  Nodes can be much smaller, but
   --  they are still at least 32 bytes long, so ignore the 5 least significant
   --  bits of their addresses.

   function Hash_Context is new Hash_Address (8);
   function Hash_Unit is new Hash_Address (8);
   function Hash_Node is new Hash_Address (5);
   function Hash_TDH is new Hash_Address (8);

   function Hash (Self : Internal_Context) return Hash_Type
   is (Hash_Context (System.Address (Self)));

   function Hash (Self : Internal_Unit) return Hash_Type
   is (Hash_Unit (System.Address (Self)));

   function Hash (Self : Internal_Node) return Hash_Type
   is (Hash_Node (System.Address (Self)));

   function Hash (Self : Token_Data_Handler_Access) return Hash_Type
   is (Hash_TDH (if Self = null
                 then System.Null_Address
                 else Self.all'Address));

   type Diagnostics_Access is access constant Diagnostics_Vectors.Vector;
   --  Reference to an analysis unit's diagnostics array

   -------------------------------------
   -- GNATCOLL.VFS.Virtual_File cache --
   -------------------------------------

   --  Cache for ``GNATCOLL.VFS.Virtual_File`` we create for String filenames.
   --  Re-using older ``Virtual_File`` values is useful as this reduces the
   --  need to normalize paths, which is a costly operation.

   package Virtual_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => GNATCOLL.VFS.Virtual_File,
      Equivalent_Keys => "=",
      "="             => GNATCOLL.VFS."=",
      Hash            => Ada.Strings.Unbounded.Hash);

   subtype Virtual_File_Cache is Virtual_File_Maps.Map;

   Empty_Virtual_File_Cache : Virtual_File_Maps.Map
     renames Virtual_File_Maps.Empty_Map;

   function Normalized_Unit_Filename
     (Cache : in out Virtual_File_Cache; Filename : String)
      return GNATCOLL.VFS.Virtual_File;
   --  Try to return a canonical filename. This is used to have an
   --  as-unique-as-possible analysis unit identifier.

   -------------------------
   -- (Re)parsing helpers --
   -------------------------

   type Lexer_Input (Kind : Lexer_Input_Kind) is record
      case Kind is
         when File | Bytes_Buffer =>
            Charset : Unbounded_String;
            --  Name of the charset to use in order to decode the input source

            Read_BOM : Boolean;
            --  Whether the lexer should look for an optional Byte Order Mark

            case Kind is
               when File =>
                  Filename : GNATCOLL.VFS.Virtual_File;
                  --  Name of the file to read

               when Bytes_Buffer =>
                  Bytes       : System.Address;
                  Bytes_Count : Natural;
                  --  Source buffer to read (start address and buffer length)

               when others =>
                  null;
            end case;

         when Text_Buffer =>
            Text       : System.Address;
            Text_Count : Natural;
            --  Source buffer to read (start address and buffer length)
      end case;
   end record;
   --  Input from which a lexer can read tokens

   type Reparsed_Unit (Present : Boolean := False) is record
      case Present is
         when False =>
            null;
         when True =>
            TDH          : Token_Data_Handler;
            Diagnostics  : Diagnostics_Vectors.Vector;
            Ast_Mem_Pool : Bump_Ptr_Pool;
            Ast_Root     : Internal_Node;
      end case;
   end record;
   --  Holder for analysis unit attributes affected by reparsing. Having a
   --  dedicated data structure to store this allows to decouple the reparsing
   --  step from the actual analysis unit update, which in turns allow to
   --  implement behaviors like canceling the reparsing on parsing error.

   procedure Destroy (Reparsed : in out Reparsed_Unit);
   --  Free all resources in Reparsed

end Langkit_Support.Internal.Analysis;
