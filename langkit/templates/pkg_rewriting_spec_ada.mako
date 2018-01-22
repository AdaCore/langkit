## vim: filetype=makoada

private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Hash;

private with System;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;

package ${ada_lib_name}.Rewriting is

   type Rewriting_Handle is private;
   --  Handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is private;
   --  Handle for the process of rewriting an analysis unit. Such handles are
   --  owned by a Rewriting_Handle instance.

   type Node_Rewriting_Handle is private;
   --  Handle for the process of rewriting an AST node. Such handles are owned
   --  by a Rewriting_Handle instance.

   No_Rewriting_Handle      : constant Rewriting_Handle;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle;

   function Handle (Context : Analysis_Context) return Rewriting_Handle;
   --  Return the rewriting handle associated to Context, or
   --  No_Rewriting_Handle if Context is not being rewritten.

   function Context (Handle : Rewriting_Handle) return Analysis_Context
      with Pre => Handle /= No_Rewriting_Handle;
   --  Return the analysis context associated to Handle

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle
     with Pre  => Handle (Context) = No_Rewriting_Handle,
          Post => Handle (Context) /= No_Rewriting_Handle
                  and then Start_Rewriting'Result = Handle (Context)
                  and then ${ada_lib_name}.Rewriting.Context
                             (Start_Rewriting'Result) = Context;
   --  Start a rewriting session for Context.
   --
   --  This handle will keep track of all changes to do on Context's analysis
   --  units. Once the set of changes is complete, call the Apply procedure to
   --  actually update Context. This makes it possible to inspect the "old"
   --  Context state while creating the list of changes.
   --
   --  There can be only one rewriting session per analysis context, so this
   --  will raise an Existing_Rewriting_Handle_Error exception if Context
   --  already has a living rewriting session.

   procedure Apply (Handle : in out Rewriting_Handle)
      with Pre  => Handle /= No_Rewriting_Handle,
           Post => Handle = No_Rewriting_Handle;
   --  Apply all modifications to Handle's analysis context and close Handle

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle
      with Pre => Handle (Context (Unit)) /= No_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Unit

   function Handle
     (Node : ${root_entity.api_name}'Class) return Node_Rewriting_Handle
      with Pre => Handle (Context (Get_Unit (Node))) /= No_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Node

private
   use Ada.Strings.Unbounded;

   type Rewriting_Handle_Type;
   type Unit_Rewriting_Handle_Type;
   type Node_Rewriting_Handle_Type;
   type Rewriting_Handle is access Rewriting_Handle_Type;
   type Unit_Rewriting_Handle is access Unit_Rewriting_Handle_Type;
   type Node_Rewriting_Handle is access Node_Rewriting_Handle_Type;

   No_Rewriting_Handle : constant Rewriting_Handle := null;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle := null;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle := null;

   type AST_Node_Pointer is new System.Address;
   function Hash (Node : AST_Node_Pointer) return Ada.Containers.Hash_Type;
   --  Wrappers for bare AST nodes, to avoid circular dependencies

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Unit_Rewriting_Handle,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => AST_Node_Pointer,
      Element_Type    => Node_Rewriting_Handle,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Rewriting_Handle_Type is record
      Context : Analysis_Context;
      Units   : Unit_Maps.Map;
   end record;

   type Unit_Rewriting_Handle_Type is record
      Context_Handle : Rewriting_Handle;
      Unit           : Analysis_Unit;
      Nodes          : Node_Maps.Map;
   end record;

   type Node_Rewriting_Handle_Type is record
      Context_Handle : Rewriting_Handle;
      Node           : AST_Node_Pointer;
   end record;

end ${ada_lib_name}.Rewriting;
