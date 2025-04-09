## vim: filetype=makoada

--  This package provides support for tree-based source code rewriting.
--
--  .. ATTENTION:: This is an experimental feature, so even if it is exposed to
--  allow experiments, it is totally unsupported and the API is very likely to
--  change in the future.

with System;

with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
private with Langkit_Support.Generic_API.Rewriting;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;
with ${ada_lib_name}.Generic_API.Introspection;
use ${ada_lib_name}.Generic_API.Introspection;

package ${ada_lib_name}.Rewriting is

   use Support.Diagnostics, Support.Text;

   type Rewriting_Handle is private;
   ${ada_doc('langkit.rewriting.rewriting_handle_type', 3)}

   type Unit_Rewriting_Handle is private;
   ${ada_doc('langkit.rewriting.unit_rewriting_handle_type', 3)}

   type Node_Rewriting_Handle is private;
   ${ada_doc('langkit.rewriting.node_rewriting_handle_type', 3)}

   No_Rewriting_Handle      : constant Rewriting_Handle;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle;

   type Unit_Rewriting_Handle_Array is
      array (Positive range <>) of Unit_Rewriting_Handle;

   type Node_Rewriting_Handle_Array is
      array (Positive range <>) of Node_Rewriting_Handle;

   -----------------------
   -- Context rewriting --
   -----------------------

   function Handle (Context : Analysis_Context) return Rewriting_Handle;
   ${ada_doc('langkit.rewriting.context_handle', 3)}

   function Context (Handle : Rewriting_Handle) return Analysis_Context;
   ${ada_doc('langkit.rewriting.handle_context', 3)}

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle;
   ${ada_doc('langkit.rewriting.start_rewriting', 3)}

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle);
   ${ada_doc('langkit.rewriting.abort_rewriting', 3)}

   type Apply_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Unit : Analysis_Unit;
            --  Reference to the analysis unit that was being processed when
            --  the error occurred.

            Diagnostics : Diagnostics_Vectors.Vector;
            --  Corresponding list of error messages
         when True => null;
      end case;
   end record;

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result;
   ${ada_doc('langkit.rewriting.apply', 3)}

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   ${ada_doc('langkit.rewriting.unit_handles', 3)}

   --------------------
   -- Unit rewriting --
   --------------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.unit_handle', 3)}

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit;
   ${ada_doc('langkit.rewriting.handle_unit', 3)}

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.root', 3)}

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   ${ada_doc('langkit.rewriting.set_root', 3)}

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   ${ada_doc('langkit.rewriting.unit_unparse', 3)}

   --------------------
   -- Node rewriting --
   --------------------

   function Handle
     (Node : ${root_entity.api_name}'Class) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.node_handle', 3)}

   function Node
     (Handle : Node_Rewriting_Handle) return ${root_entity.api_name};
   ${ada_doc('langkit.rewriting.handle_node', 3)}

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   ${ada_doc('langkit.rewriting.node_context', 3)}

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   ${ada_doc('langkit.rewriting.unparse', 3)}

   function Kind (Handle : Node_Rewriting_Handle) return ${T.node_kind};
   ${ada_doc('langkit.rewriting.kind', 3)}

   function Type_Of
     (Handle : Node_Rewriting_Handle)
      return Langkit_Support.Generic_API.Introspection.Type_Ref
   is (Kind_To_Type (Kind (Handle)));
   --  Return the introspection type reference corresponding to ``Handle``'s
   --  node.

   function Image (Handle : Node_Rewriting_Handle) return String;
   ${ada_doc('langkit.rewriting.node_image', 3)}

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   ${ada_doc('langkit.rewriting.tied', 3)}

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.parent', 3)}

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   ${ada_doc('langkit.rewriting.children_count', 3)}

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.child_by_ref', 3)}

   function Child
     (Handle : Node_Rewriting_Handle;
      Fields : Struct_Member_Ref_Array) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.child_deep', 3)}

   function Children
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Array;
   ${ada_doc('langkit.rewriting.children', 3)}

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref;
      Child  : Node_Rewriting_Handle);
   ${ada_doc('langkit.rewriting.set_child_by_ref', 3)}

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   ${ada_doc('langkit.rewriting.text', 3)}

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   ${ada_doc('langkit.rewriting.set_text', 3)}

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   ${ada_doc('langkit.rewriting.replace', 3)}

   procedure Rotate (Handles : Node_Rewriting_Handle_Array);
   ${ada_doc('langkit.rewriting.rotate', 3)}

   function Is_List_Node (Handle : Node_Rewriting_Handle) return Boolean;
   ${ada_doc('langkit.rewriting.is_list_node', 3)}

   -------------------------
   -- List node rewriting --
   -------------------------

   function First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.first_child', 3)}

   function Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.last_child', 3)}

   function Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.next_child', 3)}

   function Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.previous_child', 3)}

   procedure Insert_Before
     (Handle, New_Sibling : Node_Rewriting_Handle);
   ${ada_doc('langkit.rewriting.insert_before', 3)}

   procedure Insert_After
     (Handle, New_Sibling : Node_Rewriting_Handle);
   ${ada_doc('langkit.rewriting.insert_after', 3)}

   procedure Insert_First (Handle, New_Child : Node_Rewriting_Handle);
   ${ada_doc('langkit.rewriting.insert_first', 3)}

   procedure Insert_Last (Handle, New_Child : Node_Rewriting_Handle);
   ${ada_doc('langkit.rewriting.insert_last', 3)}

   procedure Remove_Child (Handle : Node_Rewriting_Handle);
   ${ada_doc('langkit.rewriting.remove_child', 3)}

   -------------------
   -- Node creation --
   -------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.clone', 3)}

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind}) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.create_node', 3)}

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind};
      Text   : Text_Type) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.create_token_node', 3)}

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${T.node_kind};
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.create_regular_node', 3)}

   ---------------
   -- Templates --
   ---------------

   --  Templating is a way to create trees of node rewriting handles. It is
   --  intended to be more convenient than calling node constructors for each
   --  individual node in a tree.
   --
   --  A template is text that represents source code, including zero or
   --  multiple placeholders (stray "{}").
   --
   --  Create a tree of new nodes from a template is called instantiating a
   --  template: just call Create_From_Template, passing to it the template
   --  itself, a sequence of nodes (the template arguments) to fill the
   --  template placeholders and a grammar rule to parse the resulting source
   --  code. This will unparse given nodes to replace placeholders in the
   --  template text, and then parse the resulting source code in order to
   --  create a tree of node rewriting handles.
   --
   --  In order not to interfer with the template DSL, stray "{" and "}"
   --  characters in the source code must be doubled: for instance "{{"
   --  represent "{" in the source code to be parsed.

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.create_from_template', 3)}

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------

   ## Emit shortcuts for constructors of nodes that have fields

   % for n in ctx.node_types:
      % if not n.abstract and \
            not n.is_token_node and \
            not n.is_list_type and \
            n.get_parse_fields():

         function Create_${n.entity.api_name}
           (Handle : Rewriting_Handle
            % for f in n.get_parse_fields():
               ; ${f.api_name} : Node_Rewriting_Handle
            % endfor
            ) return Node_Rewriting_Handle;

      % endif
   % endfor

private

   package G renames Langkit_Support.Generic_API.Rewriting;

   type Rewriting_Handle is new G.Rewriting_Handle;
   type Unit_Rewriting_Handle is new G.Unit_Rewriting_Handle;
   type Node_Rewriting_Handle is new G.Node_Rewriting_Handle;

   No_Rewriting_Handle : constant Rewriting_Handle :=
      Rewriting_Handle (G.No_Rewriting_Handle);
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle :=
      Unit_Rewriting_Handle (G.No_Unit_Rewriting_Handle);
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle :=
      Node_Rewriting_Handle (G.No_Node_Rewriting_Handle);

end ${ada_lib_name}.Rewriting;
