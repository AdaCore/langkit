## vim: filetype=makoada

--  This package provides support for tree-based source code rewriting.
--
--  .. ATTENTION:: This is an experimental feature, so even if it is exposed to
--  allow experiments, it is totally unsupported and the API is very likely to
--  change in the future.

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;

with System;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;
private with ${ada_lib_name}.Rewriting_Implementation;

package ${ada_lib_name}.Rewriting is

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

   function Context (Handle : Rewriting_Handle) return Analysis_Context
      with Pre => Handle /= No_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.handle_context', 3)}

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle
     with Pre  => Handle (Context) = No_Rewriting_Handle,
          Post => Handle (Context) /= No_Rewriting_Handle
                  and then Has_With_Trivia (Context)
                  and then Start_Rewriting'Result = Handle (Context)
                  and then ${ada_lib_name}.Rewriting.Context
                             (Start_Rewriting'Result) = Context;
   ${ada_doc('langkit.rewriting.start_rewriting', 3)}

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle)
      with Pre  => Handle /= No_Rewriting_Handle,
           Post => Handle = No_Rewriting_Handle;
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

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result
      with Pre  => Handle /= No_Rewriting_Handle,
           Post => (if Apply'Result.Success
                    then Handle = No_Rewriting_Handle
                    else Handle = Handle'Old);
   ${ada_doc('langkit.rewriting.apply', 3)}

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array
      with Pre => Handle /= No_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.unit_handles', 3)}

   --------------------
   -- Unit rewriting --
   --------------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle
      with Pre => Handle (Context (Unit)) /= No_Rewriting_Handle
                  and then not Unit.Has_Diagnostics;
   ${ada_doc('langkit.rewriting.unit_handle', 3)}

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit
      with Pre => Handle /= No_Unit_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.handle_unit', 3)}

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
      with Pre => Handle /= No_Unit_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.root', 3)}

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle)
      with Pre => Handle /= No_Unit_Rewriting_Handle
                  and then (Root = No_Node_Rewriting_Handle
                            or else not Tied (Root));
   ${ada_doc('langkit.rewriting.set_root', 3)}

   --------------------
   -- Node rewriting --
   --------------------

   function Handle
     (Node : ${root_entity.api_name}'Class) return Node_Rewriting_Handle
      with Pre => Handle (Context (Unit (Node))) /= No_Rewriting_Handle
                  and then not Node.Unit.Has_Diagnostics;
   ${ada_doc('langkit.rewriting.node_handle', 3)}

   function Node
     (Handle : Node_Rewriting_Handle) return ${root_entity.api_name}
      with Pre => Handle /= No_Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.handle_node', 3)}

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle
      with Pre => Handle /= No_Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.node_context', 3)}

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type
      with Pre => Handle /= No_Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.unparse', 3)}

   function Kind (Handle : Node_Rewriting_Handle) return ${root_node_kind_name}
      with Pre => Handle /= No_Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.kind', 3)}

   function Tied (Handle : Node_Rewriting_Handle) return Boolean
      with Pre => Handle /= No_Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.tied', 3)}

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
      with Pre => Handle /= No_Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.parent', 3)}

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural
      with Pre => Handle /= No_Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.children_count', 3)}

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle
      with Pre => Handle /= No_Node_Rewriting_Handle
                  and then Index in 1 .. Children_Count (Handle);
   ${ada_doc('langkit.rewriting.child', 3)}

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
      with Pre =>
         Handle /= No_Node_Rewriting_Handle
         and then Index in 1 .. Children_Count (Handle)
         and then (Child = No_Node_Rewriting_Handle or else not Tied (Child));
   ${ada_doc('langkit.rewriting.set_child', 3)}

   function Text (Handle : Node_Rewriting_Handle) return Text_Type
      with Pre => Handle /= No_Node_Rewriting_Handle
                  and then Is_Token_Node (Kind (Handle));
   ${ada_doc('langkit.rewriting.text', 3)}

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type)
      with Pre => Handle /= No_Node_Rewriting_Handle
                  and then Is_Token_Node (Kind (Handle));
   ${ada_doc('langkit.rewriting.set_text', 3)}

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle)
      with Pre =>
         Handle /= No_Node_Rewriting_Handle
         and then Tied (Handle)
         and then (New_Node = No_Node_Rewriting_Handle
                   or else not Tied (New_Node));
   ${ada_doc('langkit.rewriting.replace', 3)}

   -------------------------
   -- List node rewriting --
   -------------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
      with Pre  => Handle /= No_Node_Rewriting_Handle
                   and then Is_List_Node (Kind (Handle))
                   and then Index <= Children_Count (Handle) + 1
                   and then (Child = No_Node_Rewriting_Handle
                             or else not Tied (Child)),
           Post => Rewriting.Child (Handle, Index) = Child;
   ${ada_doc('langkit.rewriting.insert_child', 3)}

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle)
      with Pre  => Handle /= No_Node_Rewriting_Handle
                   and then Is_List_Node (Kind (Handle))
                   and then (Child = No_Node_Rewriting_Handle
                             or else not Tied (Child)),
           Post => Rewriting.Child (Handle, Children_Count (Handle)) = Child;
   ${ada_doc('langkit.rewriting.append_child', 3)}

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive)
      with Pre  => Handle /= No_Node_Rewriting_Handle
                   and then Is_List_Node (Kind (Handle))
                   and then Index in 1 .. Children_Count (Handle);
   ${ada_doc('langkit.rewriting.remove_child', 3)}

   -------------------
   -- Node creation --
   -------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.clone', 3)}

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : ${root_node_kind_name}) return Node_Rewriting_Handle
      with Pre => Handle /= No_Rewriting_Handle;
   ${ada_doc('langkit.rewriting.create_node', 3)}

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : ${root_node_kind_name};
      Text   : Text_Type) return Node_Rewriting_Handle
      with Pre => Handle /= No_Rewriting_Handle
                  and then Is_Token_Node (Kind);
   ${ada_doc('langkit.rewriting.create_token_node', 3)}

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${root_node_kind_name};
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle
      with Pre => Handle /= No_Rewriting_Handle
                  and then not Is_Token_Node (Kind)
                  and then (for all C of Children =>
                            C = No_Node_Rewriting_Handle or else not Tied (C));
   ${ada_doc('langkit.rewriting.create_regular_node', 3)}

   ---------------
   -- Templates --
   ---------------

   --  Templating is a way to create trees of node rewriting handles. It is
   --  intended to be more convenient than calling node constructors for each
   --  individual node in a tree.
   --
   --  A template is text that represents source code, including zero or
   --  multiple holes (stray "{}").
   --
   --  Create a tree of new nodes from a template is called instantiating a
   --  template: just call Create_From_Template, passing to it the template
   --  itself, a sequence of nodes (the template arguments) to fill the
   --  template holes and a grammar rule to parse the resulting source code.
   --  This will unparse given nodes to replace holes in the template text, and
   --  then parse the resulting source code in order to create a tree of node
   --  rewriting handles.
   --
   --  In order not to interfer with the template DSL, stray "{" and "}"
   --  characters in the source code must be doubled: for instance "{{"
   --  represent "{" in the source code to be parsed.

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle
      with Pre => (for all A of Arguments =>
                   A = No_Node_Rewriting_Handle
                   or else Rewriting.Context (A) = Handle);
   ${ada_doc('langkit.rewriting.create_from_template', 3)}

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------

   ## Emit shortcuts for constructors of nodes that have fields

   % for n in ctx.astnode_types:
      % if not n.abstract and \
            not n.is_token_node and \
            not n.is_list_type and \
            n.get_parse_fields():

         function Create_${n.entity.api_name}
           (Handle : Rewriting_Handle
            % for f in n.get_parse_fields():
               ; ${f.name} : Node_Rewriting_Handle
            % endfor
            ) return Node_Rewriting_Handle
            with Pre => Handle /= No_Rewriting_Handle;

      % endif
   % endfor

private
   package Impl renames ${ada_lib_name}.Rewriting_Implementation;

   --  Workaround S114-026 by not deriving from Impl.Rewriting_Handle directly.
   --  TODO: Cleanup once S114-026 is fixed.
   type Rewriting_Handle is new System.Address;
   type Unit_Rewriting_Handle is new System.Address;
   type Node_Rewriting_Handle is new System.Address;

   No_Rewriting_Handle : constant Rewriting_Handle :=
      Rewriting_Handle (System.Null_Address);
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle :=
      Unit_Rewriting_Handle (System.Null_Address);
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle :=
      Node_Rewriting_Handle (System.Null_Address);

end ${ada_lib_name}.Rewriting;
