## vim: filetype=makoada

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;

with Langkit_Support.File_Readers;      use Langkit_Support.File_Readers;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal;          use Langkit_Support.Internal;
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Text;              use Langkit_Support.Text;
with Langkit_Support.Types;             use Langkit_Support.Types;

with ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Generic_Introspection;
use ${ada_lib_name}.Generic_Introspection;
with ${ada_lib_name}.Private_Converters; use ${ada_lib_name}.Private_Converters;
with ${ada_lib_name}.Public_Converters;  use ${ada_lib_name}.Public_Converters;

package body ${ada_lib_name}.Generic_API is

   pragma Warnings (Off, "possible aliasing problem for type");
   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, Implementation.Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Implementation.Internal_Context, Internal_Context);
   pragma Warnings (On, "possible aliasing problem for type");

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Unit, Implementation.Internal_Unit);
   function "+" is new Ada.Unchecked_Conversion
     (Implementation.Internal_Unit, Internal_Unit);

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Node, Implementation.${T.root_node.name});
   function "+" is new Ada.Unchecked_Conversion
     (Implementation.${T.root_node.name}, Internal_Node);

   function "+"
     (Entity : Internal_Entity) return Implementation.${root_entity.name};

   function "+" (Rule : Grammar_Rule_Index) return Common.Grammar_Rule
   is (Common.Grammar_Rule'Val (Rule - 1));
   --  Grammar rules start at 1 in the generic API: rebase the value before
   --  converting it to the native type.

   function "+" (Token : Common.Token_Reference) return Internal_Token
   is ((Get_Token_TDH (Token), Get_Token_Index (Token)));

   --  Descriptors for grammar rules

   <%
      rule_name_refs = []
      main_rule_id = None
   %>
   % for i, n in enumerate(ctx.grammar.user_defined_rules, 1):
      <%
         name = f"Rule_Name_{i}"
         rule_name_refs.append(f"{i} => {name}'Access")
         if n == ctx.grammar.main_rule_name:
            main_rule_id = i
      %>
      ${name} : aliased constant Text_Type :=
        ${text_repr(names.Name.from_lower(n).camel_with_underscores)};
   % endfor
   <% assert main_rule_id is not None %>
   Grammar_Rule_Names : aliased constant Grammar_Rule_Name_Array :=
     (${", ".join(rule_name_refs)});

   --  Descriptors for toen kinds

   <% kind_refs = [] %>
   % for i, token in enumerate(ctx.lexer.sorted_tokens, 1):
      <%
         name = f"Token_Kind_Name_{i}"
         kind_refs.append(f"{i} => {name}'Access")
      %>
      ${name} : aliased constant Text_Type :=
        ${text_repr(token.base_name.camel_with_underscores)};
   % endfor
   Token_Kind_Names : aliased constant Token_Kind_Name_Array :=
     (${", ".join(kind_refs)});

   --  Implementations for generic operations on analysis types

   function Create_Context
     (Charset     : String;
      File_Reader : File_Reader_Reference;
      With_Trivia : Boolean;
      Tab_Stop    : Natural) return Internal_Context;

   procedure Context_Inc_Ref (Context : Internal_Context);
   procedure Context_Dec_Ref (Context : in out Internal_Context);
   function Context_Version (Context : Internal_Context) return Version_Number;
   function Context_Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean;
   function Context_Get_From_File
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Rule              : Grammar_Rule_Index) return Internal_Unit;

   function Unit_Version (Unit : Internal_Unit) return Version_Number;
   function Unit_Filename (Unit : Internal_Unit) return String;
   function Unit_Root (Unit : Internal_Unit) return Internal_Node;
   function Unit_First_Token (Unit : Internal_Unit) return Internal_Token;
   function Unit_Last_Token (Unit : Internal_Unit) return Internal_Token;
   function Unit_Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;

   type Internal_Node_Metadata_Type is record
      Ref_Count : Natural;
      Internal  : Implementation.${T.env_md.name};
   end record;
   type Internal_Node_Metadata_Access is
      access all Internal_Node_Metadata_Type;

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Node_Metadata, Internal_Node_Metadata_Access);

   procedure Node_Metadata_Inc_Ref (Metadata : Internal_Node_Metadata);
   procedure Node_Metadata_Dec_Ref (Metadata : in out Internal_Node_Metadata);

   function Node_Kind (Node : Internal_Node) return Type_Index;
   function Node_Parent (Node : Internal_Node) return Internal_Node;
   function Node_Children_Count (Node : Internal_Node) return Natural;
   procedure Node_Get_Child
     (Node            : Internal_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Internal_Node);
   function Node_Fetch_Sibling
     (Node : Internal_Node; Offset : Integer) return Internal_Node;
   function Node_Token_Start (Node : Internal_Node) return Internal_Token;
   function Node_Token_End (Node : Internal_Node) return Internal_Token;

   function Entity_Image (Entity : Internal_Entity) return String;

   ---------
   -- "+" --
   ---------

   function "+"
     (Entity : Internal_Entity) return Implementation.${root_entity.name}
   is
      MD : constant Internal_Node_Metadata_Access := +Entity.Metadata;
   begin
      return (Node => +Entity.Node,
              Info => (MD           => (if MD = null
                                        then Implementation.No_Metadata
                                        else MD.Internal),
                       Rebindings   => Entity.Rebindings,
                       From_Rebound => Entity.From_Rebound));
   end "+";

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset     : String;
      File_Reader : File_Reader_Reference;
      With_Trivia : Boolean;
      Tab_Stop    : Natural) return Internal_Context
   is
      FR : Implementation.Internal_File_Reader_Access :=
         Wrap_Public_File_Reader (File_Reader);

      Actual_Tab_Stop : constant Positive :=
        (if Tab_Stop = 0
         then ${ctx.default_tab_stop}
         else Tab_Stop);

      Result : constant Implementation.Internal_Context :=
        Implementation.Create_Context
          (Charset        => Charset,
           File_Reader    => FR,
           Event_Handler  => null,
           Unit_Provider  => null,
           With_Trivia    => With_Trivia,
           Tab_Stop       => Actual_Tab_Stop,
           Max_Call_Depth => ${ctx.default_max_call_depth});
   begin
      return +Result;
   end Create_Context;

   ---------------------
   -- Context_Inc_Ref --
   ---------------------

   procedure Context_Inc_Ref (Context : Internal_Context) is
   begin
      Implementation.Inc_Ref (+Context);
   end Context_Inc_Ref;

   ---------------------
   -- Context_Dec_Ref --
   ---------------------

   procedure Context_Dec_Ref (Context : in out Internal_Context) is
      Ctx : Implementation.Internal_Context := +Context;
   begin
      Implementation.Dec_Ref (Ctx);
      Context := +Ctx;
   end Context_Dec_Ref;

   ---------------------
   -- Context_Version --
   ---------------------

   function Context_Version (Context : Internal_Context) return Version_Number
   is
      Ctx : constant Implementation.Internal_Context := +Context;
   begin
      return Ctx.Serial_Number;
   end Context_Version;

   ----------------------
   -- Context_Has_Unit --
   ----------------------

   function Context_Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean is
   begin
      return Implementation.Has_Unit (+Context, Unit_Filename);
   end Context_Has_Unit;

   ---------------------------
   -- Context_Get_From_File --
   ---------------------------

   function Context_Get_From_File
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Rule              : Grammar_Rule_Index) return Internal_Unit
   is
      Ctx : constant Implementation.Internal_Context := +Context;
   begin
      return +Implementation.Get_From_File
        (Ctx, Filename, Charset, Reparse, +Rule);
   end Context_Get_From_File;

   ------------------
   -- Unit_Version --
   ------------------

   function Unit_Version (Unit : Internal_Unit) return Version_Number is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return U.Unit_Version;
   end Unit_Version;

   -------------------
   -- Unit_Filename --
   -------------------

   function Unit_Filename (Unit : Internal_Unit) return String is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return Implementation.Get_Filename (U);
   end Unit_Filename;

   ---------------
   -- Unit_Root --
   ---------------

   function Unit_Root (Unit : Internal_Unit) return Internal_Node is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +U.AST_Root;
   end Unit_Root;

   ----------------------
   -- Unit_First_Token --
   ----------------------

   function Unit_First_Token (Unit : Internal_Unit) return Internal_Token is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +Implementation.First_Token (U);
   end Unit_First_Token;

   ---------------------
   -- Unit_Last_Token --
   ---------------------

   function Unit_Last_Token (Unit : Internal_Unit) return Internal_Token is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +Implementation.Last_Token (U);
   end Unit_Last_Token;

   -------------------
   -- Unit_Get_Line --
   -------------------

   function Unit_Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type
   is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return Implementation.Get_Line (U, Line_Number);
   end Unit_Get_Line;

   ---------------------------
   -- Node_Metadata_Inc_Ref --
   ---------------------------

   procedure Node_Metadata_Inc_Ref (Metadata : Internal_Node_Metadata) is
      MD : constant Internal_Node_Metadata_Access := +Metadata;
   begin
      MD.Ref_Count := MD.Ref_Count + 1;
   end Node_Metadata_Inc_Ref;

   ---------------------------
   -- Node_Metadata_Dec_Ref --
   ---------------------------

   procedure Node_Metadata_Dec_Ref (Metadata : in out Internal_Node_Metadata)
   is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Node_Metadata_Type, Internal_Node_Metadata_Access);
      MD : Internal_Node_Metadata_Access := +Metadata;
   begin
      MD.Ref_Count := MD.Ref_Count - 1;
      if MD.Ref_Count = 0 then
         Destroy (MD);
      end if;
      Metadata := No_Internal_Node_Metadata;
   end Node_Metadata_Dec_Ref;

   ---------------
   -- Node_Kind --
   ---------------

   function Node_Kind (Node : Internal_Node) return Type_Index is
      N : constant Implementation.${T.root_node.name} := +Node;
   begin
      return Node_Kinds (N.Kind);
   end Node_Kind;

   -----------------
   -- Node_Parent --
   -----------------

   function Node_Parent (Node : Internal_Node) return Internal_Node is
      N : constant Implementation.${T.root_node.name} := +Node;
   begin
      return +N.Parent;
   end Node_Parent;

   -------------------------
   -- Node_Children_Count --
   -------------------------

   function Node_Children_Count (Node : Internal_Node) return Natural is
      N : constant Implementation.${T.root_node.name} := +Node;
   begin
      return Implementation.Children_Count (N);
   end Node_Children_Count;

   --------------------
   -- Node_Get_Child --
   --------------------

   procedure Node_Get_Child
     (Node            : Internal_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Internal_Node)
   is
      R : Implementation.${T.root_node.name};
   begin
      Implementation.Get_Child (+Node, Index, Index_In_Bounds, R);
      Result := +R;
   end Node_Get_Child;

   ------------------------
   -- Node_Fetch_Sibling --
   ------------------------

   function Node_Fetch_Sibling
     (Node : Internal_Node; Offset : Integer) return Internal_Node is
   begin
      return +Implementation.Fetch_Sibling (+Node, Offset);
   end Node_Fetch_Sibling;

   ----------------------
   -- Node_Token_Start --
   ----------------------

   function Node_Token_Start (Node : Internal_Node) return Internal_Token is
   begin
      return +Implementation.Token_Start (+Node);
   end Node_Token_Start;

   --------------------
   -- Node_Token_End --
   --------------------

   function Node_Token_End (Node : Internal_Node) return Internal_Token is
   begin
      return +Implementation.Token_End (+Node);
   end Node_Token_End;

   ------------------
   -- Entity_Image --
   ------------------

   function Entity_Image (Entity : Internal_Entity) return String is
   begin
      return Implementation.Image (+Entity);
   end Entity_Image;

   --  Language descriptor table for ${ada_lib_name}.
   --
   --  We define it here and export its address to avoid making the
   --  $.Generic_API spec (which is public) depend on the
   --  $.Generic_Introspection one (which is private), which allows not
   --  exporting the many symbols from the latter when building a shared
   --  library (Windows has a small limit for the number of exported symbols).

   Language_Name : aliased constant Text_Type :=
     ${text_repr(ctx.lang_name.camel_with_underscores)};

   Desc : aliased constant Language_Descriptor :=
     (Language_Name => Language_Name'Access,

      Default_Grammar_Rule => ${main_rule_id},
      Grammar_Rule_Names   => Grammar_Rule_Names'Access,

      Token_Kind_Names => Token_Kind_Names'Access,

      Types          => Generic_Introspection.Types'Access,
      Enum_Types     => Generic_Introspection.Enum_Types'Access,
      Array_Types    => Generic_Introspection.Array_Types'Access,
      Struct_Types   => Generic_Introspection.Struct_Types'Access,
      Builtin_Types  => Generic_Introspection.Builtin_Types'Access,
      First_Node     => Generic_Introspection.First_Node,
      Struct_Members => Generic_Introspection.Struct_Members'Access,
      First_Property => Generic_Introspection.First_Property,

      Create_Context        => Create_Context'Access,
      Context_Inc_Ref       => Context_Inc_Ref'Access,
      Context_Dec_Ref       => Context_Dec_Ref'Access,
      Context_Version       => Context_Version'Access,
      Context_Has_Unit      => Context_Has_Unit'Access,
      Context_Get_From_File => Context_Get_From_File'Access,

      Unit_Version     => Unit_Version'Access,
      Unit_Filename    => Unit_Filename'Access,
      Unit_Root        => Unit_Root'Access,
      Unit_First_Token => Unit_First_Token'Access,
      Unit_Last_Token  => Unit_Last_Token'Access,
      Unit_Get_Line    => Unit_Get_Line'Access,

      Node_Metadata_Inc_Ref => Node_Metadata_Inc_Ref'Access,
      Node_Metadata_Dec_Ref => Node_Metadata_Dec_Ref'Access,

      Node_Kind           => Node_Kind'Access,
      Node_Parent         => Node_Parent'Access,
      Node_Children_Count => Node_Children_Count'Access,
      Node_Get_Child      => Node_Get_Child'Access,
      Node_Fetch_Sibling  => Node_Fetch_Sibling'Access,
      Node_Token_Start    => Node_Token_Start'Access,
      Node_Token_End      => Node_Token_End'Access,

      Entity_Image => Entity_Image'Access,

      Create_Enum => Create_Enum'Access);

   Desc_Address : constant System.Address := Desc'Address
     with Export, External_Name => "${ada_lib_name}__language_id";

   procedure Dummy is null;

end ${ada_lib_name}.Generic_API;
