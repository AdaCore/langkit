## vim: filetype=makoada

private with Ada.Unchecked_Conversion;

private with Langkit_Support.File_Readers;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
private with Langkit_Support.Internal;
private with Langkit_Support.Text;
private with Langkit_Support.Types;

private with ${ada_lib_name}.Implementation;

package ${ada_lib_name}.Generic_API is

   Id : constant Language_Id;
   --  Unique identifier for ${ada_lib_name}

private

   use Langkit_Support.File_Readers;
   use Langkit_Support.Internal;
   use Langkit_Support.Text;
   use Langkit_Support.Types;

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
      ${name} : aliased constant String :=
        ${ascii_repr(names.Name.from_lower(n).camel_with_underscores)};
   % endfor
   <% assert main_rule_id is not None %>
   Grammar_Rule_Names : aliased constant Grammar_Rule_Name_Array :=
     (${", ".join(rule_name_refs)});

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
   function Unit_Root (Unit : Internal_Unit) return Internal_Node;
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
   function "+" is new Ada.Unchecked_Conversion
     (Internal_Node_Metadata_Access, Internal_Node_Metadata);

   procedure Node_Metadata_Inc_Ref (Metadata : Internal_Node_Metadata);
   procedure Node_Metadata_Dec_Ref (Metadata : in out Internal_Node_Metadata);

   function Node_Parent (Node : Internal_Node) return Internal_Node;
   function Node_Children_Count (Node : Internal_Node) return Natural;
   procedure Node_Get_Child
     (Node            : Internal_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Internal_Node);
   function Node_Fetch_Sibling
     (Node : Internal_Node; Offset : Integer) return Internal_Node;

   function Entity_Image (Entity : Internal_Entity) return String;

   --  Language descriptor table for ${ada_lib_name}

   Language_Name : aliased constant String :=
     ${ascii_repr(ctx.lang_name.camel_with_underscores)};

   Desc : aliased constant Language_Descriptor :=
     (Language_Name => Language_Name'Access,

      Default_Grammar_Rule => ${main_rule_id},
      Grammar_Rule_Names   => Grammar_Rule_Names'Access,

      Create_Context        => Create_Context'Access,
      Context_Inc_Ref       => Context_Inc_Ref'Access,
      Context_Dec_Ref       => Context_Dec_Ref'Access,
      Context_Version       => Context_Version'Access,
      Context_Has_Unit      => Context_Has_Unit'Access,
      Context_Get_From_File => Context_Get_From_File'Access,

      Unit_Version  => Unit_Version'Access,
      Unit_Root     => Unit_Root'Access,
      Unit_Get_Line => Unit_Get_Line'Access,

      Node_Metadata_Inc_Ref => Node_Metadata_Inc_Ref'Access,
      Node_Metadata_Dec_Ref => Node_Metadata_Dec_Ref'Access,

      Node_Parent         => Node_Parent'Access,
      Node_Children_Count => Node_Children_Count'Access,
      Node_Get_Child      => Node_Get_Child'Access,
      Node_Fetch_Sibling  => Node_Fetch_Sibling'Access,

      Entity_Image => Entity_Image'Access);

   function "+" is new Ada.Unchecked_Conversion
     (Language_Descriptor_Access, Language_Id);
   Id : constant Language_Id := +Desc'Access;

end ${ada_lib_name}.Generic_API;
