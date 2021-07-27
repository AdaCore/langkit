## vim: filetype=makoada

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with ${ada_lib_name}.Common;
with ${ada_lib_name}.Public_Converters; use ${ada_lib_name}.Public_Converters;

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

   ---------------
   -- Unit_Root --
   ---------------

   function Unit_Root (Unit : Internal_Unit) return Internal_Node is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +U.AST_Root;
   end Unit_Root;

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

   ------------------
   -- Entity_Image --
   ------------------

   function Entity_Image (Entity : Internal_Entity) return String is
   begin
      return Implementation.Image (+Entity);
   end Entity_Image;

end ${ada_lib_name}.Generic_API;
