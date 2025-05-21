## vim: filetype=makoada

with System;

with Ada.Unchecked_Deallocation;

with ${ada_lib_name}.Public_Converters;  use ${ada_lib_name}.Public_Converters;
with ${ada_lib_name}.Implementation;

package body ${ada_lib_name}.Generic_Impl is

   ---------
   -- "+" --
   ---------

   function "+"
     (Entity : Internal_Entity) return Implementation.${root_entity.name}
   is
      Md : constant Internal_Node_Metadata_Access := +Entity.Metadata;
   begin
      return (Node => +Entity.Node,
              --  Metadata can be a null pointer, if it's coming from the
              --  generic ``No_Lk_Node`` value.
              Info => (Md           => (if Md = null
                                        then Implementation.No_Metadata
                                        else Md.Internal),
                       Rebindings   => Entity.Rebindings,
                       From_Rebound => Entity.From_Rebound));
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
     (Entity : Implementation.${root_entity.name}) return Internal_Entity
   is
      use ${ada_lib_name}.Implementation;

      Md : Internal_Node_Metadata_Access :=
        (if Entity.Info.Md = Implementation.No_Metadata
         then No_Metadata
         else new Internal_Node_Metadata_Type'
                    (Ref_Count => 1,
                     Internal  => Entity.Info.Md));
   begin
      return (Node         => +Entity.Node,
              Rebindings   => Entity.Info.Rebindings,
              From_Rebound => Entity.Info.From_Rebound,
              Metadata     => +Md);
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
         then ${cfg.library.defaults.tab_stop}
         else Tab_Stop);

      Result : constant Implementation.Internal_Context :=
        Implementation.Allocate_Context;
   begin
      Implementation.Initialize_Context
        (Context        => Result,
         Charset        => Charset,
         File_Reader    => FR,
         Event_Handler  => null,
         Unit_Provider  => null,
         With_Trivia    => With_Trivia,
         Tab_Stop       => Actual_Tab_Stop);
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

   -----------------------------
   -- Context_Has_With_Trivia --
   -----------------------------

   function Context_Has_With_Trivia (Context : Internal_Context) return Boolean
   is
   begin
      return Implementation.Has_With_Trivia (+Context);
   end Context_Has_With_Trivia;

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

   -----------------------------
   -- Context_Get_From_Buffer --
   -----------------------------

   function Context_Get_From_Buffer
     (Context                   : Internal_Context;
      Filename, Buffer, Charset : String;
      Rule                      : Grammar_Rule_Index) return Internal_Unit
   is
      Ctx : constant Implementation.Internal_Context := +Context;
   begin
      return +Implementation.Get_From_Buffer
        (Context  => Ctx,
         Filename => Filename,
         Buffer   => Buffer,
         Charset  => Charset,
         Rule     => +Rule);
   end Context_Get_From_Buffer;

   ----------------------------
   -- Context_Templates_Unit --
   ----------------------------

   function Context_Templates_Unit
     (Context : Internal_Context) return Internal_Unit
   is
      Ctx : constant Implementation.Internal_Context := +Context;
   begin
      return +Implementation.Templates_Unit (Ctx);
   end Context_Templates_Unit;

   ------------------
   -- Unit_Context --
   ------------------

   function Unit_Context (Unit : Internal_Unit) return Internal_Context is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +U.Context;
   end Unit_Context;

   ------------------
   -- Unit_Version --
   ------------------

   function Unit_Version (Unit : Internal_Unit) return Version_Number is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return U.Unit_Version;
   end Unit_Version;

   ----------------------------
   -- Unit_Reparse_From_File --
   ----------------------------

   procedure Unit_Reparse_From_File (Unit : Internal_Unit; Charset : String) is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      Implementation.Reparse (Unit => U, Charset => Charset);
   end Unit_Reparse_From_File;

   ------------------------------
   -- Unit_Reparse_From_Buffer --
   ------------------------------

   procedure Unit_Reparse_From_Buffer
     (Unit : Internal_Unit; Buffer : String; Charset : String)
   is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      Implementation.Reparse (Unit => U, Buffer => Buffer, Charset => Charset);
   end Unit_Reparse_From_Buffer;

   -------------------
   -- Unit_Filename --
   -------------------

   function Unit_Filename (Unit : Internal_Unit) return String is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return Implementation.Get_Filename (U);
   end Unit_Filename;

   ------------------
   -- Unit_Charset --
   ------------------

   function Unit_Charset (Unit : Internal_Unit) return String is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return Implementation.Get_Charset (U);
   end Unit_Charset;

   ----------------------
   -- Unit_Diagnostics --
   ----------------------

   function Unit_Diagnostics (Unit : Internal_Unit) return Diagnostics_Access
   is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return U.Diagnostics'Unrestricted_Access;
   end Unit_Diagnostics;

   --------------------------------
   -- Unit_Format_GNU_Diagnostic --
   --------------------------------

   function Unit_Format_GNU_Diagnostic
     (Unit : Internal_Unit; D : Diagnostic) return String
   is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return Implementation.Format_GNU_Diagnostic (U, D);
   end Unit_Format_GNU_Diagnostic;

   ---------------
   -- Unit_Root --
   ---------------

   function Unit_Root (Unit : Internal_Unit) return Internal_Node is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +U.Ast_Root;
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

   -----------------------
   -- Unit_Lookup_Token --
   -----------------------

   function Unit_Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Internal_Token
   is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +Implementation.Lookup_Token (U, Sloc);
   end Unit_Lookup_Token;

   ----------------------
   -- Unit_Token_Count --
   ----------------------

   function Unit_Token_Count (Unit : Internal_Unit) return Natural is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +Implementation.Token_Count (U);
   end Unit_Token_Count;

   -----------------------
   -- Unit_Trivia_Count --
   -----------------------

   function Unit_Trivia_Count (Unit : Internal_Unit) return Natural is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +Implementation.Trivia_Count (U);
   end Unit_Trivia_Count;

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

   ---------------------
   -- Unit_Do_Parsing --
   ---------------------

   procedure Unit_Do_Parsing
     (Unit : Internal_Unit; Input : Lexer_Input; Result : out Reparsed_Unit)
   is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      Implementation.Do_Parsing (U, Input, Result);
   end Unit_Do_Parsing;

   -------------------
   -- Unit_Set_Rule --
   -------------------

   procedure Unit_Set_Rule (Unit : Internal_Unit; Rule : Grammar_Rule_Index) is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      Implementation.Set_Rule (U, +Rule);
   end Unit_Set_Rule;

   -------------------------------
   -- Unit_Update_After_Reparse --
   -------------------------------

   procedure Unit_Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : out Reparsed_Unit)
   is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      Implementation.Update_After_Reparse (U, Reparsed);
   end Unit_Update_After_Reparse;

   ---------------------------
   -- Node_Metadata_Inc_Ref --
   ---------------------------

   procedure Node_Metadata_Inc_Ref (Metadata : Internal_Node_Metadata) is
      Md : constant Internal_Node_Metadata_Access := +Metadata;
   begin

      pragma Assert (Md /= null);
      pragma Assert (Md /= No_Metadata);

      Md.Ref_Count := Md.Ref_Count + 1;
   end Node_Metadata_Inc_Ref;

   ---------------------------
   -- Node_Metadata_Dec_Ref --
   ---------------------------

   procedure Node_Metadata_Dec_Ref (Metadata : in out Internal_Node_Metadata)
   is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Node_Metadata_Type, Internal_Node_Metadata_Access);
      Md : Internal_Node_Metadata_Access := +Metadata;
   begin

      pragma Assert (Md /= null);
      pragma Assert (Md /= No_Metadata);

      Md.Ref_Count := Md.Ref_Count - 1;
      if Md.Ref_Count = 0 then
         Destroy (Md);
      end if;
      Metadata := Internal_Node_Metadata (System.Null_Address);
   end Node_Metadata_Dec_Ref;

   ---------------------------
   -- Node_Metadata_Compare --
   ---------------------------

   function Node_Metadata_Compare
      (L, R : Internal_Node_Metadata) return Boolean
   is
      Left  : Internal_Node_Metadata_Access := +L;
      Right : Internal_Node_Metadata_Access := +R;
      use ${ada_lib_name}.Implementation;
   begin
      --  Compare metadata with the generated ``Compare_Metadata`` function, if
      --  applicable.
      if Left = null then
         Left := No_Metadata;
      end if;

      if Right = null then
         Right := No_Metadata;
      end if;

      return Compare_Metadata (Left.Internal, Right.Internal);
   end Node_Metadata_Compare;

   ---------------
   -- Node_Unit --
   ---------------

   function Node_Unit (Node : Internal_Node) return Internal_Unit is
      N : constant Implementation.${T.root_node.name} := +Node;
   begin
      return +N.Unit;
   end Node_Unit;

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

   function Node_Parent (Node : Internal_Entity) return Internal_Entity is
      E      : constant Implementation.${root_entity.name} := +Node;
      Result : constant Implementation.${root_entity.name} :=
        Implementation.Parent (E.Node, E.Info);
   begin
      return +Result;
   end Node_Parent;

   ------------------
   -- Node_Parents --
   ------------------

   function Node_Parents
     (Node : Internal_Entity; With_Self : Boolean) return Internal_Entity_Array
   is
      E       : constant Implementation.${root_entity.name} := +Node;
      Parents : Implementation.${root_entity.array.name} :=
        Implementation.Parents (E.Node, With_Self, E.Info);
   begin
      return Result : Internal_Entity_Array (Parents.Items'Range) do
         for I in Result'Range loop
            Result (I) := +Parents.Items (I);
         end loop;
         Implementation.Dec_Ref (Parents);
      end return;
   end Node_Parents;

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

   -------------------
   -- Node_Is_Ghost --
   -------------------

   function Node_Is_Ghost (Node : Analysis.Internal_Node) return Boolean is
   begin
      return Implementation.Is_Ghost (+Node);
   end Node_Is_Ghost;

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

   ---------------
   -- Node_Text --
   ---------------

   function Node_Text (Node : Internal_Node) return Text_Type is
   begin
      return Implementation.Text (+Node);
   end Node_Text;

   ---------------------
   -- Node_Sloc_Range --
   ---------------------

   function Node_Sloc_Range
     (Node : Internal_Node) return Source_Location_Range is
   begin
      return Implementation.Sloc_Range (+Node);
   end Node_Sloc_Range;

   -----------------
   -- Node_Lookup --
   -----------------

   function Node_Lookup
     (Node : Analysis.Internal_Node;
      Sloc : Source_Location) return Analysis.Internal_Node is
   begin
      return +Implementation.Lookup (+Node, Sloc);
   end Node_Lookup;

   -------------------------------
   -- Node_Last_Attempted_Child --
   -------------------------------

   function Node_Last_Attempted_Child (Node : Internal_Node) return Integer is
      N : Implementation.${T.root_node.name} := +Node;
   begin
      return N.Last_Attempted_Child;
   end Node_Last_Attempted_Child;

   ------------------------------
   -- Node_Children_And_Trivia --
   ------------------------------

   function Node_Children_And_Trivia
     (Node : Internal_Node) return Node_Or_Token_Array_Access
   is
      N : Implementation.${T.root_node.name} := +Node;
      R : constant Implementation.Bare_Children_Vector :=
        N.Children_And_Trivia;
   begin
      return Result : constant Node_Or_Token_Array_Access :=
        new Node_Or_Token_Array (1 .. R.Last_Index)
      do
         for I in Result.all'Range loop
            declare
               E : Implementation.Bare_Child_Record renames R (I);
            begin
               case E.Kind is
                  when Common.Child =>
                     Result (I) := (Is_Node => True, Node => +E.Node);
                  when Common.Trivia =>
                     Result (I) := (Is_Node => False, Token => +E.Trivia);
               end case;
            end;
         end loop;
      end return;
   end Node_Children_And_Trivia;

   ------------------
   -- Entity_Image --
   ------------------

   function Entity_Image (Entity : Internal_Entity) return String is
   begin
      return Implementation.Image (+Entity);
   end Entity_Image;

   -------------------------
   -- Token_Is_Equivalent --
   -------------------------

   function Token_Is_Equivalent
     (Left, Right       : Internal_Token;
      Left_SN, Right_SN : Token_Safety_Net) return Boolean
   is
   begin
      return Common.Is_Equivalent
        (Wrap_Token (Left_SN.Context, Left),
         Wrap_Token (Right_SN.Context, Right));
   end Token_Is_Equivalent;

end ${ada_lib_name}.Generic_Impl;
