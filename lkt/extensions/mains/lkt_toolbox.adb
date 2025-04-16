with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with GNAT.Traceback.Symbolic;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Liblktlang.Analysis;             use Liblktlang.Analysis;
with Liblktlang.Common;
with Liblktlang.Semantic_Diagnostics; use Liblktlang.Semantic_Diagnostics;
with Liblktlang_Support.Diagnostics;  use Liblktlang_Support.Diagnostics;
with Liblktlang_Support.Diagnostics.Output;
use Liblktlang_Support.Diagnostics.Output;
with Liblktlang_Support.Slocs;        use Liblktlang_Support.Slocs;
with Liblktlang_Support.Text;         use Liblktlang_Support.Text;

procedure Lkt_Toolbox is

   package Arg is

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Lkt toolbox. Toolbox like command line frontend for the "
                 & "LKT langkit library.");

      package Files is new Parse_Positional_Arg_List
        (Parser   => Parser,
         Name     => "files",
         Arg_Type => Unbounded_String,
         Help     => "The files to parse");

      package Check_Only is new Parse_Flag
        (Parser => Parser,
         Short  => "-C",
         Long   => "--check-only",
         Help   => "Only output the errors");

      package Flag_Invalid is new Parse_Flag
        (Parser => Parser,
         Short  => "-I",
         Long   => "--check-invalid-decls",
         Help   => "Flag decls that generate errors that are not annotated"
                   & " with the @invalid annotation. Also flag decls"
                   & " annotated with @invalid that don't trigger any errors");

      package Debug_Solver is new Parse_Flag
        (Parser => Parser,
         Short  => "-D",
         Long   => "--debug",
         Help   => "Enable Solver debug traces");

      package Solve_Line is new Parse_Option
        (Parser => Parser,
         Short  => "-L",
         Long   => "--solve-line",
         Arg_Type => Natural,
         Help   => "Only do name resolution at line N",
         Default_Val => 0);
   end Arg;

   use Liblktlang;

   package Invalid_Decl_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Analysis.Lkt_Node,
      Element_Type    => Boolean,
      Hash            => Analysis.Hash,
      Equivalent_Keys => "=");

   procedure Print_Lkt_Toolbox_Diagnostic
     (Node : Lkt_Node'Class; Message : Text_Type);
   --  Internal wrapper to ``Print_Diagnostic`` used by lkt_toolbox to print
   --  additional diagnostics.

   procedure Print_Nameres_Results
     (Node : Lkt_Node'Class; Perform_Analysis : Boolean := False);
   --  Print name and type resolution information about a node if
   --  ``Perform_Analysis`` is ``True`` and recurse on its children for testing
   --  and debugging purposes.
   --
   --  The value of ``Perform_Analysis`` will be given to recursive calls, but
   --  may change to ``True`` when the node's source location line is equal to
   --  ``Solve_Line``.

   function Populate_Invalid_Decl_Map
     (Node : Analysis.Lkt_Node'Class) return Common.Visit_Status;
   --  Populate ``Invalid_Decl_Map`` and reject declarations with ``@invalid``
   --  annotations that are nested in another declaration annotated with
   --  ``@invalid``.

   Invalid_Decl_Map : Invalid_Decl_Maps.Map;
   --  Map of declarations annotated with ``@invalid``. The boolean elements of
   --  the map are initialized to ``False`` and set to ``True`` whenever a
   --  diagnostic is emitted for the related declaration. Therefore, this map
   --  is used to check that at least one diagnostic has been emitted for each
   --  declaration annotated with ``@invalid``.

   ----------------------------------
   -- Print_Lkt_Toolbox_Diagnostic --
   ----------------------------------

   procedure Print_Lkt_Toolbox_Diagnostic
     (Node : Lkt_Node'Class; Message : Text_Type)
   is
      Sloc_Range : constant Source_Location_Range := Node.Sloc_Range;
      Unit       : constant Analysis.Analysis_Unit := Node.Unit;
      Path       : constant String := Simple_Name (Unit.Get_Filename);
   begin
      Print_Diagnostic ((Sloc_Range, To_Unbounded_Text (Message)), Unit, Path);
   end Print_Lkt_Toolbox_Diagnostic;

   ---------------------------
   -- Print_Nameres_Results --
   ---------------------------

   Indent : Natural := 0;

   procedure Print_Nameres_Results
     (Node : Lkt_Node'Class; Perform_Analysis : Boolean := False)
   is
      procedure Put_Line_Indent (Item : Text_Type);
      --  Print ``Item`` on the standard output with indentation (using
      --  ``Indent`` to determine how much to indent).

      function Get_Custom_Image
        (Node : Lkt_Node'Class) return Text_Type;
      --  Wrapper around the various ``P_Custom_Image`` node properties that
      --  handles null nodes in addition.

      procedure Print_Expr_Nameres (Node : Expr'Class);
      --  Print the type and name resolution of an Expr

      procedure Print_Ref_Id_Nameres (Node : Ref_Id'Class);
      --  Print the type and name resolution of a Ref_Id

      procedure Print_Base_Val_Decl_Nameres (Node : Base_Val_Decl'Class);
      --  Print the type and name resolution of an Base_Val_Decl

      ---------------------
      -- Put_Line_Indent --
      ---------------------

      procedure Put_Line_Indent (Item : Text_Type)
      is
         use Ada.Wide_Wide_Text_IO;
      begin
         Put (Indent * "   ");
         Put_Line (Item);
      end Put_Line_Indent;

      ----------------------
      -- Get_Custom_Image --
      ----------------------

      function Get_Custom_Image (Node : Lkt_Node'Class) return Text_Type
      is
         use type Common.Lkt_Node_Kind_Type;
      begin
         if Node.Is_Null then
            return "None";
         elsif Node.Kind in Common.Lkt_Decl then
            return Node.As_Decl.P_Custom_Image;
         elsif Node.Kind = Common.Lkt_Ref_Id then
            return  Node.As_Ref_Id.P_Custom_Image;
         else
            return To_Text (Node.Image);
         end if;
      end Get_Custom_Image;

      ------------------------
      -- Print_Expr_Nameres --
      ------------------------

      procedure Print_Expr_Nameres (Node : Expr'Class) is
      begin
         Put_Line_Indent ("Expr " & Get_Custom_Image (Node));
         Put_Line_Indent
           ("     has_type " & Get_Custom_Image (Node.P_Get_Type));
         New_Line;
      end Print_Expr_Nameres;

      --------------------------
      -- Print_Ref_Id_Nameres --
      --------------------------

      procedure Print_Ref_Id_Nameres (Node : Ref_Id'Class) is
      begin
         Put_Line_Indent ("Id   " & Get_Custom_Image (Node));
         Put_Line_Indent
           ("     has_type " & Get_Custom_Image (Node.P_Get_Type));
         Put_Line_Indent
           ("     references " & Get_Custom_Image (Node.P_Referenced_Decl));
         New_Line;
      end Print_Ref_Id_Nameres;

      ---------------------------------
      -- Print_Base_Val_Decl_Nameres --
      ---------------------------------

      procedure Print_Base_Val_Decl_Nameres (Node : Base_Val_Decl'Class) is
      begin
         Put_Line_Indent ("Decl " & Get_Custom_Image (Node));
         Put_Line_Indent
           ("     has_type " & Get_Custom_Image (Node.P_Get_Type));
         New_Line;
      end Print_Base_Val_Decl_Nameres;

      use type Common.Lkt_Node_Kind_Type;

      Indented     : Boolean := False;

      Node_Line    : constant Line_Number := Sloc_Range (Node).Start_Line;
      Allowed_Line : constant Line_Number := Line_Number (Arg.Solve_Line.Get);
      Can_Nameres  : constant Boolean :=
         Perform_Analysis or else Node_Line = Allowed_Line;
   begin
      --  The Lkt specification does not handle name and type resolution in
      --  lexer and grammar declarations: ignore them.
      if Node.Kind in Common.Lkt_Lexer_Decl | Common.Lkt_Grammar_Decl then
         return;
      end if;

      --  If the analysis was not successful, print the emitted diagnostics.
      --  In any case, print the resulting name and type resolution
      --  information.
      if Can_Nameres then
         if Node.P_Xref_Entry_Point then
            declare
               Results : constant Solver_Result :=
                  Node.P_Solve_Enclosing_Context;
            begin
               if not Analysis.Success (Results) then
                  Put_Line (Node.Image & " failed nameres:");
                  for Diagnostic of Analysis.Diagnostics (Results) loop
                     Print_Solver_Diagnostic (Diagnostic);
                     New_Line;
                  end loop;
               end if;
            end;
         end if;
         begin
            if Node.Kind = Common.Lkt_Ref_Id then
                  Print_Ref_Id_Nameres (Node.As_Ref_Id);
                  Indented := True;
                  Indent := Indent + 1;
            elsif Node.Kind = Common.Lkt_Def_Id then
               null;
            elsif Node.Kind in Common.Lkt_Base_Val_Decl then
               Print_Base_Val_Decl_Nameres (Node.As_Base_Val_Decl);
               Indented := True;
               Indent := Indent + 1;
            elsif Node.Kind in Common.Lkt_Expr then
               Print_Expr_Nameres (Node.As_Expr);
               Indented := True;
               Indent := Indent + 1;
            end if;
         exception
            when E : Liblktlang.Common.Property_Error =>
               Put_Line_Indent
                 ("ERROR: Resolution failed for node "
                  & Get_Custom_Image (Node));
               Put_Line_Indent
                 ("   with message """
                  & To_Text (Ada.Exceptions.Exception_Message (E))
                  & """");
         end;
      end if;

      for Child of Node.Children loop
         if not Child.Is_Null then
            Print_Nameres_Results (Child, Can_Nameres);
         end if;
      end loop;

      if Indented then
         Indent := Indent - 1;
      end if;

   end Print_Nameres_Results;

   -------------------------------
   -- Populate_Invalid_Decl_Map --
   -------------------------------

   function Populate_Invalid_Decl_Map
     (Node : Analysis.Lkt_Node'Class) return Common.Visit_Status
   is
      use type Common.Lkt_Node_Kind_Type;
   begin
      --  Populate ``Invalid_Decl_Map`` with declarations annotated with
      --  ``@invalid``.

      if Node.Kind = Common.Lkt_Full_Decl
         and then Node.As_Full_Decl.P_Has_Annotation
           (To_Unbounded_Text ("invalid"))
      then
         --  ``P_Topmost_Invalid_Decl`` should return the same node. In that
         --  case, include this node in the map, otherwise nested ``@invalid``
         --  declarations have been detected: emit a diagnostic.

         if Invalid_Decl_Map.Contains (Node.P_Topmost_Invalid_Decl) then
            Set_Exit_Status (1);

            Print_Lkt_Toolbox_Diagnostic (Node, "nested @invalid declaration");
         else
            Invalid_Decl_Map.Include (Node.As_Lkt_Node, False);
         end if;
      end if;
      return Common.Into;
   end Populate_Invalid_Decl_Map;

   Ctx : constant Analysis.Analysis_Context := Analysis.Create_Context;
begin
   if Arg.Parser.Parse then
      Set_Solver_Debug_Mode (Arg.Debug_Solver.Get);
      for File_Name of Arg.Files.Get loop
         declare
            File_Name_Str : constant String := To_String (File_Name);
            Unit          : constant Analysis.Analysis_Unit :=
               Ctx.Get_From_File (File_Name_Str);
         begin
            if not Arg.Check_Only.Get then
               Put_Line ("Resolving " & File_Name_Str);
               Put_Line ((File_Name_Str'Length + 10) * "=");
            end if;

            if Unit.Diagnostics'Length > 0 then
               for Diagnostic of Unit.Diagnostics loop
                  Print_Diagnostic
                    (Diagnostic, Unit, Simple_Name (Unit.Get_Filename));
               end loop;
               return;
            end if;

            if Arg.Flag_Invalid.Get then
               Unit.Root.Traverse (Populate_Invalid_Decl_Map'Access);
            end if;

            if Arg.Check_Only.Get then
               Print_Solver_Diagnostics_In_Unit (Unit);
            else
               Print_Nameres_Results (Unit.Root, Arg.Solve_Line.Get = 0);
            end if;

            if Arg.Flag_Invalid.Get then

               --  Ensure that all ``@invalid`` declarations in the map have
               --  corresponding diagnostics. Otherwise, emit an error.

               for E in Invalid_Decl_Map.Iterate loop
                  if not Invalid_Decl_Maps.Element (E) then
                     Set_Exit_Status (1);

                     Print_Lkt_Toolbox_Diagnostic
                       (Invalid_Decl_Maps.Key (E),
                        "@invalid declaration without diagnostic");
                  end if;
               end loop;

            end if;

         end;
      end loop;
   end if;
exception
   when E : Common.Property_Error =>
      Put_Line (Ada.Exceptions.Exception_Message (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Lkt_Toolbox;
