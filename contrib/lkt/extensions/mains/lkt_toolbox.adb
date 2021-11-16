with Ada.Command_Line;            use Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Directories;             use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;                 use Ada.Text_IO;

with GNAT.Traceback.Symbolic;
with GNATCOLL.Opt_Parse;          use GNATCOLL.Opt_Parse;
with GNATCOLL.Traces;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Langkit_Support.Diagnostics.Output;
use Langkit_Support.Diagnostics.Output;

with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Liblktlang.Analysis;         use Liblktlang.Analysis;
with Liblktlang.Common;

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

   end Arg;

   use Liblktlang;

   package Invalid_Decl_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Analysis.Lkt_Node,
      Element_Type    => Boolean,
      Hash            => Analysis.Hash,
      Equivalent_Keys => "=");

   procedure Print_Semantic_Result
     (S : Analysis.Semantic_Result; Unit : Analysis.Analysis_Unit);
   --  Print a semantic result

   function Format_Node (Decl_Node : Decl'Class) return String;
   --  Format node for semantic result printing

   procedure Print_Lkt_Toolbox_Diagnostic
     (Node : Lkt_Node'Class; Message : Wide_Wide_String);
   --  Internal wrapper to ``Print_Diagnostic`` used by lkt_toolbox to print
   --  additional diagnostics.

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

   -----------------
   -- Format_Node --
   -----------------

   function Format_Node (Decl_Node : Decl'Class) return String is
   begin
      --  Remove rebindings information as there is no easy way to filter
      --  out/format rebindings information involving prelude declarations.
      return Decl_Node.P_As_Bare_Decl.Image;
   end Format_Node;

   ----------------------------------
   -- Print_Lkt_Toolbox_Diagnostic --
   ----------------------------------

   procedure Print_Lkt_Toolbox_Diagnostic
     (Node : Lkt_Node'Class; Message : Wide_Wide_String)
   is
      Sloc_Range : constant Source_Location_Range := Node.Sloc_Range;
      Unit       : constant Analysis.Analysis_Unit := Node.Unit;
      Path       : constant String := Simple_Name (Unit.Get_Filename);
   begin
      Print_Diagnostic ((Sloc_Range, To_Unbounded_Text (Message)), Unit, Path);
   end Print_Lkt_Toolbox_Diagnostic;

   ---------------------------
   -- Print_Semantic_Result --
   ---------------------------

   procedure Print_Semantic_Result
     (S : Analysis.Semantic_Result; Unit : Analysis.Analysis_Unit)
   is
      Node : constant Lkt_Node'Class := Analysis.Node (S);
   begin
      if Analysis.Error_Message (S) /= "" then
         declare
            Diag : constant Diagnostic :=
              (Node.Sloc_Range,
               To_Unbounded_Text (Analysis.Error_Message (S)));
         begin
            --  Emit an error if the declaration including ``Node`` has no
            --  ``@invalid`` annotation. Update ``Invalid_Decl_Map`` otherwise.

            if Node.P_Topmost_Invalid_Decl.Is_Null then
               Set_Exit_Status (1);

               Print_Lkt_Toolbox_Diagnostic
                 (Node,
                  "unexpected diagnostic, is @invalid annotation missing?");
            else
               Invalid_Decl_Map (Node.P_Topmost_Invalid_Decl) := True;
            end if;

            Print_Diagnostic
              (Diag, Unit, Simple_Name (Node.Unit.Get_Filename));
         end;
      elsif
        not Arg.Check_Only.Get
        and then not Analysis.Result_Type (S).Is_Null
      then
         Put_Line ("Expr " & Node.Image);
         Put_Line ("     has type " & Analysis.Result_Type (S).Image);
         New_Line;
      elsif
      not Arg.Check_Only.Get
        and then not Analysis.Result_Ref (S).Is_Null
      then
         Put_Line ("Id   " & Node.Image);
         Put_Line ("     references " & Format_Node (Analysis.Result_Ref (S)));
         New_Line;
      end if;
   end Print_Semantic_Result;

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
   GNATCOLL.Traces.Parse_Config_File;

   if Arg.Parser.Parse then
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

            Unit.Root.Traverse (Populate_Invalid_Decl_Map'Access);

            declare
               Diags : constant Analysis.Tree_Semantic_Result :=
                 Unit.Root.P_Check_Semantic;
            begin
               for D of Analysis.Results (Diags) loop
                  Print_Semantic_Result (D, Unit);
               end loop;
            end;

            --  Ensure that all ``@invalid`` declarations in the map have been
            --  reported. Print a diagnostic otherwise.

            for E in Invalid_Decl_Map.Iterate loop
               if not Invalid_Decl_Maps.Element (E) then
                  Set_Exit_Status (1);

                  Print_Lkt_Toolbox_Diagnostic
                    (Invalid_Decl_Maps.Key (E),
                     "@invalid declaration without diagnostic");
               end if;
            end loop;

         end;
      end loop;
   end if;
exception
   when E : Common.Property_Error =>
      Put_Line (Ada.Exceptions.Exception_Message (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Lkt_Toolbox;
