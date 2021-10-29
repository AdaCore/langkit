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

   procedure Print_Semantic_Result
     (S : Analysis.Semantic_Result; Unit : Analysis.Analysis_Unit);
   --  Print a semantic result

   function Format_Node (Decl_Node : Decl'Class) return String;
   --  Format node for semantic result printing

   -----------------
   -- Format_Node --
   -----------------

   function Format_Node (Decl_Node : Decl'Class) return String is
   begin
      --  Remove rebindings information as there is no easy way to filter
      --  out/format rebindings information involving prelude declarations.
      return Decl_Node.P_As_Bare_Decl.Image;
   end Format_Node;

   ---------------------------
   -- Print_Semantic_Result --
   ---------------------------

   procedure Print_Semantic_Result
     (S : Analysis.Semantic_Result; Unit : Analysis.Analysis_Unit)
   is
   begin
      if Analysis.Error_Message (S) /= "" then
         declare
            Diag : constant Diagnostic :=
              (Analysis.Node (S).Sloc_Range,
               To_Unbounded_Text (Analysis.Error_Message (S)));
         begin
            Print_Diagnostic
              (Diag, Unit,
               Simple_Name (Analysis.Node (S).Unit.Get_Filename));
         end;
      elsif
        not Arg.Check_Only.Get
        and then not Analysis.Result_Type (S).Is_Null
      then
         Put_Line ("Expr " & Analysis.Node (S).Image);
         Put_Line ("     has type " & Analysis.Result_Type (S).Image);
         New_Line;
      elsif
      not Arg.Check_Only.Get
        and then not Analysis.Result_Ref (S).Is_Null
      then
         Put_Line ("Id   " & Analysis.Node (S).Image);
         Put_Line ("     references " & Format_Node (Analysis.Result_Ref (S)));
         New_Line;
      end if;
   end Print_Semantic_Result;

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

            declare
               Diags : constant Analysis.Tree_Semantic_Result :=
                 Unit.Root.P_Check_Semantic;
            begin
               for D of Analysis.Results (Diags) loop
                  Print_Semantic_Result (D, Unit);
               end loop;
            end;
         end;
      end loop;
   end if;
exception
   when E : Common.Property_Error =>
      Put_Line (Ada.Exceptions.Exception_Message (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Lkt_Toolbox;
