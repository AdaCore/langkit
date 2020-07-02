## vim: filetype=makoada

with Ada.Calendar;              use Ada.Calendar;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
pragma Warnings (Off, "internal");
with Ada.Text_IO;               use Ada.Text_IO;

with GNATCOLL.Opt_Parse;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with ${ada_lib_name}.Analysis;  use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;    use ${ada_lib_name}.Common;
% if ctx.generate_unparser:
with ${ada_lib_name}.Unparsing; use ${ada_lib_name}.Unparsing;
% endif

procedure Parse is

   package String_Vectors is new Ada.Containers.Vectors
     (Natural, Unbounded_String);

   function Convert (Grammar_Rule_Name : String) return Grammar_Rule;

   package Args is
      use GNATCOLL.Opt_Parse;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help =>
           "Run ${ada_lib_name}'s"
           & " parser on an input string(s) or file(s)");

      package Silent is new Parse_Flag
        (Parser, "-s", "--silent",
         Help => "Do not print the representation of the resulting tree");

      package Print_Envs is new Parse_Flag
        (Parser, "-E", "--print-envs", "Print lexical environments computed");

      package Measure_Time is new Parse_Flag
        (Parser, "-t", "--time", "Time the execution of parsing");

      package Check is new Parse_Flag
        (Parser, "-C", "--check",
         Help => "Perform consistency checks on the tree");

      package Rule is new Parse_Option
        (Parser, "-r", "--rule-name",
         Arg_Type    => Grammar_Rule,
         Default_Val => Default_Grammar_Rule,
         Help        => "Rule name to parse");

      package Charset is new Parse_Option
        (Parser, "-c", "--charset",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Charset to use to decode the source code");

      package Do_Print_Trivia is new Parse_Flag
        (Parser, "-P", "--print-with-trivia",
         Help => "Print a simplified tree with trivia included");

      package Do_Print_Tokens is new Parse_Flag
        (Parser, "-T", "--print-tokens",
         Help => "Only print the stream of tokens/trivia");

      package Hide_Slocs is new Parse_Flag
        (Parser,
         Long => "--hide-slocs",
         Help => "When printing the tree, hide source locations");

      package Lookups is new Parse_Option_List
        (Parser, "-L", "--lookups",
         Arg_Type   => Unbounded_String,
         Accumulate => True,
         Help       => "Lookups to do in the parse tree");

      package File_Names is new Parse_Option_List
        (Parser, "-f", "--file-name",
         Arg_Type   => Unbounded_String,
         Accumulate => True,
         Help       => "Files to parse");

      package File_List is new Parse_Option
        (Parser, "-F", "--file-list",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        =>
           "Parse files listed in the provided filename with the regular"
            & " analysis circuitry (useful for timing measurements)");

      % if ctx.generate_unparser:
      package Do_Unparse is new Parse_Flag
        (Parser, "-u", "--unparse",
         Help => "Unparse the code with the built-in unparser");
      % endif
      package Strings is new Parse_Positional_Arg_List
        (Parser,
         Name        => "strings",
         Arg_Type    => Unbounded_String,
         Help        => "Raw strings to parse",
         Allow_Empty => True);
   end Args;

   procedure Process_Lookups (Node : ${root_entity.api_name}'Class);
   procedure Process_Node (Res : ${root_entity.api_name}'Class);
   procedure Process_File (Filename : String; Ctx : Analysis_Context);
   procedure Print_Token_Stream (Unit : Analysis_Unit);
   procedure Parse_Input (Content : String);

   function Create_Parse_Context return Analysis_Context is
     (Create_Context (Charset     => To_String (Args.Charset.Get),
                      With_Trivia => Args.Do_Print_Trivia.Get
                                     or else Args.Do_Print_Tokens.Get));

   -------------
   -- Convert --
   -------------

   function Convert (Grammar_Rule_Name : String) return Grammar_Rule is
   begin
      return Grammar_Rule'Value (Grammar_Rule_Name & "_Rule");
   exception
      when Constraint_Error =>
         raise GNATCOLL.Opt_Parse.Opt_Parse_Error
           with "Unsupported rule: " & Grammar_Rule_Name;
   end Convert;

   ---------------------
   -- Process_Lookups --
   ---------------------

   procedure Process_Lookups (Node : ${root_entity.api_name}'Class) is
   begin
      for Lookup_Str of Args.Lookups.Get loop
         New_Line;

         declare
            Sep : constant Natural := Index (Lookup_Str, ":");

            Line   : constant Line_Number := Line_Number'Value
              (Slice (Lookup_Str, 1, Sep - 1));
            Column : constant Column_Number := Column_Number'Value
              (Slice (Lookup_Str, Sep + 1, Length (Lookup_Str)));

            Sloc        : constant Source_Location := (Line, Column);
            Lookup_Node : constant ${root_entity.api_name} :=
               Lookup (Node, (Line, Column));
         begin
            Put_Line ("Lookup " & Image (Sloc) & ":");
            Print (Lookup_Node, not Args.Hide_Slocs.Get);
         end;
      end loop;
   end Process_Lookups;

   ------------------
   -- Process_Node --
   ------------------

   procedure Process_Node (Res : ${root_entity.api_name}'Class) is
   begin
      if Is_Null (Res) then
         Put_Line ("<null node>");
         return;
      end if;

      if not Args.Silent.Get then
         if Args.Do_Print_Trivia.Get then
            PP_Trivia (Res);
         else
            Print (Res, not Args.Hide_Slocs.Get);
         end if;
      end if;

      Process_Lookups (Res);

      % if ctx.generate_unparser:
      if Args.Do_Unparse.Get then
         Put_Line (Unparse (Res));
      end if;
      % endif
   end Process_Node;

   ------------------------
   -- Print_Token_Stream --
   ------------------------

   procedure Print_Token_Stream (Unit : Analysis_Unit) is
      use ${ada_lib_name}.Common.Token_Data_Handlers;

      Token : Token_Reference := First_Token (Unit);
      TD    : Token_Data_Type;
   begin
      while Token /= No_Token loop
         TD := Data (Token);
         Put_Line
           ("  [" & (if Is_Trivia (TD) then "trivia" else "token ")
            & Token_Index'Image (Index (TD)) & "] "
            & Token_Kind_Name (Kind (TD))
            & " " & Image (Text (Token), With_Quotes => True));
         Token := Next (Token);
      end loop;
      New_Line;
   end Print_Token_Stream;

   -----------------
   -- Parse_Input --
   -----------------

   procedure Parse_Input (Content : String) is
      Ctx  : constant Analysis_Context := Create_Parse_Context;
      Unit : Analysis_Unit;
   begin
      Unit := Get_From_Buffer
        (Context  => Ctx,
         Filename => "<input>",
         Buffer   => Content,
         Rule     => Args.Rule.Get);

      if Has_Diagnostics (Unit) then
         Put_Line ("Parsing failed:");
         for D of Diagnostics (Unit) loop
            Put_Line (Format_GNU_Diagnostic (Unit, D));
         end loop;
      end if;

      if Args.Do_Print_Tokens.Get then
         Print_Token_Stream (Unit);

      else
         --  Error recovery may make the parser return something even on error:
         --  process it anyway.
         Process_Node (Root (Unit));
      end if;
   end Parse_Input;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (Filename : String; Ctx : Analysis_Context)
   is
      package Node_Sets is new Ada.Containers.Hashed_Sets
        (${root_entity.api_name}, Hash, "=", "=");

      Set : Node_Sets.Set;

      procedure Check_Consistency
        (Node, Parent : ${root_entity.api_name});

      procedure Check_Consistency
        (Node, Parent : ${root_entity.api_name}) is
      begin
         if Node.Parent /= Parent then
            Put_Line ("Invalid parent for node " & Node.Image);
         end if;

         if Set.Contains (Node) then
            Put_Line ("Duplicate node" & Node.Image);
         end if;

         Set.Insert (Node);

         for C of Node.Children loop
            if not C.Is_Null then
               Check_Consistency (C, Node);
            end if;
         end loop;
      end Check_Consistency;

      Unit         : Analysis_Unit;
      Time_Before  : constant Time := Clock;
      Time_After   : Time;
      AST          : ${root_entity.api_name};
   begin
      Unit := Get_From_File (Ctx, Filename, "", True, Rule => Args.Rule.Get);
      AST := Root (Unit);
      Time_After := Clock;

      if Has_Diagnostics (Unit) then
         for D of Diagnostics (Unit) loop
            Put_Line (Format_GNU_Diagnostic (Unit, D));
         end loop;
      end if;

      if Args.Do_Print_Tokens.Get then
         Print_Token_Stream (Unit);

      elsif not Is_Null (AST) then
         if not Args.Silent.Get then
            if Args.Do_Print_Trivia.Get then
               PP_Trivia (Unit);
            else
               Print (AST, not Args.Hide_Slocs.Get);
            end if;

            Process_Lookups (AST);
         end if;

         if Args.Print_Envs.Get then
            Populate_Lexical_Env (Unit);
            Put_Line ("");
            Put_Line ("==== Dumping lexical environments ====");
            Dump_Lexical_Env (Unit);
         end if;

         if Args.Check.Get then
            Put_Line ("");
            Put_Line ("==== Checking tree consistency ====");
            if not AST.Is_Null then
               Check_Consistency
                 (AST, No_${root_entity.api_name});
            end if;
         end if;

         % if ctx.generate_unparser:
         if Args.Do_Unparse.Get then
            Put_Line (Unparse (AST));
         end if;
         % endif
      end if;

      if Args.Measure_Time.Get then
         Put_Line
           ("Time elapsed: " & Duration'Image (Time_After - Time_Before));
      end if;

   end Process_File;

begin
   if not Args.Parser.Parse then
      return;
   end if;

   if Args.File_List.Get /= Null_Unbounded_String then
      declare
         F   : File_Type;
         Ctx : constant Analysis_Context := Create_Parse_Context;
      begin
         Open (F, In_File, To_String (Args.File_List.Get));
         while not End_Of_File (F) loop
            declare
               Filename : constant String := Get_Line (F);
            begin
               Process_File (Filename, Ctx);
            end;
         end loop;
         Close (F);
      end;

   elsif Args.File_Names.Get'Length /= 0 then
      declare
         Ctx : constant Analysis_Context := Create_Parse_Context;
      begin
         for File_Name of Args.File_Names.Get loop
            Process_File (To_String (File_Name), Ctx);
         end loop;
      end;

   else
      for Input_Str_Unbounded of Args.Strings.Get loop
         declare
            Time_Before : constant Time := Clock;
            Time_After  : Time;
            Input_Str   : constant String := To_String (Input_Str_Unbounded);
         begin
            Parse_Input (Input_Str);
            Time_After := Clock;
            if Args.Measure_Time.Get then
               Put_Line
                 ("Time elapsed: "
                  & Duration'Image (Time_After - Time_Before));
            end if;
         end;
      end loop;

   end if;

end Parse;
