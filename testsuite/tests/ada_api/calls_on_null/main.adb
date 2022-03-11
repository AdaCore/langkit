with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Langkit_Support.Symbols;     use Langkit_Support.Symbols;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is

   procedure Put_Line (Exc : Exception_Occurrence);
   --  Helper to print exception information

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Exc : Exception_Occurrence) is
   begin
      Put_Line (Exception_Name (Exc) & " : " & Exception_Message (Exc));
   end Put_Line;

   Dummy_Bool  : Boolean;
   Dummy_Unit  : Analysis_Unit;
   Dummy_Node  : Foo_Node;
   Dummy_Token : Token_Reference;
   Dummy_Int   : Integer;

begin
   Put_Line ("Context:Has_Unit");
   begin
      Dummy_Bool := No_Analysis_Context.Has_Unit ("foo");
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Context:Get_From_File");
   begin
      Dummy_Unit := No_Analysis_Context.Get_From_File ("foo");
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Context:Get_From_Buffer (String)");
   begin
      Dummy_Unit := No_Analysis_Context.Get_From_Buffer ("foo", Buffer => "");
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Context:Get_From_Buffer (Unbounded_String)");
   begin
      Dummy_Unit := No_Analysis_Context.Get_From_Buffer
        ("foo", Buffer => Null_Unbounded_String);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Context:Get_With_Error");
   begin
      Dummy_Unit := No_Analysis_Context.Get_With_Error ("foo", "some error");
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Context:Unit_Provider");
   begin
      declare
         Dummy : constant Unit_Provider_Reference :=
            No_Analysis_Context.Unit_Provider;
      begin
         Put_Line ("No exception");
      end;
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Context:Has_With_Trivia");
   begin
      Dummy_Bool := No_Analysis_Context.Has_With_Trivia;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Context:Discard_Errors_In_Populate_Lexical_Env");
   begin
      No_Analysis_Context.Discard_Errors_In_Populate_Lexical_Env (True);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Context:Set_Logic_Resolution_Timeout");
   begin
      No_Analysis_Context.Set_Logic_Resolution_Timeout (0);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Context:Has_Rewriting_Handle");
   begin
      Dummy_Bool := No_Analysis_Context.Has_Rewriting_Handle;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Context");
   begin
      declare
         Dummy : constant Analysis_Context := No_Analysis_Unit.Context;
      begin
         Put_Line ("No exception");
      end;
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Reparse (None)");
   begin
      No_Analysis_Unit.Reparse;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Reparse (String)");
   begin
      No_Analysis_Unit.Reparse (Buffer => "foo");
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Populate_Lexical_Env");
   begin
      No_Analysis_Unit.Populate_Lexical_Env;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Get_Filename");
   begin
      Put_Line ("No exception: " & No_Analysis_Unit.Get_Filename);
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Get_Charset");
   begin
      Put_Line ("No exception: " & No_Analysis_Unit.Get_Charset);
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Has_Diagnostics");
   begin
      Dummy_Bool := No_Analysis_Unit.Has_Diagnostics;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Diagnostics");
   begin
      declare
         Dummy : constant Diagnostics_Array := No_Analysis_Unit.Diagnostics;
      begin
         Put_Line ("No exception");
      end;
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Format_GNU_Diagnostic");
   declare
      D : constant Diagnostic :=
        (Sloc_Range => (1, 1, 1, 1),
         Message    => To_Unbounded_Text (""));
   begin
      Put_Line ("No exception: " & No_Analysis_Unit.Format_GNU_Diagnostic (D));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Root");
   begin
      Dummy_Node := No_Analysis_Unit.Root;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:First_Token");
   begin
      Dummy_Token := No_Analysis_Unit.First_Token;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Last_Token");
   begin
      Dummy_Token := No_Analysis_Unit.Last_Token;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Token_Count");
   begin
      Dummy_Int := No_Analysis_Unit.Token_Count;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Trivia_Count");
   begin
      Dummy_Int := No_Analysis_Unit.Trivia_Count;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Text");
   begin
      Put_Line ("No exception: " & Image (No_Analysis_Unit.Text));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Lookup_Token");
   begin
      Dummy_Token := No_Analysis_Unit.Lookup_Token ((1, 1));
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Get_Line");
   begin
      Put_Line ("No exception: " & Image (No_Analysis_Unit.Get_Line (1)));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Dump_Lexical_Env");
   begin
      No_Analysis_Unit.Dump_Lexical_Env;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:Print");
   begin
      No_Analysis_Unit.Print;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Unit:PP_Trivia");
   begin
      No_Analysis_Unit.PP_Trivia;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Is_Token_Node");
   begin
      Dummy_Bool := No_Foo_Node.Is_Token_Node;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Is_Synthetic");
   begin
      Dummy_Bool := No_Foo_Node.Is_Synthetic;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Kind");
   declare
      Dummy : Foo_Node_Kind_Type;
   begin
      Dummy := No_Foo_Node.Kind;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Kind_Name");
   begin
      Put_Line ("No exception: " & No_Foo_Node.Kind_Name);
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Children_Count");
   begin
      Put_Line ("No exception: " & No_Foo_Node.Children_Count'Image);
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:First_Child_Index");
   begin
      Put_Line ("No exception: " & No_Foo_Node.First_Child_Index'Image);
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Last_Child_Index");
   begin
      Put_Line ("No exception: " & No_Foo_Node.Last_Child_Index'Image);
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:First_Child");
   begin
      Dummy_Node := No_Foo_Node.First_Child;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Last_Child");
   begin
      Dummy_Node := No_Foo_Node.Last_Child;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Get_Child");
   begin
      No_Foo_Node.Get_Child (1, Dummy_Bool, Dummy_Node);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Child");
   begin
      Dummy_Node := No_Foo_Node.Child (1);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Sloc_Range");
   begin
      Put_Line ("No exception: " & Image (No_Foo_Node.Sloc_Range));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Compare");
   begin
      Put_Line ("No exception: " & No_Foo_Node.Compare ((1, 1))'Image);
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Lookup");
   begin
      Dummy_Node := No_Foo_Node.Lookup ((1, 1));
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Text");
   begin
      Put_Line ("No exception: " & Image (No_Foo_Node.Text));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Token_Range");
   declare
      Dummy : Token_Iterator;
   begin
      Dummy := No_Foo_Node.Token_Range;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Print");
   declare
   begin
      No_Foo_Node.Print;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:PP_Trivia");
   declare
   begin
      No_Foo_Node.PP_Trivia;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Traverse");
   declare
      function Process (Dummy : Foo_Node'Class) return Visit_Status
      is (raise Program_Error);
   begin
      No_Foo_Node.Traverse (Process'Access);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Assign_Names_To_Logic_Vars");
   declare
   begin
      No_Foo_Node.Assign_Names_To_Logic_Vars;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Children_And_Trivia");
   begin
      declare
         Dummy : constant Children_Array := No_Foo_Node.Children_And_Trivia;
      begin
         Put_Line ("No exception");
      end;
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("List:List_Child");
   declare
      Dummy : Decl;
   begin
      Dummy := No_Decl_List.List_Child (1);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("List:List_First");
   begin
      Dummy_Int := No_Decl_List.Decl_List_First;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("List:List_Next");
   begin
      Dummy_Int := No_Decl_List.Decl_List_Next (1);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("List:List_Has_Element");
   begin
      Dummy_Bool := No_Decl_List.Decl_List_Has_Element (1);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("List:List_Element");
   begin
      declare
         Dummy : constant Decl'Class := No_Decl_List.Decl_List_Element (1);
      begin
         Put_Line ("No exception");
      end;
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Field access");
   declare
      Dummy : Identifier;
   begin
      Dummy := No_Decl.F_Name;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Node:Property call");
   begin
      Dummy_Bool := No_Foo_Node.P_Prop;
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Token_Reference:Get_Symbol");
   declare
      Dummy : Symbol_Type;
   begin
      Dummy := Get_Symbol (No_Token);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Token_Reference:Data");
   declare
      Dummy : Token_Data_Type;
   begin
      Dummy := Data (No_Token);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Token_Reference:Text");
   begin
      Put_Line ("No exception: " & Image (Text (No_Token)));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Token_Reference:Text (Single)");
   begin
      Put_Line ("No exception: " & Image (Text (No_Token)));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Token_Reference:Text (Range)");
   begin
      Put_Line ("No exception: " & Image (Text (No_Token, No_Token)));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Token_Reference:Origin_Filename");
   begin
      Put_Line ("No exception: " & Origin_Filename (No_Token));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Token_Reference:Origin_Charset");
   begin
      Put_Line ("No exception: " & Origin_Charset (No_Token));
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Token_Reference:Raw_Data");
   declare
      Dummy : Stored_Token_Data;
   begin
      Dummy := Raw_Data (No_Token);
      Put_Line ("No exception");
   exception
      when Exc : Precondition_Failure =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("main.adb: Done");
end Main;
