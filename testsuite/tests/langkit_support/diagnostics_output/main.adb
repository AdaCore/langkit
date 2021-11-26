--  Test the behavior of the ``Langkit_Support.Diagnostic.Output`` module, and
--  more specifically of the ``Print_Diagnostic`` procedure.

with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Diagnostics.Output;
use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Text;    use Langkit_Support.Text;


procedure Main is

   type Lines_Array is array (Positive range <>) of Unbounded_Text_Type;

   type Simple_Buffer (Size : Positive) is new Text_Buffer_Ifc with record
      Lines : Lines_Array (1 .. Size);
   end record;

   overriding function Get_Line
     (Self : Simple_Buffer; Line_Number : Positive) return Text_Type
   is
   begin
      return To_Text (Self.Lines (Line_Number));
   end;

   B : Simple_Buffer :=
     (Size  => 1,
      Lines => (1 => To_Unbounded_Text ("A simple line")));

begin
   Put_Line ("Regular diagnostic case");
   Output.Print_Diagnostic
     (Diagnostic'
       (Sloc_Range => Source_Location_Range'(1, 1, 3, 9),
        Message    => To_Unbounded_Text ("Test message")),
      B, "main.adb");

   Put_Line ("Beginning of line no range message");
   Output.Print_Diagnostic
     (Diagnostic'
       (Sloc_Range => Source_Location_Range'(1, 1, 1, 1),
        Message    => To_Unbounded_Text ("Test message")),
      B, "main.adb");
end Main;
