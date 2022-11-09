with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;

package Support is

   type Event_Handler is new Libfoolang.Analysis.Event_Handler_Interface
   with record
      Label : Unbounded_String;
   end record;

   overriding procedure Unit_Requested_Callback
     (Self               : in out Event_Handler;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);

   overriding procedure Unit_Parsed_Callback
     (Self     : in out Event_Handler;
      Context  : Analysis_Context'Class;
      Unit     : Analysis_Unit'Class;
      Reparsed : Boolean);

   overriding procedure Release (Self : in out Event_Handler);

   function Create_Event_Handler
     (Label : String) return Event_Handler_Reference;

   procedure Log (Self : Event_Handler; Subp : String);

end Support;
