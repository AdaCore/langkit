with Langkit_Support.Text;         use Langkit_Support.Text;
with Libfoolang.Analysis; use Libfoolang.Analysis;

package Support is
   type Event_Handler is new Libfoolang.Analysis.Event_Handler_Interface
   with record
      null;
   end record;

   overriding procedure Unit_Requested_Callback
     (Self               : Event_Handler;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);

   overriding procedure Unit_Parsed_Callback
     (Self     : Event_Handler;
      Context  : Analysis_Context'Class;
      Unit     : Analysis_Unit'Class;
      Reparsed : Boolean);

   overriding procedure Release (Self : in out Event_Handler);

   procedure Dump (Unit : Analysis_Unit);
end Support;
