with Support;

package Helpers_A is

   type Text_Type is null record;
   --  Attempts to add "use Helpers_A;" in Libfoolang.Analysis will create
   --  compilation errors (conflict with Langkit_Support.Text.Text_Type).

   procedure Say_Hello is new Support.Say_Hello ("A");

end Helpers_A;
