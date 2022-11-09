with Langkit_Support.Text; use Langkit_Support.Text;

package body Libfoolang.Implementation.Extensions is

   function Foo_Node_P_Get_Unit
     (Node : Bare_Foo_Node;
      Name : Symbol_Type) return Internal_Unit
   is
      Ret : Internal_Unit :=
         Libfoolang.Implementation.Get_From_File
           (Node.Unit.Context,
            Filename => Image (Name.all & ".txt"),
            Charset  => "",
            Reparse  => False,
            Rule     => Main_Rule_Rule);
   begin
      if Node.Unit.Context.Event_Handler /= null then
         Node.Unit.Context.Event_Handler.Unit_Requested_Callback
           (Node.Unit.Context, Name.all, Node.Unit, Ret.AST_Root /= null, True);
      end if;

      return Ret;
   end Foo_Node_P_Get_Unit;

end Libfoolang.Implementation.Extensions;
