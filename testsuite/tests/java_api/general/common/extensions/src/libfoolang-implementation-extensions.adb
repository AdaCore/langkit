with Langkit_Support.Text; use Langkit_Support.Text;

package body Libfoolang.Implementation.Extensions is

   ---------------------------------------
   -- Foo_Node_P_Trigger_Unit_Requested --
   ---------------------------------------

   function Foo_Node_P_Trigger_Unit_Requested
     (Node  : Bare_Foo_Node;
      Name  : Symbol_Type;
      Found : Boolean;
      Error : Boolean) return Boolean
   is
      EH : constant Internal_Event_Handler_Access :=
        Node.Unit.Context.Event_Handler;
   begin
      if EH /= null then
         EH.Unit_Requested_Callback
           (Context            => Node.Unit.Context,
            Name               => +Name,
            From               => Node.Unit,
            Found              => Found,
            Is_Not_Found_Error => Error);
      end if;

      return False;
   end Foo_Node_P_Trigger_Unit_Requested;

   function Foo_Node_P_New_Struct_With_Inner
     (Node : Bare_Foo_Node) return Internal_With_Inner
   is
      Empty_Struct : Internal_Empty_Struct := (Dummy => ' ');
   begin
      return
        (Empty => Empty_Struct,
         Sloc  => (1, 42),
         Som   =>
           (Examples => No_Internal_Entity_Example_Array_Type));
   end Foo_Node_P_New_Struct_With_Inner;

end Libfoolang.Implementation.Extensions;
