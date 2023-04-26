package body Libfoolang.Implementation.Extensions is

   -------------------------------------
   -- Name_P_Referenced_Unit_Or_Error --
   -------------------------------------

   function Name_P_Referenced_Unit_Or_Error
     (Node : Bare_Name; Or_Error : Boolean) return Internal_Unit
   is
      Root           : constant Bare_Foo_Node := Node.Unit.AST_Root;
      Requested_Name : Symbol_Type_Array_Access :=
         Dispatcher_Name_P_Symbols (Node);
      Has_Errors     : Boolean := False;
   begin
      for I in 1 .. Children_Count (Root) loop
         declare
            S    : constant Bare_Foo_Node := Child (Root, I);
            SN   : constant Bare_Name := Scope_F_Name (S);
            Node : Symbol_Type_Array_Access := Dispatcher_Name_P_Symbols (SN);
         begin
            if Node.Items = Requested_Name.Items then
               Has_Errors := Populate_Lexical_Env (S) or else Has_Errors;
            end if;
            Dec_Ref (Node);
         end;
      end loop;
      Dec_Ref (Requested_Name);
      if Has_Errors then
         raise Property_Error;
      end if;
      return Node.Unit;
   end Name_P_Referenced_Unit_Or_Error;

end Libfoolang.Implementation.Extensions;
