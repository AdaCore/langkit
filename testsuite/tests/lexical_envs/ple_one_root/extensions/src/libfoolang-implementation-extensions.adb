package body Libfoolang.Implementation.Extensions is

   ------------------------------------
   -- Id_P_Referenced_Scope_Or_Error --
   ------------------------------------

   function Id_P_Referenced_Scope_Or_Error
     (Node : Bare_Id; Or_Error : Boolean) return Bare_Scope
   is
      function Error (Message : String) return Bare_Scope
      is (if Or_Error
          then raise Property_Error with Message
          else null);

      Unit : constant Internal_Unit :=
        Get_From_File
          (Context  => Node.Unit.Context,
           Filename => "helper.txt",
           Charset  => "",
           Reparse  => False,
           Rule     => Default_Grammar_Rule);
      Root : constant Bare_Foo_Node := Unit.Ast_Root;
   begin
      --  For the purpose of this test, assume that the only scope we will ever
      --  have to resolve is "a". If the unit has only one element, it's that
      --  scope. If it's a list of scope, it's the second one.

      if +Get_Symbol (Node) /= "a" then
         raise Program_Error;
      end if;

      Populate_Lexical_Env (Unit, 2);
      if Root = null then
         return Error ("empty unit");
      elsif Root.Kind = Foo_Scope then
         return Root;
      elsif Root.Kind = Foo_Scope_List and then Children_Count (Root) >= 2 then
         return Child (Root, 2);
      else
         return Error ("scope not found");
      end if;
   end Id_P_Referenced_Scope_Or_Error;

end Libfoolang.Implementation.Extensions;
