with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Liblktlang.Analysis; use Liblktlang.Analysis;
with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;

package body Liblktlang.Implementation.Extensions is

   Prelude_Content : String :=
     "@builtin struct Int {}" & ASCII.LF &
     "@builtin struct BigInt {}" & ASCII.LF &
     "@builtin struct String {}" & ASCII.LF &
     "@builtin struct Symbol {}" & ASCII.LF &
     "@builtin enum Bool (false, true) {}" & ASCII.LF &
     "@builtin generic[T] struct Array {}" & ASCII.LF &
     "";

   ----------------------------------
   -- Langkit_Root_P_Fetch_Prelude --
   ----------------------------------

   function Langkit_Root_P_Fetch_Prelude
     (Node : Bare_Langkit_Root) return Boolean
   is
      Ctx     : Analysis_Context := Wrap_Context (Node.Unit.Context);
      Prelude : Analysis_Unit;
   begin
      Prelude := Ctx.Get_From_File ("__prelude");
      if Prelude.Root = No_LK_Node then
         Prelude := Ctx.Get_From_Buffer ("__prelude", "ascii", Prelude_Content);

         if Prelude.Diagnostics'Length > 0 then
            for Diagnostic of Prelude.Diagnostics loop
               Put_Line (To_Pretty_String (Diagnostic));
            end loop;
         end if;
         Populate_Lexical_Env (Prelude);
         return True;
      else
         return False;
      end if;
   end Langkit_Root_P_Fetch_Prelude;

   ----------------------
   -- Decl_Short_Image --
   ----------------------

   function Decl_Short_Image (Node : Bare_Decl) return Text_Type is
   begin
      return
        "<" & To_Text (Kind_Name (Node))
        & " """ & Image (Dispatcher_Decl_P_Name (Node)) & """ "
        & To_Text (Ada.Directories.Simple_Name (Get_Filename (Unit (Node))))
        & ":" & To_Text (Image (Sloc_Range (Node))) & ">";
   end Decl_Short_Image;

   --------------------------------------
   -- LK_Node_P_Env_From_Vals_Internal --
   --------------------------------------

   function LK_Node_P_Env_From_Vals_Internal
     (Node : Bare_LK_Node;
      Vals : Internal_EnvKV_Array_Access) return Lexical_Env
   is
     Ret : Lexical_Env;
   begin
      Ret := AST_Envs.Create_Lexical_Env
        (No_Env_Getter, Node, Owner => Node.Unit);
      Register_Destroyable (Node.Unit, Ret.Env);

      for El of Vals.Items loop
         AST_Envs.Add (Ret, El.Key, El.Value);
      end loop;

      return Ret;
   end LK_Node_P_Env_From_Vals_Internal;

end Liblktlang.Implementation.Extensions;
