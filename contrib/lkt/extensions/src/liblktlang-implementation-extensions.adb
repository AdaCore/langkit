with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Liblktlang.Analysis;          use Liblktlang.Analysis;
with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;

package body Liblktlang.Implementation.Extensions is

   Prelude_Content : String :=
     "@builtin struct Int {}" & ASCII.LF &
     "@builtin struct BigInt {}" & ASCII.LF &
     "@builtin struct String {}" & ASCII.LF &
     "@builtin struct Symbol {}" & ASCII.LF &
     "@builtin struct Regexp {}" & ASCII.LF &
     "@builtin enum Bool (false, true) {}" & ASCII.LF &
     "@builtin generic[T] struct Array {" & ASCII.LF &
     "    fun __call__(index : Int): T" & ASCII.LF &
     "    fun length(): int" & ASCII.LF &
     "}" & ASCII.LF &
     "@builtin generic[T] struct ASTList {" & ASCII.LF &
     "}" & ASCII.LF &
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

   ------------------------
   -- Ref_Id_Short_Image --
   ------------------------

   function Ref_Id_Short_Image (Node : Bare_Ref_Id) return Text_Type is
   begin
      return
        "<" & To_Text (Kind_Name (Node))
        & " """ & Text (Node) & """ "
        & To_Text (Ada.Directories.Simple_Name (Get_Filename (Unit (Node))))
        & ":" & To_Text (Image (Sloc_Range (Node))) & ">";
   end Ref_Id_Short_Image;

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

   ----------------------------------------------
   -- LK_Node_P_Internal_Fetch_Referenced_Unit --
   ----------------------------------------------

   function LK_Node_P_Internal_Fetch_Referenced_Unit
     (Node : Bare_LK_Node;
      Name : Character_Type_Array_Access) return Internal_Unit
   is
   begin
      return Get_From_Provider
        (Context => Node.Unit.Context,
         Name    => Name.Items,
         Kind    => Unit_Body,
         Charset => "ascii",
         Reparse => False);
   end LK_Node_P_Internal_Fetch_Referenced_Unit;

   -------------------------------------
   -- String_Lit_P_Is_Prefixed_String --
   -------------------------------------

   function String_Lit_P_Is_Prefixed_String
     (Node : Bare_String_Lit) return Boolean
   is
      Tok_Kind : constant Token_Kind :=
         Kind (Data (Token (Node, Node.Token_Start_Index)));
   begin
      return Tok_Kind = lkt_P_String;
   end String_Lit_P_Is_Prefixed_String;

   -------------------------
   -- String_Lit_P_Prefix --
   -------------------------

   function String_Lit_P_Prefix
     (Node : Bare_String_Lit) return Character_Type
   is
      N_Text : constant Text_Type := Text (Node);
   begin
      return
        (if String_Lit_P_Is_Prefixed_String (Node) then
            N_Text (N_Text'First)
         else
            Character_Type'Val (0));
   end String_Lit_P_Prefix;

   --------------------------------
   -- String_Lit_P_Denoted_Value --
   --------------------------------

   function String_Lit_P_Denoted_Value
     (Node : Bare_String_Lit) return Character_Type_Array_Access
   is
      N_Text : constant Text_Type := Text (Node);
   begin
      return Create_Character_Type_Array
        (if String_Lit_P_Is_Prefixed_String (Node) then
            N_Text (N_Text'First + 2 .. N_Text'Last - 1)
         else
            N_Text (N_Text'First + 1 .. N_Text'Last - 1));
   end String_Lit_P_Denoted_Value;

end Liblktlang.Implementation.Extensions;
