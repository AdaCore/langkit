with Ada.Assertions; use Ada.Assertions;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Liblktlang.Analysis;          use Liblktlang.Analysis;
with Liblktlang.Prelude;
with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;

package body Liblktlang.Implementation.Extensions is

   ----------------------------------
   -- Langkit_Root_P_Fetch_Prelude --
   ----------------------------------

   function Langkit_Root_P_Fetch_Prelude
     (Node : Bare_Langkit_Root) return Boolean
   is
      Ctx     : constant Analysis_Context := Wrap_Context (Node.Unit.Context);
      Prelude : Analysis_Unit;
   begin
      Prelude := Ctx.Get_From_File ("__prelude");
      if Prelude.Root = No_Lkt_Node then
         Prelude := Ctx.Get_From_Buffer
           ("__prelude", "ascii", Liblktlang.Prelude.Content);

         --  Check if we have syntactic or semantic errors in the prelude. If
         --  we do, raise an assertion error.

         if Prelude.Diagnostics'Length > 0 then
            for Diagnostic of Prelude.Diagnostics loop
               Put_Line (To_Pretty_String (Diagnostic));
            end loop;
            raise Assertion_Error with "Errors in prelude";
         end if;

         declare
            Errors : constant Semantic_Result_Array :=
              Prelude.Root.As_Langkit_Root.P_Check_Legality;
         begin
            if Errors'Length > 0 then
               for R of Errors loop
                  Put_Line
                    (Image (Analysis.Node (R).Full_Sloc_Image
                            & Error_Message (R)));
               end loop;
               raise Assertion_Error with "Errors in prelude";
            end if;
         end;

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
      Full_Name_Acc : String_Type := Dispatcher_Decl_P_Full_Name (Node);
      Full_Name     : constant Text_Type := Full_Name_Acc.Content;
      File_Name     : constant Text_Type :=
         To_Text (Ada.Directories.Simple_Name (Get_Filename (Unit (Node))));
   begin
      Dec_Ref (Full_Name_Acc);
      if File_Name = "__prelude" then
         return "<" & To_Text (Kind_Name (Node))
           & " prelude: """ & Full_Name & """>";
      else
         return "<" & To_Text (Kind_Name (Node)) & " """ & Full_Name & """ "
           & File_Name

           --  Don't show the sloc for function types, because it will be the
           --  root node's sloc, and thus will always change when we add stuff
           --  to the file, which is not helpful nor practical for tests.
           & (if Node.Kind = Lkt_Function_Type
              then ""
              else ":" & To_Text (Image (Sloc_Range (Node)))) & ">";
      end if;
   end Decl_Short_Image;

   ---------------------------------------
   -- Lkt_Node_P_Env_From_Vals_Internal --
   ---------------------------------------

   function Lkt_Node_P_Env_From_Vals_Internal
     (Node : Bare_Lkt_Node;
      Vals : Internal_EnvKV_Array_Access) return Lexical_Env
   is
      Ret : constant Lexical_Env :=
         Create_Static_Lexical_Env (Null_Lexical_Env, Node);
   begin

      for El of Vals.Items loop
         AST_Envs.Add (Ret, El.Key, El.Value);
      end loop;

      return Ret;
   end Lkt_Node_P_Env_From_Vals_Internal;

   -----------------------------------------------
   -- Lkt_Node_P_Internal_Fetch_Referenced_Unit --
   -----------------------------------------------

   function Lkt_Node_P_Internal_Fetch_Referenced_Unit
     (Node : Bare_Lkt_Node; Name : String_Type) return Internal_Unit
   is
      I : constant Internal_Unit := Get_From_Provider
        (Context => Node.Unit.Context,
         Name    => Name.Content,
         Kind    => Unit_Body,
         Charset => "ascii",
         Reparse => False);
   begin
      Populate_Lexical_Env (Wrap_Unit (I));
      return I;
   end Lkt_Node_P_Internal_Fetch_Referenced_Unit;

   -------------------------------------
   -- String_Lit_P_Is_Prefixed_String --
   -------------------------------------

   function String_Lit_P_Is_Prefixed_String
     (Node : Bare_String_Lit) return Boolean
   is
      Tok_Kind : constant Token_Kind :=
         Kind (Data (Token (Node, Node.Token_Start_Index)));
   begin
      return Tok_Kind = Lkt_P_String;
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
     (Node : Bare_String_Lit) return String_Type
   is
      N_Text : constant Text_Type := Text (Node);
   begin
      --  TODO: handle escape sequences

      return Create_String
        (if String_Lit_P_Is_Prefixed_String (Node) then
            N_Text (N_Text'First + 2 .. N_Text'Last - 1)
         else
            N_Text (N_Text'First + 1 .. N_Text'Last - 1));
   end String_Lit_P_Denoted_Value;

end Liblktlang.Implementation.Extensions;
