## vim: filetype=makoada

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with ${ada_lib_name}.Lexer; use ${ada_lib_name}.Lexer;

package body ${ada_lib_name}.Debug is

   --------
   -- PN --
   --------

   procedure PN (Node : ${root_node_type_name}) is
   begin
      Put_Line (Image (Node.Short_Image));
   end PN;

   --------
   -- PT --
   --------

   procedure PT (Node : ${root_node_type_name}) is
   begin
      Node.Print;
   end PT;

   ----------
   -- PTok --
   ----------

   procedure PTok (TDH : Token_Data_Handler_Access; T : Token_Index) is
      Index : constant Natural := Natural (T);
   begin
      if Index not in TDH.Tokens.First_Index .. TDH.Tokens.Last_Index then
         Put_Line ("<invalid token>");

      else
         declare
            D : constant Lexer.Token_Data_Type := TDH.Tokens.Get (Index);
         begin
            Put (Token_Kind_Name (D.Kind));
            Put (" " & Image (Text (TDH.all, D), With_Quotes => True));
            Put_Line (" [" & Image (D.Sloc_Range) & "]");
         end;
      end if;
   end PTok;

   ----------
   -- PEnv --
   ----------

   procedure PEnv (Env : Lexical_Env) is
   begin
      Dump_Lexical_Env_Parent_Chain (Env);
   end PEnv;

   -----------------
   -- Sym_Matches --
   -----------------

   function Sym_Matches (S : Symbol_Type; Text : String) return Boolean is
   begin
      return Image (S.all) = Text;
   end Sym_Matches;

end ${ada_lib_name}.Debug;
