## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with ${ada_lib_name}.Common; use ${ada_lib_name}.Common;
with ${ada_lib_name}.Lexer;  use ${ada_lib_name}.Lexer;

${(exts.with_clauses(with_clauses + [
   ((ctx.symbol_canonicalizer.unit_fqn, False, False)
    if ctx.symbol_canonicalizer else None),
]))}

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
      Node.Print (Show_Slocs => True);
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
            D : constant Stored_Token_Data := TDH.Tokens.Get (Index);
         begin
            Put (Token_Kind_Name (To_Token_Kind (D.Kind)));
            Put (" " & Image (Text (TDH.all, D), With_Quotes => True));
            Put_Line (" [" & Image (D.Sloc_Range) & "]");
         end;
      end if;
   end PTok;

   ----------
   -- PEnv --
   ----------

   procedure PEnv (Env : AST_Envs.Lexical_Env) is
   begin
      AST_Envs.Dump_Lexical_Env_Parent_Chain (Env);
   end PEnv;

   -----------------
   -- Sym_Matches --
   -----------------

   function Sym_Matches (S : Symbol_Type; Text : String) return Boolean is
      Symbol : constant Symbolization_Result :=
         % if ctx.symbol_canonicalizer:
            ${ctx.symbol_canonicalizer.fqn} (To_Text (Text))
         % else:
            Create_Symbol (To_Text (Text))
         % endif
      ;
   begin
      return Symbol.Success and then Image (S.all) = Image (Symbol.Symbol);
   end Sym_Matches;

   ----------
   -- PRel --
   ----------

   procedure PRel (Rel : Relation; Context_Node : ${root_node_type_name}) is
   begin
      Assign_Names_To_Logic_Vars (Context_Node);
      Print_Relation (Rel, null, False);
   end PRel;

end ${ada_lib_name}.Debug;
