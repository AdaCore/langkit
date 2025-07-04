## vim: filetype=makoada

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with System;

with GNATCOLL.Iconv;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Conversions;

with ${ada_lib_name}.Generic_API;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Lexer_Implementation;
use ${ada_lib_name}.Lexer_Implementation;
with ${ada_lib_name}.Private_Converters;

% if emitter.coverage:
with Ada.Environment_Variables;

with Interfaces.C;

with GNATcov_RTS.Buffers.Lists.${ada_lib_name};
with GNATcov_RTS.Traces.Output.Files;
% endif

package body ${ada_lib_name}.Common is

   Is_Token_Node_Kind : constant array (${T.node_kind}) of Boolean :=
   ${ada_block_with_parens(
       [
           f"{n.ada_kind_name} => {n.is_token_node}"
           for n in ctx.node_types
           if not n.abstract
       ],
       3
   )};
   --  For each node kind, return whether it is a node that contains only a
   --  single token.

   Is_Error_Node_Kind : constant array (${T.node_kind}) of Boolean :=
   ${ada_block_with_parens(
       [
           f"{n.ada_kind_name} => {n.is_error_node}"
           for n in ctx.node_types
           if not n.abstract
       ],
       3
   )};
   --  For each node kind, return whether it is an error node

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference;
   function Get_Token_Context (Token : Token_Reference) return Internal_Context;
   function Get_Token_Unit (Token : Token_Reference) return Internal_Unit;
   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access;
   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index;
   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural);
   --  Implementations for converters soft-links

   function From_Generic (Token : Lk_Token) return Common.Token_Reference
     with Export, External_Name => "${ada_lib_name}__from_generic_token";
   function To_Generic (Token : Common.Token_Reference) return Lk_Token
     with Export, External_Name => "${ada_lib_name}__to_generic_token";
   --  Implementation for converters hard-links in Private_Converters

   function "+" is new Ada.Unchecked_Conversion
     (Langkit_Support.Internal.Analysis.Internal_Context, Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, Langkit_Support.Internal.Analysis.Internal_Context);

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference;
   --  Create a token reference for ``Index`` using the token data handler
   --  reference from ``Origin``.

   Token_Kind_To_Literals : constant array (Token_Kind) of Text_Access := (
   <% already_seen_set = set() %>

   % for lit, tok in ctx.lexer.literals_map.items():
      ## It's more user-friendly to represent the newline token by name rather
      ## than by escape sequence in user messages.
      % if tok.ada_name not in already_seen_set and '\n' not in lit:
         ${tok.ada_name} => new Text_Type'("${lit}"),
         <% already_seen_set.add(tok.ada_name) %>
      % endif
   % endfor
      others => new Text_Type'("")
   );

   Token_Kind_Names : constant array (Token_Kind) of String_Access := (
      % for tok in ctx.lexer.tokens:
          ${tok.ada_name} =>
             new String'("${tok.name}")
          % if (not loop.last):
              ,
          % endif
      % endfor
   );

   ---------------------
   -- Token_Kind_Name --
   ---------------------

   function Token_Kind_Name (Token_Id : Token_Kind) return String is
     (Token_Kind_Names (Token_Id).all);

   ------------------------
   -- Token_Kind_Literal --
   ------------------------

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type is
     (Token_Kind_To_Literals (Token_Id).all);

   -----------------------
   -- Token_Error_Image --
   -----------------------

   function Token_Error_Image (Token_Id : Token_Kind) return String is
      Literal : constant Text_Type := Token_Kind_Literal (Token_Id);
   begin
      return (if Literal /= ""
              then "'" & Image (Literal) & "'"
              else Token_Kind_Name (Token_Id));
   end Token_Error_Image;

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
   is (Token_Kind'Val (Raw));

   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
   is (Token_Kind'Pos (Kind));

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Kind : ${T.node_kind}) return Boolean is
   begin
      return Is_Token_Node_Kind (Kind);
   end Is_Token_Node;

   -------------------
   -- Is_Error_Node --
   -------------------

   function Is_Error_Node (Kind : ${T.node_kind}) return Boolean is
   begin
      return Is_Error_Node_Kind (Kind);
   end Is_Error_Node;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Kind : ${T.node_kind}) return Boolean is
   begin
      % if ctx.generic_list_type.concrete_subclasses:
         return ${(
            'Kind in {}'.format(ctx.generic_list_type.ada_kind_range_name)
         )};
      % else:
         pragma Unreferenced (Kind);
         return False;
      % endif
   end Is_List_Node;

   ------------------
   -- Rewrap_Token --
   ------------------

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference is
   begin
      return (if Index = No_Token_Or_Trivia_Index
              then No_Token
              else (Origin.TDH, Index, Origin.Safety_Net));
   end Rewrap_Token;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Token_Reference) is
      SN  : Token_Safety_Net renames Self.Safety_Net;
      Ctx : constant Internal_Context := +SN.Context;
   begin
      if Self.TDH /= null
         and then (Ctx.Serial_Number /= SN.Context_Version
                   or else Self.TDH.Version /= SN.TDH_Version)
      then
         raise Stale_Reference_Error;
      end if;
   end Check_Safety_Net;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Token_Reference) return Boolean is
      pragma Assert (Left.TDH = Right.TDH);
   begin
      Check_Safety_Net (Left);
      Check_Safety_Net (Right);
      if Left.Index.Token < Right.Index.Token then
         return True;

      elsif Left.Index.Token = Right.Index.Token then
         return Left.Index.Trivia < Right.Index.Trivia;

      else
         return False;
      end if;
   end "<";

   ----------
   -- Next --
   ----------

   function Next
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Next (Token.Index, Token.TDH.all,
                                       Exclude_Trivia)));
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Previous (Token.Index, Token.TDH.all,
                                           Exclude_Trivia)));
   end Previous;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Token : Token_Reference) return Symbol_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Get_Symbol (Token.Index, Token.TDH.all);
   end Get_Symbol;

   ----------
   -- Data --
   ----------

   function Data (Token : Token_Reference) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Convert (Token.TDH.all, Token, Raw_Data (Token));
   end Data;

   ----------
   -- Text --
   ----------

   function Text (Token : Token_Reference) return Text_Type is
      RD : constant Stored_Token_Data := Raw_Data (Token);
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Token.TDH.Source_Buffer (RD.Source_First .. RD.Source_Last);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (First, Last : Token_Reference) return Text_Type is
      FD, LD : Token_Data_Type;
   begin
      Check_Safety_Net (First);
      Check_Safety_Net (Last);
      if First.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      if First.TDH /= Last.TDH then
         raise Precondition_Failure with
            "token arguments must belong to the same source";
      end if;
      FD := Data (First);
      LD := Data (Last);
      return FD.Source_Buffer.all (FD.Source_First .. LD.Source_Last);
   end Text;

   ----------
   -- Kind --
   ----------

   function Kind (Token_Data : Token_Data_Type) return Token_Kind is
   begin
      return Token_Data.Kind;
   end Kind;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Token);
      return Token.Index.Trivia /= No_Token_Index;
   end Is_Trivia;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean is
   begin
      return Token_Data.Is_Trivia;
   end Is_Trivia;

   -----------
   -- Index --
   -----------

   function Index (Token : Token_Reference) return Token_Index is
   begin
      Check_Safety_Net (Token);
      return (if Token.Index.Trivia = No_Token_Index
              then Token.Index.Token
              else Token.Index.Trivia);
   end Index;

   -----------
   -- Index --
   -----------

   function Index (Token_Data : Token_Data_Type) return Token_Index is
   begin
      return Token_Data.Index;
   end Index;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range
   is
   begin
      return Token_Data.Sloc_Range;
   end Sloc_Range;

   ---------------------
   -- Origin_Filename --
   ---------------------

   function Origin_Filename (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return +Token.TDH.Filename.Full_Name;
   end Origin_Filename;

   --------------------
   -- Origin_Charset --
   --------------------

   function Origin_Charset (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return To_String (Token.TDH.Charset);
   end Origin_Charset;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Token_Reference) return Boolean is
      DL : constant Stored_Token_Data := Raw_Data (L);
      DR : constant Stored_Token_Data := Raw_Data (R);
   begin
      --  Two tokens with different kinds are never equivalent

      if DL.Kind /= DR.Kind then
         return False;
      end if;

      --  Depending on the token kind involved, the equivalence considers
      --  different token attributes: just the kind, the symbol or the actual
      --  token text.

      <%
         check_kind = []
         check_symbol = []
         check_text = []

         for t in ctx.lexer.sorted_tokens:
             if not t.matched_by_pattern:
                 check_kind.append(t)
             elif t.is_symbolized:
                 check_symbol.append(t)
             else:
                 check_text.append(t)

         def ada_matcher(tokens):
             return " | ".join(t.ada_name for t in tokens)
      %>
      case To_Token_Kind (DL.Kind) is
         % if check_kind:
            when ${ada_matcher(check_kind)} =>
               return True;
         % endif

         % if check_symbol:
            when ${ada_matcher(check_symbol)} =>

               --  Comparing the symbol reference itself is invalid when L and
               --  R belong to two different contexts (the two symbol
               --  references designate symbols in different symbol tables):
               --  compare the texts behind symbols for such cases.

               return (if L.TDH.Symbols = R.TDH.Symbols
                       then DL.Symbol = DR.Symbol
                       else Get (L.TDH.Symbols, DL.Symbol).all
                            = Get (R.TDH.Symbols, DR.Symbol).all);
         % endif

         % if check_text:
            when ${ada_matcher(check_text)} =>
               declare
                  TL : Text_Type renames
                    L.TDH.Source_Buffer (DL.Source_First .. DL.Source_Last);
                  TR : Text_Type renames
                    R.TDH.Source_Buffer (DR.Source_First .. DR.Source_Last);
               begin
                  return TL = TR;
               end;
         % endif
      end case;
   end Is_Equivalent;

   -----------
   -- Image --
   -----------

   function Image (Token : Token_Reference) return String is
      D : constant Token_Data_Type := Data (Token);
   begin
      return ("<Token Kind=" & Token_Kind_Name (D.Kind) &
              " Text=" & Image (Text (Token), With_Quotes => True) & ">");
   end Image;

   --------------
   -- Raw_Data --
   --------------

   function Raw_Data (T : Token_Reference) return Stored_Token_Data is
   begin
      Check_Safety_Net (T);
      if T.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return
        (if T.Index.Trivia = No_Token_Index
         then Token_Vectors.Get (T.TDH.Tokens, Natural (T.Index.Token))
         else Trivia_Vectors.Get (T.TDH.Trivias, Natural (T.Index.Trivia)).T);
   end Raw_Data;

   -------------
   -- Convert --
   -------------

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      return (Kind          => To_Token_Kind (Raw_Data.Kind),
              Is_Trivia     => Token.Index.Trivia /= No_Token_Index,
              Index         => (if Token.Index.Trivia = No_Token_Index
                                then Token.Index.Token
                                else Token.Index.Trivia),
              Source_Buffer => Text_Cst_Access (TDH.Source_Buffer),
              Source_First  => Raw_Data.Source_First,
              Source_Last   => Raw_Data.Source_Last,
              Sloc_Range    => Sloc_Range (TDH, Raw_Data));
   end Convert;

   ------------------
   -- From_Generic --
   ------------------

   function From_Generic (Token : Lk_Token) return Common.Token_Reference is
      use Langkit_Support.Internal.Conversions;
      Id         : Any_Language_Id;
      Data       : Langkit_Support.Internal.Analysis.Internal_Token;
      Safety_Net : Langkit_Support.Internal.Analysis.Token_Safety_Net;
   begin
      Unwrap_Token (Token, Id, Data, Safety_Net);
      pragma Assert (Id = Generic_API.Self_Id);
      return (Data.TDH,
              Data.Index,
              (Safety_Net.Context,
               Safety_Net.Context_Version,
               Safety_Net.TDH_Version));
   end From_Generic;

   ----------------
   -- To_Generic --
   ----------------

   function To_Generic (Token : Common.Token_Reference) return Lk_Token is
      use Langkit_Support.Internal.Conversions;
   begin
      return Wrap_Token
        (Generic_API.Self_Id,
         (Token.TDH, Token.Index),
         (Token.Safety_Net.Context,
          Token.Safety_Net.Context_Version,
          Token.Safety_Net.TDH_Version));
   end To_Generic;

   --------------------------
   -- Wrap_Token_Reference --
   --------------------------

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference is
   begin
      if Index = No_Token_Or_Trivia_Index then
         return No_Token;
      end if;

      declare
         SN : constant Token_Safety_Net :=
           (Context         => +Context,
            Context_Version => Context.Serial_Number,
            TDH_Version     => TDH.Version);
      begin
        return (TDH, Index, SN);
      end;
   end Wrap_Token_Reference;

   --------------------
   -- Get_Token_Unit --
   --------------------

   function Get_Token_Unit (Token : Token_Reference) return Internal_Unit is
      function "+" is new Ada.Unchecked_Conversion
        (System.Address, Internal_Unit);
   begin
      if Token = No_Token then
         raise Precondition_Failure with "null token argument";
      end if;
      Check_Safety_Net (Token);
      return +Token.TDH.Owner;
   end Get_Token_Unit;

   -----------------------
   -- Get_Token_Context --
   -----------------------

   function Get_Token_Context
     (Token : Token_Reference) return Internal_Context is
   begin
      return +Token.Safety_Net.Context;
   end Get_Token_Context;

   -------------------
   -- Get_Token_TDH --
   -------------------

   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access is
   begin
      return Token.TDH;
   end Get_Token_TDH;

   ---------------------
   -- Get_Token_Index --
   ---------------------

   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index is
   begin
      return Token.Index;
   end Get_Token_Index;

   ------------------------
   -- Extract_Token_Text --
   ------------------------

   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural) is
   begin
      Source_Buffer := Token.Source_Buffer;
      First := Token.Source_First;
      Last := Token.Source_Last;
   end Extract_Token_Text;

   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind (Kind : ${T.node_kind}) return Token_Kind is
      <% token_nodes = [n for n in ctx.node_types
                        if not n.abstract and n.is_token_node] %>
   begin
      % if ctx.generate_unparsers:
         case Kind is
            % for n in token_nodes:
               when ${n.ada_kind_name} =>
                  return ${n.token_kind.ada_name};
            % endfor

            when others =>
               --  Kind is not a token node, and thus the precondition does not
               --  hold.
               return (raise Program_Error);
         end case;

      % else:
         pragma Unreferenced (Kind);
         return (raise Program_Error);
      % endif
   end Token_Node_Kind;

   % if emitter.coverage:
      type Atexit_Callback is access procedure with Convention => C;

      function atexit (Callback : Atexit_Callback) return Interfaces.C.int
         with Import, Convention => C;

      procedure Dump_Source_Trace with Convention => C;
      --  Output a source trace file for all the coverage state of
      --  ${ada_lib_name}.

      -----------------------
      -- Dump_Source_Trace --
      -----------------------

      procedure Dump_Source_Trace is
         package Env renames Ada.Environment_Variables;

         --  If the $_TRACE_FILE environment variable is defined, use its
         --  content as the source trace filename. Otherwise, default to
         --  GNATcov_RTS.Traces.Output's naming scheme.

         <% env_var = '{}_TRACE_FILE'.format(emitter.lib_name_up) %>
         Filename : constant String :=
           (if Env.Exists ("${env_var}")
            then Env.Value ("${env_var}")
            else "");
      begin
         GNATcov_RTS.Traces.Output.Files.Write_Trace_File
           (Buffers  => GNATcov_RTS.Buffers.Lists.${ada_lib_name}.List,
            Filename => Filename);
      end Dump_Source_Trace;

   % endif

begin
   --  Check that we actually have full Libiconv support: as nothing works
   --  without it, we explicitly check support here instead of letting
   --  user-unfriendly errors happen during lexing.

   if not GNATCOLL.Iconv.Has_Iconv then
      raise Program_Error with "Libiconv is not available";
   end if;

   % if emitter.coverage:
      --  Register a call to Dump_Source_Trace on process exit
      declare
         Dummy : Interfaces.C.int := atexit (Dump_Source_Trace'Access);
      begin
         null;
      end;
   % endif

   Private_Converters.Wrap_Token_Reference := Wrap_Token_Reference'Access;
   Private_Converters.Get_Token_Context := Get_Token_Context'Access;
   Private_Converters.Get_Token_Unit := Get_Token_Unit'Access;
   Private_Converters.Get_Token_TDH := Get_Token_TDH'Access;
   Private_Converters.Get_Token_Index := Get_Token_Index'Access;
   Private_Converters.Extract_Token_Text := Extract_Token_Text'Access;
end ${ada_lib_name}.Common;
