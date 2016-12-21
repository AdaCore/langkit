## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />

<% root_node_array = T.root_node.array_type() %>

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with System.Storage_Elements;    use System.Storage_Elements;

with Langkit_Support.Extensions; use Langkit_Support.Extensions;
with Langkit_Support.Relative_Get;
with Langkit_Support.Symbols;    use Langkit_Support.Symbols;

pragma Warnings (Off, "referenced");
with Adalog.Abstract_Relation;   use Adalog.Abstract_Relation;
with Adalog.Debug;
with Adalog.Operations;          use Adalog.Operations;
with Adalog.Predicates;          use Adalog.Predicates;
with Adalog.Pure_Relations;      use Adalog.Pure_Relations;
with Adalog.Variadic_Operations; use Adalog.Variadic_Operations;
pragma Warnings (On, "referenced");

package body ${_self.ada_api_settings.lib_name}.AST is

   ${array_types.body(LexicalEnvType.array_type())}
   ${array_types.body(T.root_node.env_element().array_type())}
   ${array_types.body(root_node_array)}

   function Child_Number
     (Node : access ${root_node_value_type}'Class)
      return Positive
      with Pre => Node.Parent /= null;
   --  Return the 1-based index for Node in its parents' children

   function Lookup_Internal
     (Node : ${root_node_type_name};
      Sloc : Source_Location;
      Snap : Boolean := False) return ${root_node_type_name};
   procedure Lookup_Relative
     (Node       : ${root_node_type_name};
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out ${root_node_type_name};
      Snap       : Boolean := False);
   --  Implementation helpers for the looking up process

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Node : access ${root_node_value_type}'Class) is
   begin
      if Node = null then
         return;
      end if;

      Node.Destroy_Node;
      for Child of Node.all loop
         Destroy (Child);
      end loop;
   end Destroy;

   -----------
   -- Child --
   -----------

   function Child (Node  : access ${root_node_value_type}'Class;
                   Index : Positive) return ${root_node_type_name}
   is
      Result          : ${root_node_type_name};
      Index_In_Bounds : Boolean;
   begin
      Get_Child (Node, Index, Index_In_Bounds, Result);
      return (if Index_In_Bounds then Result else null);
   end Child;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class)
              return Visit_Status)
     return Visit_Status
   is
      Status : Visit_Status := Into;

   begin
      if Node /= null then
         Status := Visit (Node);

         --  Skip processing the child nodes if the returned status is Over
         --  or Stop. In the former case the previous call to Visit has taken
         --  care of processing the needed childs, and in the latter case we
         --  must immediately stop processing the tree.

         if Status = Into then
            for I in 1 .. Child_Count (Node) loop
               declare
                  Cur_Child : constant ${root_node_type_name} :=
                     Child (Node, I);

               begin
                  if Cur_Child /= null then
                     Status := Traverse (Cur_Child, Visit);
                     exit when Status /= Into;
                  end if;
               end;
            end loop;
         end if;
      end if;

      if Status = Stop then
         return Stop;

      --  At this stage the Over status has no sense and we just continue
      --  processing the tree.

      else
         return Into;
      end if;
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class)
                               return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   ------------------------
   -- Traverse_With_Data --
   ------------------------

   function Traverse_With_Data
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class;
                               Data : in out Data_type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status
   is
      function Helper (Node : access ${root_node_value_type}'Class)
                       return Visit_Status;

      ------------
      -- Helper --
      ------------

      function Helper (Node : access ${root_node_value_type}'Class)
                       return Visit_Status
      is
      begin
         return Visit (Node, Data);
      end Helper;

      Saved_Data : Data_Type;
      Result     : Visit_Status;

   begin
      if Reset_After_Traversal then
         Saved_Data := Data;
      end if;
      Result := Traverse (Node, Helper'Access);
      if Reset_After_Traversal then
         Data := Saved_Data;
      end if;
      return Result;
   end Traverse_With_Data;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Root : access ${root_node_value_type}'Class)
      return Traverse_Iterator
   is
   begin
      return Create (Root);
   end Traverse;

   ----------
   -- Next --
   ----------

   function Next (It       : in out Find_Iterator;
                  Element  : out ${root_node_type_name}) return Boolean
   is
   begin
      while Next (It.Traverse_It, Element) loop
         if It.Predicate.Evaluate (Element) then
            return True;
         end if;
      end loop;
      return False;
   end Next;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (It : in out Find_Iterator) is
   begin
      Destroy (It.Predicate);
   end Finalize;

   ----------
   -- Next --
   ----------

   overriding function Next
     (It      : in out Local_Find_Iterator;
      Element : out ${root_node_type_name})
      return Boolean
   is
   begin
      while Next (It.Traverse_It, Element) loop
         if It.Predicate (Element) then
            return True;
         end if;
      end loop;
      return False;
   end Next;

   ----------
   -- Find --
   ----------

   function Find
     (Root      : access ${root_node_value_type}'Class;
      Predicate : access function (N : ${root_node_type_name}) return Boolean)
     return Local_Find_Iterator
   is
   begin
      return Local_Find_Iterator'
        (Ada.Finalization.Limited_Controlled with
         Traverse_It => Traverse (Root),

         --  We still want to provide this functionality, even though it is
         --  unsafe. TODO: We might be able to make a safe version of this
         --  using generics. Still would be more verbose though.
         Predicate   => Predicate'Unrestricted_Access.all);
   end Find;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Lex_Env_Data_Type) is
   begin
      Self.Is_Contained_By.Destroy;
      Self.Contains.Destroy;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Lex_Env_Data) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Lex_Env_Data_Type, Lex_Env_Data);
   begin
      Destroy (Self.all);
      Free (Self);
   end Destroy;

   ---------------------------
   -- Remove_Exiled_Entries --
   ---------------------------

   procedure Remove_Exiled_Entries (Self : in out Lex_Env_Data_Type) is
   begin
      for El of Self.Is_Contained_By loop
         AST_Envs.Remove (El.Env, El.Key, El.Node);
      end loop;
      Self.Is_Contained_By.Clear;
   end Remove_Exiled_Entries;

   --------------------------
   -- Reroot_Foreign_Nodes --
   --------------------------

   procedure Reroot_Foreign_Nodes
     (Self : in out Lex_Env_Data_Type; Root_Scope : Lexical_Env)
   is
      Els : ${root_node_type_name}_Vectors.Elements_Array
        := Self.Contains.To_Array;
      Env : Lexical_Env;
   begin
      Self.Is_Contained_By.Clear;
      for El of Els loop
         Env := El.Pre_Env_Actions (El.Self_Env, Root_Scope, True);
         El.Post_Env_Actions (Env, Root_Scope);
      end loop;
   end Reroot_Foreign_Nodes;

   ----------
   -- Find --
   ----------

   function Find
     (Root      : access ${root_node_value_type}'Class;
      Predicate : ${root_node_type_name}_Predicate)
      return Find_Iterator
   is
   begin
      return (Ada.Finalization.Limited_Controlled with
              Traverse_It => Traverse (Root),
              Predicate   => Predicate);
   end Find;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (Root      : access ${root_node_value_type}'Class;
      Predicate : ${root_node_type_name}_Predicate)
      return ${root_node_type_name}
   is
      I      : Find_Iterator := Find (Root, Predicate);
      Result : ${root_node_type_name};
   begin
      if not I.Next (Result) then
         Result := null;
      end if;
      return Result;
   end Find_First;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (P : access ${root_node_type_name}_Kind_Filter;
      N : ${root_node_type_name})
      return Boolean
   is
   begin
      return N.Kind = P.Kind;
   end Evaluate;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : access ${root_node_value_type}'Class;
      Snap : Boolean := False) return Source_Location_Range
   is
      TDH                  : Token_Data_Handler renames
         Node.Unit.Token_Data.all;
      Sloc_Start, Sloc_End : Source_Location;

      function Get (Index : Token_Index) return Token_Data_Type is
        (Get_Token (TDH, Index));

   begin
      if Node.Is_Synthetic then
         return No_Source_Location_Range;
      end if;

      --  Snapping: We'll go one token before the start token, and one token
      --  after the end token, and the sloc range will extend from the end of
      --  the start token to the start of the end token, including any
      --  whitespace and trivia that might be surrounding the node.
      --
      --  TODO: Only nodes we're gonna try to snap are nodes with default
      --  anchors. However, we can make the logic more specific, eg:
      --
      --  * If the start anchor is beginning, then snap the start sloc.
      --
      --  * If the end anchor is ending, then snap the end sloc.
      --
      --  This way composite cases can work too.

      if Snap then
         declare
            Tok_Start : constant Token_Index :=
              Token_Index'Max (Node.Token_Start - 1, 0);
            Tok_End : constant Token_Index :=
              Token_Index'Min (Node.Token_End + 1, Last_Token (TDH));
         begin
            Sloc_Start := End_Sloc (Get (Tok_Start).Sloc_Range);
            Sloc_End := Start_Sloc (Get (Tok_End).Sloc_Range);
         end;
      else
         Sloc_Start := Start_Sloc (Get (Node.Token_Start).Sloc_Range);
         Sloc_End := (if Node.Token_End /= No_Token_Index
                      then End_Sloc (Get (Node.Token_End).Sloc_Range)
                      else Start_Sloc (Get (Node.Token_Start).Sloc_Range));
      end if;
      return Make_Range (Sloc_Start, Sloc_End);
   end Sloc_Range;

   ------------
   -- Lookup --
   ------------

   function Lookup (Node : access ${root_node_value_type}'Class;
                    Sloc : Source_Location;
                    Snap : Boolean := False) return ${root_node_type_name}
   is
      Position : Relative_Position;
      Result   : ${root_node_type_name};
   begin
      if Sloc = No_Source_Location then
         return null;
      end if;

      Lookup_Relative
        (${root_node_type_name} (Node), Sloc, Position, Result, Snap);
      return Result;
   end Lookup;

   ---------------------
   -- Lookup_Internal --
   ---------------------

   function Lookup_Internal
     (Node : ${root_node_type_name};
      Sloc : Source_Location;
      Snap : Boolean := False) return ${root_node_type_name}
   is
      --  For this implementation helper (i.e. internal primitive), we can
      --  assume that all lookups fall into this node's sloc range.
      pragma Assert (Compare (Sloc_Range (Node, Snap), Sloc) = Inside);

      Children : constant ${root_node_array.array()} := Node.Children;
      Pos      : Relative_Position;
      Result   : ${root_node_type_name};
   begin
      --  Look for a child node that contains Sloc (i.e. return the most
      --  precise result).

      for Child of Children loop
         --  Note that we assume here that child nodes are ordered so that the
         --  first one has a sloc range that is before the sloc range of the
         --  second child node, etc.

         if Child /= null then
            Lookup_Relative (Child, Sloc, Pos, Result, Snap);
            case Pos is
               when Before =>
                   --  If this is the first node, Sloc is before it, so we can
                   --  stop here.  Otherwise, Sloc is between the previous
                   --  child node and the next one...  so we can stop here,
                   --  too.
                   return Node;

               when Inside =>
                   return Result;

               when After =>
                   --  Sloc is after the current child node, so see with the
                   --  next one.
                   null;
            end case;
         end if;
      end loop;

      --  If we reach this point, we found no children that covers Sloc, but
      --  Node still covers it (see the assertion).
      return Node;
   end Lookup_Internal;

   -------------
   -- Compare --
   -------------

   function Compare (Node : access ${root_node_value_type}'Class;
                     Sloc : Source_Location;
                     Snap : Boolean := False) return Relative_Position is
   begin
      return Compare (Sloc_Range (Node, Snap), Sloc);
   end Compare;

   ---------------------
   -- Lookup_Relative --
   ---------------------

   procedure Lookup_Relative
     (Node       : ${root_node_type_name};
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out ${root_node_type_name};
      Snap       : Boolean := False)
   is
      Result : constant Relative_Position :=
        Compare (Node, Sloc, Snap);
   begin
      Position := Result;
      Node_Found := (if Result = Inside
                     then Lookup_Internal (Node, Sloc, Snap)
                     else null);
   end Lookup_Relative;

   -------------------
   -- Get_Extension --
   -------------------

   function Get_Extension
     (Node : access ${root_node_value_type}'Class;
      ID   : Extension_ID;
      Dtor : Extension_Destructor) return Extension_Access
   is
      use Extension_Vectors;
   begin
      for Slot of Node.Extensions loop
         if Slot.ID = ID then
            return Slot.Extension;
         end if;
      end loop;

      declare
         New_Ext : constant Extension_Access :=
           new Extension_Type'(Extension_Type (System.Null_Address));
      begin
         Append (Node.Extensions,
                 Extension_Slot'(ID        => ID,
                                 Extension => New_Ext,
                                 Dtor      => Dtor));
         return New_Ext;
      end;
   end Get_Extension;

   ---------------------
   -- Free_Extensions --
   ---------------------

   procedure Free_Extensions (Node : access ${root_node_value_type}'Class) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Extension_Type, Extension_Access);
      use Extension_Vectors;
      Slot : Extension_Slot;
   begin
      --  Explicit iteration for perf
      for J in First_Index (Node.Extensions) .. Last_Index (Node.Extensions)
      loop
         Slot := Get (Node.Extensions, J);
         Slot.Dtor (Node, Slot.Extension.all);
         Free (Slot.Extension);
      end loop;
   end Free_Extensions;

   --------------
   -- Children --
   --------------

   function Children
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_type_name}_Arrays.Array_Type
   is
      <% array_pkg = '{}_Arrays'.format(root_node_type_name) %>

      First : constant Integer := ${array_pkg}.Index_Type'First;
      Last  : constant Integer := First + Child_Count (Node) - 1;
   begin
      return A : ${array_pkg}.Array_Type (First .. Last)
      do
         for I in First .. Last loop
            A (I) := Child (Node, I);
         end loop;
      end return;
   end Children;

   function Children
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_array.name()}
   is
      C : ${root_node_type_name}_Arrays.Array_Type := Children (Node);
   begin
      return Ret : ${root_node_array.name()} := Create (C'Length) do
         Ret.Items := C;
      end return;
   end Children;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (TDH : Token_Data_Handler_Access) return Token_Type is
      use Token_Vectors, Trivia_Vectors, Integer_Vectors;
   begin
      if Length (TDH.Tokens_To_Trivias) = 0
         or else (First_Element (TDH.Tokens_To_Trivias)
                  = Integer (No_Token_Index))
      then
         --  There is no leading trivia: return the first token

         return (if Length (TDH.Tokens) = 0
                 then No_Token
                 else (TDH,
                       Token_Index (First_Index (TDH.Tokens)),
                       No_Token_Index));

      else
         return (TDH, No_Token_Index, Token_Index (First_Index (TDH.Trivias)));
      end if;
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (TDH : Token_Data_Handler_Access) return Token_Type is
      use Token_Vectors, Trivia_Vectors, Integer_Vectors;
   begin
      if Length (TDH.Tokens_To_Trivias) = 0
           or else
         Last_Element (TDH.Tokens_To_Trivias) = Integer (No_Token_Index)
      then
         --  There is no trailing trivia: return the last token

         return (if Length (TDH.Tokens) = 0
                 then No_Token
                 else (TDH,
                       Token_Index (Last_Index (TDH.Tokens)),
                       No_Token_Index));

      else
         return (TDH, No_Token_Index, Token_Index (First_Index (TDH.Trivias)));
      end if;
   end Last_Token;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Token_Type) return Boolean is
      pragma Assert (Left.TDH = Right.TDH);
   begin
      if Left.Token < Right.Token then
         return True;

      elsif Left.Token = Right.Token then
         return Left.Trivia < Right.Trivia;

      else
         return False;
      end if;
   end "<";

   ----------
   -- Next --
   ----------

   function Next (Token : Token_Type) return Token_Type is
   begin
      if Token.TDH = null then
         return No_Token;
      end if;

      declare
         use Token_Vectors, Trivia_Vectors, Integer_Vectors;
         TDH : Token_Data_Handler renames Token.TDH.all;

         function Next_Token return Token_Type is
           (if Token.Token < Token_Index (Last_Index (TDH.Tokens))
            then (Token.TDH, Token.Token + 1, No_Token_Index)
            else No_Token);
         --  Return a reference to the next token (not trivia) or No_Token if
         --  Token was the last one.

      begin
         if Token.Trivia /= No_Token_Index then
            --  Token is a reference to a trivia: take the next trivia if it
            --  exists, or escalate to the next token otherwise.

            declare
               Tr : constant Trivia_Node :=
                  Get (TDH.Trivias, Natural (Token.Trivia));
            begin
               return (if Tr.Has_Next
                       then (Token.TDH, Token.Token, Token.Trivia + 1)
                       else Next_Token);
            end;

         else
            --  Thanks to the guard above, we cannot get to the declare block
            --  for the No_Token case, so if Token does not refers to a trivia,
            --  it must be a token.

            pragma Assert (Token.Token /= No_Token_Index);

            --  If there is no trivia, just go to the next token

            if Length (TDH.Tokens_To_Trivias) = 0 then
               return Next_Token;
            end if;

            --  If this token has trivia, return a reference to the first one,
            --  otherwise get the next token.

            declare
               Tr_Index : constant Token_Index := Token_Index
                 (Get (TDH.Tokens_To_Trivias, Natural (Token.Token) + 1));
            begin
               return (if Tr_Index = No_Token_Index
                       then Next_Token
                       else (Token.TDH, Token.Token, Tr_Index));
            end;
         end if;
      end;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Token : Token_Type) return Token_Type is
   begin
      if Token.TDH = null then
         return No_Token;
      end if;

      declare
         use Token_Vectors, Trivia_Vectors, Integer_Vectors;
         TDH : Token_Data_Handler renames Token.TDH.all;
      begin
         if Token.Trivia = No_Token_Index then
            --  Token is a regular token, so the previous token is either the
            --  last trivia of the previous regular token, either the previous
            --  regular token itself.
            declare
               Prev_Trivia : Token_Index;
            begin
               --  Get the index of the trivia that is right bofre Token (if
               --  any).
               if Length (TDH.Tokens_To_Trivias) = 0 then
                  Prev_Trivia := No_Token_Index;

               else
                  Prev_Trivia := Token_Index
                    (Get (TDH.Tokens_To_Trivias, Natural (Token.Token)));
                  while Prev_Trivia /= No_Token_Index
                           and then
                        Get (TDH.Trivias, Natural (Prev_Trivia)).Has_Next
                  loop
                     Prev_Trivia := Prev_Trivia + 1;
                  end loop;
               end if;

               --  If there is no such trivia and Token was the first one, then
               --  this was the start of the token stream: no previous token.
               if Prev_Trivia = No_Token_Index
                  and then Token.Token <= First_Token_Index
               then
                  return No_Token;
               else
                  return (Token.TDH, Token.Token - 1, Prev_Trivia);
               end if;
            end;

         --  Past this point: Token is known to be a trivia

         elsif Token.Trivia = First_Token_Index then
            --  This is the first trivia for some token, so the previous token
            --  cannot be a trivia.
            return (if Token.Token = No_Token_Index
                    then No_Token
                    else (Token.TDH, Token.Token, No_Token_Index));

         elsif Token.Token = No_Token_Index then
            --  This is a leading trivia and not the first one, so the previous
            --  token has to be a trivia.
            return (Token.TDH, No_Token_Index, Token.Trivia - 1);

         --  Past this point: Token is known to be a trivia *and* it is not a
         --  leading trivia.

         else
            return (Token.TDH,
                    Token.Token,
                    (if Get (TDH.Trivias, Natural (Token.Trivia - 1)).Has_Next
                     then Token.Trivia - 1
                     else No_Token_Index));
         end if;
      end;
   end Previous;

   ----------
   -- Data --
   ----------

   function Data (T : Token_Type) return Token_Data_Type is
   begin
      return (if T.Trivia = No_Token_Index
              then Token_Vectors.Get (T.TDH.Tokens, Natural (T.Token))
              else Trivia_Vectors.Get (T.TDH.Trivias, Natural (T.Trivia)).T);
   end Data;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Token_Type) return Boolean is
      DL : Token_Data_Type := Data (L);
      DR : Token_Data_Type := Data (R);
   begin
      return DL.Kind = DR.Kind
         and then ((DL.Text = null and DR.Text = null)
                   or else DL.Text.all = DR.Text.all);
   end Is_Equivalent;

   -----------
   -- Image --
   -----------

   function Image (Token : Token_Type) return String is
      D : constant Token_Data_Type := Data (Token);
   begin
      return "<Token Kind=" & Token_Kind_Name (D.Kind) & " Text=" &
        (if D.Text = null
         then "None"
         else """" & Image (D.Text.all) & """") & ">";
   end Image;

   --------------------------
   -- Children_With_Trivia --
   --------------------------

   function Children_With_Trivia
     (Node : access ${root_node_value_type}'Class)
      return Children_Arrays.Array_Type
   is
      use Children_Vectors;

      Ret_Vec : Children_Vectors.Vector;
      TDH     : Token_Data_Handler renames Node.Unit.Token_Data.all;

      procedure Append_Trivias (First, Last : Token_Index);
      --  Append all the trivias of tokens between indices First and Last to
      --  the returned vector.

      procedure Append_Trivias (First, Last : Token_Index) is
      begin
         for I in First .. Last loop
            for D of Get_Trivias (TDH, I) loop
               Append (Ret_Vec, (Kind => Trivia, Trivia => D));
            end loop;
         end loop;
      end Append_Trivias;

      function Filter_Children (N : ${root_node_type_name}) return Boolean is
         --  Get rid of null nodes
        (N /= null
         --  Get rid of nodes with no real existence in the source code
         and then not N.Is_Ghost);

      First_Child : constant ${root_node_type_name}_Arrays.Index_Type :=
         ${root_node_type_name}_Arrays.Index_Type'First;
      N_Children  : constant ${root_node_type_name}_Arrays.Array_Type
        := ${root_node_type_name}_Arrays.Filter
          (Children (Node), Filter_Children'Access);
   begin
      if N_Children'Length > 0
        and then Node.Token_Start /= N_Children (First_Child).Token_Start
      then
         Append_Trivias (Node.Token_Start,
                         N_Children (First_Child).Token_Start - 1);
      end if;

      for I in N_Children'Range loop
         Append (Ret_Vec, Child_Record'(Child, N_Children (I)));
         Append_Trivias (N_Children (I).Token_End,
                         (if I = N_Children'Last
                          then Node.Token_End - 1
                          else N_Children (I + 1).Token_Start - 1));
      end loop;

      return A : constant Children_Arrays.Array_Type := To_Array (Ret_Vec) do
         --  Don't forget to free Ret_Vec, since its memory is not
         --  automatically managed.
         Destroy (Ret_Vec);
      end return;
   end Children_With_Trivia;

   --------------
   -- Node_Env --
   --------------

   function Node_Env
     (Node : access ${root_node_value_type})
      return AST_Envs.Lexical_Env
   is (Node.Self_Env);

   ------------------
   -- Children_Env --
   ------------------

   function Children_Env
     (Node : access ${root_node_value_type})
      return AST_Envs.Lexical_Env
   is (Node.Self_Env);

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node   : access ${root_node_value_type}'Class;
      Prefix : String := "")
   is
      Children_Prefix : constant String := Prefix & "|  ";
   begin
      Put_Line (Prefix & Kind_Name (Node));
      for C of Children_With_Trivia (Node) loop
         case C.Kind is
            when Trivia =>
               Put_Line (Children_Prefix & (if C.Trivia.Text = null
                                            then ""
                                            else Image (C.Trivia.Text.all)));
            when Child =>
               C.Node.PP_Trivia (Children_Prefix);
         end case;
      end loop;
   end PP_Trivia;

   use AST_Envs;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env
     (Node     : access ${root_node_value_type}'Class;
      Root_Env : AST_Envs.Lexical_Env)
   is

      procedure Populate_Internal
        (Node        : access ${root_node_value_type}'Class;
         Current_Env : Lexical_Env);

      -----------------------
      -- Populate_Internal --
      -----------------------

      procedure Populate_Internal
        (Node        : access ${root_node_value_type}'Class;
         Current_Env : Lexical_Env)
      is
         Initial_Env : Lexical_Env := Current_Env;
      begin
         if Node = null then
            return;
         end if;

         --  By default (i.e. unless env actions add a new env),
         --  the environment we store in Node is the current one.
         Node.Self_Env := Current_Env;

         Initial_Env := Node.Pre_Env_Actions (Current_Env, Root_Env);

         --  Call recursively on children
         for C of ${root_node_type_name}_Arrays.Array_Type'(Children (Node))
         loop
            Populate_Internal (C, Node.Self_Env);
         end loop;

         Node.Post_Env_Actions (Initial_Env, Root_Env);
      end Populate_Internal;

      Env : AST_Envs.Lexical_Env := Root_Env;
   begin
      Populate_Internal (Node, Env);
   end Populate_Lexical_Env;

   -----------------
   -- Short_Image --
   -----------------

   function Short_Image
     (Node : access ${root_node_value_type})
      return Text_Type
   is
      Self : access ${root_node_value_type}'Class := Node;
   begin
      return "<" & To_Text (Kind_Name (Self))
             & " " & To_Text (Image (Sloc_Range (Node))) & ">";
   end Short_Image;

   ------------------------
   -- Address_To_Id_Maps --
   ------------------------

   --  Those maps are used to give unique ids to lexical envs while pretty
   --  printing them.

   function Hash (S : Lexical_Env) return Hash_Type is
     (Hash_Type (To_Integer (S.all'Address)));

   package Address_To_Id_Maps is new Ada.Containers.Hashed_Maps
     (Lexical_Env, Positive, Hash, "=");

   -----------------
   -- Sorted_Envs --
   -----------------

   --  Those ordered maps are used to have a stable representation of internal
   --  lexical environments, which is not the case with hashed maps.

   function "<" (L, R : Symbol_Type) return Boolean
   is
     (L.all < R.all);

   package Sorted_Envs is new Ada.Containers.Ordered_Maps
     (Symbol_Type,
      Element_Type    => AST_Envs.Env_Element_Vectors.Vector,
      "<"             => "<",
      "="             => AST_Envs.Env_Element_Vectors."=");

   -------------------
   -- To_Sorted_Env --
   -------------------

   function To_Sorted_Env (Env : Internal_Envs.Map) return Sorted_Envs.Map is
      Ret_Env : Sorted_Envs.Map;
      use Internal_Envs;
   begin
      for El in Env.Iterate loop
         Ret_Env.Include (Key (El), Element (El));
      end loop;
      return Ret_Env;
   end To_Sorted_Env;


   ----------
   -- Dump --
   ----------

   procedure Dump_One_Lexical_Env
     (Self          : AST_Envs.Lexical_Env; Env_Id : String := "";
      Parent_Env_Id : String := "")
   is
      use Sorted_Envs;

      function Image (El : Env_Element) return String is
        (Image (Short_Image (El.El)));
      -- TODO??? This is slightly hackish, because we're converting a wide
      -- string back to string. But since we're using this solely for
      -- test/debug purposes, it should not matter. Still, would be good to
      -- have Text_Type everywhere at some point.

      function Image is new AST_Envs.Env_Element_Vectors.Image (Image);

   begin
      Put ("<LexEnv (Id" & Env_Id & ", Parent"
           & (if Self.Parent /= AST_Envs.No_Env_Getter
              then Parent_Env_Id else " null")
           & "), ");

      if Self.Env.Is_Empty then
         Put_Line ("empty>");
      else
         Put_Line ("{");
         for El in To_Sorted_Env (Self.Env.all).Iterate loop
            Put ("    ");
            Put_Line (Langkit_Support.Text.Image (Key (El).all) & ": "
                 & Image (Element (El)));
         end loop;
         Put_Line ("}>");
      end if;
   end Dump_One_Lexical_Env;
   --  This procedure dumps *one* lexical environment


   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env
     (Node     : access ${root_node_value_type}'Class;
      Root_Env : AST_Envs.Lexical_Env)
   is
      use Address_To_Id_Maps;

      Env_Ids        : Address_To_Id_Maps.Map;
      Current_Env_Id : Positive := 1;

      ----------------
      -- Get_Env_Id --
      ----------------

      function Get_Env_Id (E : Lexical_Env) return String is
         C        : Address_To_Id_Maps.Cursor;
         Inserted : Boolean;
      begin
         if E = Root_Env then
            return " <root>";
         elsif E = null then
            return " <null>";
         end if;

         Env_Ids.Insert (E, Current_Env_Id, C, Inserted);
         if Inserted then
            Current_Env_Id := Current_Env_Id + 1;
         end if;
         return Address_To_Id_Maps.Element (C)'Img;
      end Get_Env_Id;
      --  Retrieve the Id for a lexical env. Assign one if none was yet
      --  assigned.

      --------------
      -- Internal --
      --------------

      Env : Lexical_Env := null;

      procedure Internal (Current : ${root_node_type_name}) is
      begin
         if Current = null
            --  We want to ignore ghost nodes. This includes empty lists, but
            --  also Opt parsers parsed to absent qualifiers.
            or else Current.Is_Ghost
         then
            return;
         end if;

         --  We only dump environments that we haven't dumped before. This way
         --  we'll only dump environments at the site of their creation, and
         --  not in any subsequent link. We use the Env_Ids map to check which
         --  envs we have already seen or not.
         if not Env_Ids.Contains (Current.Self_Env) then
            Env := Current.Self_Env;
            Put ("<" & Kind_Name (Current) & " "
                 & Image (Sloc_Range (Current)) & "> - ");
            Dump_One_Lexical_Env
              (Env, Get_Env_Id (Env),
               Get_Env_Id (AST_Envs.Get_Env (Env.Parent)));
         end if;

         for Child of ${root_node_type_name}_Arrays.Array_Type'
            (Children (Current))
         loop
            Internal (Child);
         end loop;
      end Internal;
      --  This procedure implements the main recursive logic of dumping the
      --  environments.
   begin
      Internal (${root_node_type_name} (Node));
   end Dump_Lexical_Env;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node         : access ${root_node_value_type}'Class;
      Include_Self : Boolean := True)
      return ${root_node_array.name()}
   is
      Count : Natural := 0;
      Start : ${root_node_type_name} :=
        ${root_node_type_name} (if Include_Self then Node else Node.Parent);
      Cur   : ${root_node_type_name} := Start;
   begin
      while Cur /= null loop
         Count := Count + 1;
         Cur := Cur.Parent;
      end loop;

      declare
         Result : constant ${root_node_array.name()} := Create (Count);
      begin
         Cur := Start;
         for I in Result.Items'Range loop
            Result.Items (I) := Cur;
            Cur := Cur.Parent;
         end loop;
         return Result;
      end;
   end Parents;

   ------------
   -- Parent --
   ------------

   function Parent
     (Node : access ${root_node_value_type}'Class) return ${root_node_type_name}
   is
   begin
      return Node.Parent;
   end Parent;

   ------------------
   -- Stored_Token --
   ------------------

   function Stored_Token
     (Node  : access ${root_node_value_type}'Class;
      Token : Token_Type)
      return Token_Index
   is
   begin
      if Node.Unit.Token_Data /= Token.TDH then
         raise Property_Error with
           ("Cannot associate a token and a node from different analysis"
            & " units");
      elsif Token.Trivia /= No_Token_Index then
         raise Property_Error with
           ("A node cannot hold trivia");
      end if;

      return Token.Token;
   end Stored_Token;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Node : ${root_node_value_type})
      return
      ${root_node_type_name}_Ada2012_Iterators.Reversible_Iterator'Class
   is
   begin
      return It : constant Iterator := (Node => Node'Unrestricted_Access)
      do
         null;
      end return;
   end Iterate;

   -----------
   -- First --
   -----------

   overriding function First (Object : Iterator) return Children_Cursor is
      Node : ${root_node_type_name} renames Object.Node;
   begin
      return (if Node.Child_Count > 0
              then (Node, 1)
              else No_Children);
   end First;

   ----------
   -- Last --
   ----------

   overriding function Last (Object : Iterator) return Children_Cursor is
      Node : ${root_node_type_name} renames Object.Node;
   begin
      return (if Node.Child_Count > 0
              then (Node, Node.Child_Count)
              else No_Children);
   end Last;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object : Iterator;
      C      : Children_Cursor)
      return Children_Cursor
   is
      Node : ${root_node_type_name} renames Object.Node;
   begin
      if C = No_Children or else C.Child_Index >= Node.Child_Count then
         return No_Children;
      else
         return (Node, C.Child_Index + 1);
      end if;
   end Next;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Object : Iterator;
      C      : Children_Cursor)
      return Children_Cursor
   is
      pragma Unreferenced (Object);
   begin
      if C = No_Children or else C.Child_Index <= 1 then
         return No_Children;
      else
         return (C.Node, C.Child_Index - 1);
      end if;
   end Previous;

   ------------------
   -- Child_Number --
   ------------------

   function Child_Number
     (Node : access ${root_node_value_type}'Class)
      return Positive
   is
      N : ${root_node_type_name} := null;
   begin
      for I in Node.Parent.First_Child_Index .. Node.Parent.Last_Child_Index
      loop
         N := Child (Node.Parent, I);
         if N = Node then
            return I;
         end if;
      end loop;

      --  If we reach this point, then Node isn't a Child of Node.Parent. This
      --  is not supposed to happen.
      raise Program_Error;
   end Child_Number;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_type_name}
   is
      N : constant Positive := Child_Number (Node);
   begin
      return (if N = 1
              then null
              else Node.Parent.Child (N - 1));
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_type_name}
   is
   begin
      --  If Node is the last sibling, then Child will return null
      return Node.Parent.Child (Child_Number (Node) + 1);
   end Next_Sibling;

   % if ctx.env_metadata:
   ${struct_types.body(ctx.env_metadata)}

   -------------
   -- Combine --
   -------------

   function Combine
     (L, R : ${ctx.env_metadata.name()}) return ${ctx.env_metadata.name()}
   is
      % if not ctx.env_metadata.get_fields():
      pragma Unreferenced (L, R);
      % endif
      Ret : ${ctx.env_metadata.name()} := (others => False);
   begin
      % for field in ctx.env_metadata.get_fields():
         Ret.${field.name} := L.${field.name} or R.${field.name};
      % endfor
      return Ret;
   end Combine;

   % endif

   ---------
   -- Get --
   ---------

   function Get
     (A     : AST_Envs.Env_Element_Array;
      Index : Integer)
      return Env_Element
   is
      function Length (A : AST_Envs.Env_Element_Array) return Natural
      is (A'Length);

      function Get
        (A     : AST_Envs.Env_Element_Array;
         Index : Integer)
         return Env_Element
      is (A (Index + 1)); --  A is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Env_Element,
         Sequence_Type => AST_Envs.Env_Element_Array,
         Length        => Length,
         Get           => Get);
      Result : Env_Element;
   begin
      if Relative_Get (A, Index, Result) then
         return Result;
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   ## Generate the bodies of the root grammar class properties
   % for prop in T.root_node.get_properties(include_inherited=False):
   ${prop.prop_def}
   % endfor

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars
    (Node : access ${root_node_value_type}'Class) is
   begin
      if Adalog.Debug.Debug then
         % for f in T.root_node.get_fields( \
              include_inherited=False, \
              predicate=lambda f: is_logic_var(f.type) \
         ):
            Node.${f.name}.Dbg_Name :=
              new String'(Image (Node.Short_Image) & ".${f.name}");
         % endfor
         for Child of ${root_node_type_name}_Arrays.Array_Type'
            (Children (Node))
         loop
            if Child /= null then
               Assign_Names_To_Logic_Vars (Child);
            end if;
         end loop;
      end if;
   end Assign_Names_To_Logic_Vars;

   ---------------
   -- Can_Reach --
   ---------------

   function Can_Reach (El, From : ${root_node_type_name}) return Boolean
   is
   begin
      --  Since this function is only used to implement sequential semantics in
      --  envs, we consider that elements coming from different units are
      --  always visible for each other, and let the user implement language
      --  specific visibility rules in the DSL.
      if Get_Unit_Internal (El) /= Get_Unit_Internal (From) then
         return True;
      end if;

       return Compare
         (Start_Sloc (Sloc_Range (El)),
          Start_Sloc (Sloc_Range (From))) = After;
   end Can_Reach;

end ${_self.ada_api_settings.lib_name}.AST;
