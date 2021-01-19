## vim: filetype=makoada

<%
   lexer = ctx.lexer
   termination = lexer.Termination.ada_name
   lexing_failure = lexer.LexingFailure.ada_name
%>

package body ${ada_lib_name}.Lexer_State_Machine is

   Is_Trivia : constant array (Token_Kind) of Boolean := (
      ${', '.join('{} => {}'.format(t.ada_name, t.is_trivia)
                  for t in lexer.sorted_tokens)}
   );

   type Character_Range is record
      First, Last : Character_Type;
   end record;

   type Character_Range_Array is array (Positive range <>) of Character_Range;
   --  Sorted list of dijoint character ranges

   pragma Warnings (Off, "referenced");
   function Contains
     (Char : Character_Type; Ranges : Character_Range_Array) return Boolean;
   --  Return whether Char is included in the given ranges
   pragma Warnings (On, "referenced");

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : out Lexer_State;
      Input       : Text_Access;
      Input_First : Positive;
      Input_Last  : Natural) is
   begin
      Self.Input := Input;
      Self.Input_First := Input_First;
      Self.Input_Last := Input_Last;
      Self.Has_Next := True;
      Self.Last_Token := (Kind       => ${termination},
                          Text_First => Input_First,
                          Text_Last  => Input_First - 1);
      Self.Last_Token_Kind := ${termination};
   end Initialize;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Self : Lexer_State) return Lexed_Token is
   begin
      return Self.Last_Token;
   end Last_Token;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (Self : Lexer_State) return Boolean is
   begin
      return Self.Has_Next;
   end Has_Next;

   --------------
   -- Contains --
   --------------

   function Contains
     (Char : Character_Type; Ranges : Character_Range_Array) return Boolean
   is
      Low  : Natural := Ranges'First;
      High : Natural := Ranges'Last;
   begin
      while Low <= High loop
         declare
            Middle : constant Natural := (Low + High) / 2;
            R      : Character_Range renames Ranges (Middle);
         begin
            if Char < R.First then
               High := Middle - 1;
            elsif Char > R.Last then
               Low := Middle + 1;
            else
               return True;
            end if;
         end;
      end loop;
      return False;
   end Contains;

${emitter.dfa_code.ada_table_decls('   ')}

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Self : in out Lexer_State; Token : out Lexed_Token)
   is
      Input : constant Text_Access := Self.Input;

      First_Index : Positive;
      --  Index of the first input character for the token to return

      Index : Positive;
      --  Index for the next input character to be analyzed

      Match_Index : Natural;
      --  If we found a match, index for its last character. Otherwise, zero.

      Match_Ignore : Boolean;
      --  If we found a match, whether we must ignore it and restart the
      --  automaton after its character range.

      Match_Kind : Token_Kind;
      --  If we found a match and it is not ignored, kind for the token to
      --  emit. Meaningless otherwise.
   begin
      First_Index := Self.Last_Token.Text_Last + 1;

      <<Start>>
      Index := First_Index;
      Match_Index := 0;
      Match_Ignore := False;

      % for i, state in enumerate(emitter.dfa_code.states):
         ## No transition can go to the first state, so don't emit a label
         ## for it. This avoids an "unreferenced" warning.
         % if i > 0:
            <<${state.label}>>
         % endif

         ## If actions are associated to this state, execute them now. Note
         ## that we will still continue running the automaton: we don't want to
         ## return a token as soon as we find one, but rather return the
         ## longest one.
         % if state.action is not None:
            % if state.action.is_case_action:
               case Self.Last_Token_Kind is
                  % for alt in state.action.all_alts:
                     when ${('others' if alt.prev_token_cond is None else
                             ' | '.join(t.ada_name
                                        for t in alt.prev_token_cond))} =>
                        Match_Kind := ${alt.send.ada_name};
                        Match_Index := Index - 1 - ${(
                           state.action.match_length - alt.match_size
                        )};
                  % endfor
               end case;

            % elif state.action.is_ignore:
               Match_Index := Index - 1;
               Match_Ignore := True;

            % else:
               Match_Index := Index - 1;
               Match_Kind := ${state.action.ada_name};
            % endif
         % endif

         ## If we are about to read past the input buffer, just stop there
         if Index > Self.Input_Last then
            goto Stop;
         end if;

         ## Read the current character and transition to the next state, or
         ## stop if there is no transition for that character.
         % if state.has_transitions:
         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            ## Lower case transitions
            % if state.case_transitions:
            case Input_Char is
            % for char_set, next_state in state.case_transitions:
               when ${char_set.ada_ranges} => goto ${next_state};
            % endfor
            % endif

            when others =>
               ## If there are some, handle non-ASCII transitions
               % if state.named_table_transitions:
               if Input_Char > Character_Type'Val (127) then
                  % for table_name, next_state \
                        in state.named_table_transitions:
                     if Contains (Input_Char, ${table_name}) then
                        goto ${next_state};
                     end if;
                  % endfor
               end if;
               % endif

               ## If control flow reaches this point, it means that we could
               ## not match a token up to the current point: stop here.
               goto Stop;
            end case;
         end;
         % else:
         Index := Index + 1;
         goto Stop;
         % endif

      % endfor

      <<Stop>>
      --  We end up here as soon as the currently analyzed character was not
      --  accepted by any transitions from the current state. Two cases from
      --  there:

      if Match_Index = 0 then
         --  We haven't found a match. Just create an error token and plan to
         --  start a new token at the next character.
         if Index > Self.Input_Last then
            Token := (${termination}, Index, Index - 1);
            Self.Has_Next := False;
         else
            Token := (${lexing_failure}, First_Index, First_Index);
         end if;

      elsif Match_Ignore then
         --  We found a match. It must be ignored: resume lexing to start right
         --  after the matched text.
         First_Index := Match_Index + 1;
         goto Start;

      else
         --  We found a match for which we must emit a token
         Token := (Match_Kind, First_Index, Match_Index);
      end if;

      Self.Last_Token := Token;
      if not Is_Trivia (Token.Kind) then
         Self.Last_Token_Kind := Token.Kind;
      end if;
   end Next_Token;

end ${ada_lib_name}.Lexer_State_Machine;
