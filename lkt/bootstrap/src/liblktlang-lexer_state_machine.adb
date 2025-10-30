


package body Liblktlang.Lexer_State_Machine is

   Is_Trivia : constant array (Token_Kind) of Boolean :=
     (Lkt_Amp => False,
      Lkt_And_Kw => False,
      Lkt_At => False,
      Lkt_Big_Number => False,
      Lkt_Bind_Kw => False,
      Lkt_Block_String_Line => False,
      Lkt_Case_Kw => False,
      Lkt_Char => False,
      Lkt_Class_Kw => False,
      Lkt_Colon => False,
      Lkt_Comb => False,
      Lkt_Comma => False,
      Lkt_Comment => True,
      Lkt_Discard_Kw => False,
      Lkt_Div => False,
      Lkt_Dot => False,
      Lkt_Double_Colon => False,
      Lkt_Dyn_Var_Kw => False,
      Lkt_E_Q => False,
      Lkt_Elif_Kw => False,
      Lkt_Ellipsis => False,
      Lkt_Else_Kw => False,
      Lkt_Enum_Kw => False,
      Lkt_Equal => False,
      Lkt_Excl_Mark => False,
      Lkt_Fat_Right_Arrow => False,
      Lkt_Fun_Kw => False,
      Lkt_G_T => False,
      Lkt_G_T_E => False,
      Lkt_Generic_Kw => False,
      Lkt_Grammar_Kw => False,
      Lkt_Identifier => False,
      Lkt_If_Kw => False,
      Lkt_Implements_Kw => False,
      Lkt_Import_Kw => False,
      Lkt_In_Kw => False,
      Lkt_Int_Mark => False,
      Lkt_Is_Kw => False,
      Lkt_L_Brace => False,
      Lkt_L_Brack => False,
      Lkt_L_Par => False,
      Lkt_L_T => False,
      Lkt_L_T_E => False,
      Lkt_Left_Arrow => False,
      Lkt_Lexer_Kw => False,
      Lkt_Lexing_Failure => True,
      Lkt_Match_Kw => False,
      Lkt_Minus => False,
      Lkt_N_E => False,
      Lkt_Not_Kw => False,
      Lkt_Null_Kw => False,
      Lkt_Number => False,
      Lkt_Or_Kw => False,
      Lkt_P_String => False,
      Lkt_Percent => False,
      Lkt_Pipe => False,
      Lkt_Plus => False,
      Lkt_Private_Kw => False,
      Lkt_Public_Kw => False,
      Lkt_R_Brace => False,
      Lkt_R_Brack => False,
      Lkt_R_Par => False,
      Lkt_Raise_Kw => False,
      Lkt_Right_Arrow => False,
      Lkt_Semicolon => False,
      Lkt_String => False,
      Lkt_Struct_Kw => False,
      Lkt_Termination => False,
      Lkt_Then_Kw => False,
      Lkt_Times => False,
      Lkt_Trait_Kw => False,
      Lkt_Triple_Colon => False,
      Lkt_Try_Kw => False,
      Lkt_Two_Sided_Arrow => False,
      Lkt_Val_Kw => False,
      Lkt_When_Kw => False,
      Lkt_Whitespace => True);

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
      Self.Last_Token := (Kind       => Lkt_Termination,
                          Text_First => Input_First,
                          Text_Last  => Input_First - 1);
      Self.Last_Token_Kind := Lkt_Termination;
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



         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) .. Character_Type'Val (16#a#) | Character_Type'Val (16#c#) .. Character_Type'Val (16#d#) | ' ' => goto State_1;
               when '!' => goto State_2;
               when '"' => goto State_3;
               when '#' => goto State_4;
               when '%' => goto State_5;
               when '&' => goto State_6;
               when ''' => goto State_7;
               when '(' => goto State_8;
               when ')' => goto State_9;
               when '*' => goto State_10;
               when '+' => goto State_11;
               when ',' => goto State_12;
               when '-' => goto State_13;
               when '.' => goto State_14;
               when '/' => goto State_15;
               when '0' .. '9' => goto State_16;
               when ':' => goto State_17;
               when ';' => goto State_18;
               when '<' => goto State_19;
               when '=' => goto State_20;
               when '>' => goto State_21;
               when '?' => goto State_22;
               when '@' => goto State_23;
               when 'A' .. 'Z' | 'b' .. 'a' | 'c' .. 'b' | 'd' .. 'c' | 'e' .. 'd' | 'f' .. 'e' | 'g' .. 'f' | 'h' | 'j' .. 'k' | 'm' .. 'l' | 'n' .. 'm' | 'o' .. 'n' | 'p' .. 'o' | 'q' | 's' .. 'r' | 't' .. 's' | 'u' | 'w' .. 'v' | 'x' .. 'z' => goto State_24;
               when '[' => goto State_25;
               when ']' => goto State_26;
               when '_' => goto State_27;
               when 'a' => goto State_28;
               when 'b' => goto State_29;
               when 'c' => goto State_30;
               when 'd' => goto State_31;
               when 'e' => goto State_32;
               when 'f' => goto State_33;
               when 'g' => goto State_34;
               when 'i' => goto State_35;
               when 'l' => goto State_36;
               when 'm' => goto State_37;
               when 'n' => goto State_38;
               when 'o' => goto State_39;
               when 'p' => goto State_40;
               when 'r' => goto State_41;
               when 's' => goto State_42;
               when 't' => goto State_43;
               when 'v' => goto State_44;
               when 'w' => goto State_45;
               when '{' => goto State_46;
               when '|' => goto State_47;
               when '}' => goto State_48;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_1>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Whitespace;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) .. Character_Type'Val (16#a#) | Character_Type'Val (16#c#) .. Character_Type'Val (16#d#) | ' ' => goto State_49;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_2>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Excl_Mark;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_50;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_3>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_51;
               when '"' => goto State_52;
               when '\' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_4>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_54;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_5>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Percent;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_6>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Amp;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_7>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_55;
               when ''' => goto State_56;
               when '\' => goto State_57;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_8>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_L_Par;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_9>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_R_Par;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_10>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Times;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_11>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Plus;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_12>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Comma;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_13>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Minus;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '>' => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_14>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Dot;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '.' => goto State_59;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_15>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Div;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_16>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Number;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_60;
               when 'b' => goto State_61;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_17>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Colon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ':' => goto State_62;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_18>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Semicolon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_19>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_L_T;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '-' => goto State_63;
               when '=' => goto State_64;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_20>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Equal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_65;
               when '>' => goto State_66;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_21>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_G_T;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_67;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_22>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Int_Mark;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_23>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_At;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_24>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_25>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_L_Brack;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_26>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_R_Brack;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_27>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_28>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_69;
               when 'n' => goto State_70;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_29>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_69;
               when 'i' => goto State_71;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_30>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'k' | 'm' .. 'z' => goto State_69;
               when 'a' => goto State_72;
               when 'l' => goto State_73;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_31>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'x' | 'z' => goto State_69;
               when 'i' => goto State_74;
               when 'y' => goto State_75;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_32>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' | 'o' .. 'z' => goto State_69;
               when 'l' => goto State_76;
               when 'n' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_33>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 't' | 'v' .. 'z' => goto State_69;
               when 'u' => goto State_78;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_34>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'q' | 's' .. 'z' => goto State_69;
               when 'e' => goto State_79;
               when 'r' => goto State_80;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_35>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'e' | 'g' .. 'l' | 'n' .. 'm' | 'o' .. 'r' | 't' .. 'z' => goto State_69;
               when 'f' => goto State_81;
               when 'm' => goto State_82;
               when 'n' => goto State_83;
               when 's' => goto State_84;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_36>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_85;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_37>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_69;
               when 'a' => goto State_86;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_38>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'n' | 'p' .. 't' | 'v' .. 'z' => goto State_69;
               when 'o' => goto State_87;
               when 'u' => goto State_88;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_39>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_69;
               when 'r' => goto State_89;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_40>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 't' | 'v' .. 'z' => goto State_69;
               when 'r' => goto State_90;
               when 'u' => goto State_91;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_41>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_69;
               when 'a' => goto State_92;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_42>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_69;
               when 't' => goto State_93;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_43>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'g' | 'i' .. 'q' | 's' .. 'z' => goto State_69;
               when 'h' => goto State_94;
               when 'r' => goto State_95;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_44>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_69;
               when 'a' => goto State_96;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_45>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_68;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'g' | 'i' .. 'z' => goto State_69;
               when 'h' => goto State_97;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_46>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_L_Brace;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_47>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Pipe;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_98;
               when '>' => goto State_99;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_48>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_R_Brace;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_49>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Whitespace;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) .. Character_Type'Val (16#a#) | Character_Type'Val (16#c#) .. Character_Type'Val (16#d#) | ' ' => goto State_49;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_50>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_N_E;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_51>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_51;
               when '"' => goto State_52;
               when '\' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_52>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_53>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_100;
               when '"' => goto State_101;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_54>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_102;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_55>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. Character_Type'Val (16#10ffff#) => goto State_55;
               when ''' => goto State_56;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_56>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Char;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_57>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. Character_Type'Val (16#10ffff#) => goto State_55;
               when ''' => goto State_103;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_58>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Right_Arrow;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_59>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '.' => goto State_104;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_60>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Number;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_60;
               when 'b' => goto State_61;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_61>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Big_Number;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_62>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Double_Colon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ':' => goto State_105;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_63>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Left_Arrow;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '>' => goto State_106;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_64>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_L_T_E;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_65>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_E_Q;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_66>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Fat_Right_Arrow;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_67>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_G_T_E;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_68>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_107;
               when '"' => goto State_108;
               when '\' => goto State_109;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_69>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_70>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'c' | 'e' .. 'z' => goto State_69;
               when 'd' => goto State_110;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_71>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_69;
               when 'n' => goto State_111;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_72>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_69;
               when 's' => goto State_112;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_73>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_69;
               when 'a' => goto State_113;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_74>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_69;
               when 's' => goto State_114;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_75>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_69;
               when 'n' => goto State_115;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_76>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'r' | 't' .. 'z' => goto State_69;
               when 'i' => goto State_116;
               when 's' => goto State_117;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_77>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 't' | 'v' .. 'z' => goto State_69;
               when 'u' => goto State_118;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_78>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_69;
               when 'n' => goto State_119;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_79>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_69;
               when 'n' => goto State_120;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_80>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_69;
               when 'a' => goto State_121;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_81>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_If_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_82>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'o' | 'q' .. 'z' => goto State_69;
               when 'p' => goto State_122;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_83>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_In_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_84>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Is_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_85>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'w' | 'y' .. 'z' => goto State_69;
               when 'x' => goto State_123;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_86>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_69;
               when 't' => goto State_124;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_87>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_69;
               when 't' => goto State_125;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_88>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_69;
               when 'l' => goto State_126;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_89>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Or_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_90>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_69;
               when 'i' => goto State_127;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_91>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' | 'c' .. 'z' => goto State_69;
               when 'b' => goto State_128;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_92>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_69;
               when 'i' => goto State_129;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_93>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_69;
               when 'r' => goto State_130;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_94>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_131;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_95>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'x' | 'z' => goto State_69;
               when 'a' => goto State_132;
               when 'y' => goto State_133;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_96>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_69;
               when 'l' => goto State_134;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_97>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_135;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_98>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Block_String_Line;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#c#) | Character_Type'Val (16#e#) .. Character_Type'Val (16#10ffff#) => goto State_136;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_99>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Comb;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_100>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_51;
               when '"' => goto State_52;
               when '\' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_101>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_51;
               when '"' => goto State_52;
               when '\' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_102>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_102;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_103>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Char;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ''' => goto State_56;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_104>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Ellipsis;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_105>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Triple_Colon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_106>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Two_Sided_Arrow;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_107>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_107;
               when '"' => goto State_108;
               when '\' => goto State_109;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_108>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_P_String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_109>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_137;
               when '"' => goto State_138;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_110>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_And_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_111>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'c' | 'e' .. 'z' => goto State_69;
               when 'd' => goto State_139;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_112>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_140;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_113>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_69;
               when 's' => goto State_141;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_114>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'b' | 'd' .. 'z' => goto State_69;
               when 'c' => goto State_142;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_115>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'u' | 'w' .. 'z' => goto State_69;
               when 'v' => goto State_143;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_116>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'e' | 'g' .. 'z' => goto State_69;
               when 'f' => goto State_144;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_117>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_145;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_118>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'l' | 'n' .. 'z' => goto State_69;
               when 'm' => goto State_146;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_119>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Fun_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_120>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_147;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_121>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'l' | 'n' .. 'z' => goto State_69;
               when 'm' => goto State_148;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_122>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'n' | 'p' .. 'z' => goto State_69;
               when 'l' => goto State_149;
               when 'o' => goto State_150;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_123>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_151;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_124>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'b' | 'd' .. 'z' => goto State_69;
               when 'c' => goto State_152;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_125>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Not_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_126>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_69;
               when 'l' => goto State_153;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_127>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'u' | 'w' .. 'z' => goto State_69;
               when 'v' => goto State_154;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_128>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_69;
               when 'l' => goto State_155;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_129>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_69;
               when 's' => goto State_156;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_130>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 't' | 'v' .. 'z' => goto State_69;
               when 'u' => goto State_157;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_131>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_69;
               when 'n' => goto State_158;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_132>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_69;
               when 'i' => goto State_159;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_133>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Try_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_134>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Val_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_135>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_69;
               when 'n' => goto State_160;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_136>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Block_String_Line;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#c#) | Character_Type'Val (16#e#) .. Character_Type'Val (16#10ffff#) => goto State_136;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_137>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_107;
               when '"' => goto State_108;
               when '\' => goto State_109;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_138>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_107;
               when '"' => goto State_108;
               when '\' => goto State_109;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_139>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Bind_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_140>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Case_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_141>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_69;
               when 's' => goto State_161;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_142>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_69;
               when 'a' => goto State_162;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_143>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_69;
               when 'a' => goto State_163;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_144>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Elif_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_145>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Else_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_146>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Enum_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_147>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_69;
               when 'r' => goto State_164;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_148>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'l' | 'n' .. 'z' => goto State_69;
               when 'm' => goto State_165;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_149>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_166;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_150>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_69;
               when 'r' => goto State_167;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_151>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_69;
               when 'r' => goto State_168;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_152>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'g' | 'i' .. 'z' => goto State_69;
               when 'h' => goto State_169;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_153>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Null_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_154>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_69;
               when 'a' => goto State_170;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_155>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_69;
               when 'i' => goto State_171;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_156>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_172;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_157>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'b' | 'd' .. 'z' => goto State_69;
               when 'c' => goto State_173;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_158>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Then_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_159>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_69;
               when 't' => goto State_174;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_160>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_When_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_161>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Class_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_162>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_69;
               when 'r' => goto State_175;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_163>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_69;
               when 'r' => goto State_176;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_164>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_69;
               when 'i' => goto State_177;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_165>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_69;
               when 'a' => goto State_178;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_166>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'l' | 'n' .. 'z' => goto State_69;
               when 'm' => goto State_179;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_167>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_69;
               when 't' => goto State_180;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_168>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Lexer_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_169>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Match_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_170>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_69;
               when 't' => goto State_181;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_171>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'b' | 'd' .. 'z' => goto State_69;
               when 'c' => goto State_182;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_172>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Raise_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_173>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_69;
               when 't' => goto State_183;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_174>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Trait_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_175>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'c' | 'e' .. 'z' => goto State_69;
               when 'd' => goto State_184;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_176>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Dyn_Var_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_177>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'b' | 'd' .. 'z' => goto State_69;
               when 'c' => goto State_185;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_178>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_69;
               when 'r' => goto State_186;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_179>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_187;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_180>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Import_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_181>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_69;
               when 'e' => goto State_188;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_182>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Public_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_183>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Struct_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_184>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Discard_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_185>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Generic_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_186>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Grammar_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_187>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_69;
               when 'n' => goto State_189;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_188>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Private_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_189>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_69;
               when 't' => goto State_190;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_190>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Identifier;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_69;
               when 's' => goto State_191;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_191>>

               Match_Index := Index - 1;
               Match_Kind := Lkt_Implements_Kw;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;


      <<Stop>>
      --  We end up here as soon as the currently analyzed character was not
      --  accepted by any transitions from the current state. Two cases from
      --  there:

      if Match_Index = 0 then
         --  We haven't found a match. Just create an error token and plan to
         --  start a new token at the next character.
         if Index > Self.Input_Last then
            Token := (Lkt_Termination, Index, Index - 1);
            Self.Has_Next := False;
         else
            Token := (Lkt_Lexing_Failure, First_Index, First_Index);
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

end Liblktlang.Lexer_State_Machine;
