--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Internal.Unparsing;
use Langkit_Support.Internal.Unparsing;

package body Langkit_Support.Generic_API.Unparsing is

   ----------------------
   -- Required_Spacing --
   ----------------------

   function Required_Spacing
     (Left, Right : Token_Kind_Ref) return Spacing_Kind is
   begin
      if Left = No_Token_Kind_Ref then
         return None;
      elsif Language (Left) /= Language (Right) then
         raise Precondition_Failure with
           "inconsistent languages for requested token kinds";
      end if;

      declare
         Id : constant Language_Descriptor_Access := +Language (Left);
         LK : constant Token_Kind_Index := To_Index (Left);

         function Family (Kind : Token_Kind_Index) return Token_Family_Index
         is (Id.Token_Kinds (Kind).Family);
      begin
         --  If a newline is required after Left, we do not even need to check
         --  what Right is.

         if Id.Unparsers.Token_Newlines (LK) then
            return Newline;

         --  Otherwise, check if at least a space is required between Left and
         --  Right.

         elsif Id.Unparsers.Token_Spacings
                 (Family (LK), Family (To_Index (Right)))
         then
            return Whitespace;

         else
            return None;
         end if;
      end;
   end Required_Spacing;

   --------------------------
   -- Unparse_To_Fragments --
   --------------------------

   procedure Unparse_To_Fragments
     (Node : Lk_Node; Fragments : out Unparsing_Fragment_Vectors.Vector)
   is
      Id        : constant Language_Id := Node.Language;
      Desc      : constant Language_Descriptor_Access := +Id;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      procedure Append (Token : Token_Unparser);
      procedure Append (Tokens : Token_Sequence);

      ------------
      -- Append --
      ------------

      procedure Append (Token : Token_Unparser) is
      begin
         Fragments.Append
           ((Kind       => Token_Fragment,
             Token_Kind => From_Index (Id, Token.Kind),
             Token_Text => To_Unbounded_Text (Token.Text.all)));
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append (Tokens : Token_Sequence) is
      begin
         for T of Tokens.all loop
            Append (T);
         end loop;
      end Append;

      Node_Type     : constant Type_Ref := Type_Of (Node);
      Node_Unparser : Node_Unparser_Impl renames
        Unparsers.Node_Unparsers (To_Index (Node_Type)).all;
   begin
      Fragments.Clear;

      case Node_Unparser.Kind is
         when Regular =>
            --  Append fragments that precede the first field

            Append (Node_Unparser.Pre_Tokens);

            --  Then append fragments for each field and the tokens between
            --  them.

            declare
               Node_Members : constant Struct_Member_Ref_Array :=
                 Members (Node_Type);
               I            : Positive := 1;
               Child        : Lk_Node;
               FU           : Field_Unparser;
            begin
               for Member of Node_Members loop
                  if Is_Field (Member)
                     and then not Is_Null_For (Member, Node_Type)
                  then
                     Child := Node.Child (I);
                     FU :=
                       Node_Unparser.Field_Unparsers
                       .Field_Unparsers (I)'Access;

                     --  Append fragments that appear unconditionally between
                     --  fields.

                     Append (Node_Unparser.Field_Unparsers.Inter_Tokens (I));

                     --  Then append fragments for the field itself, if
                     --  present. Note that unparsing tables
                     --  (Empty_List_Is_Absent component) determine whether
                     --  a non-null child with no children of its own must be
                     --  treated as absent.

                     if not Child.Is_Null
                        and then (not FU.Empty_List_Is_Absent
                                  or else Child.Children_Count > 0)
                     then
                        Append (FU.Pre_Tokens);
                        Fragments.Append
                          ((Kind  => Field_Fragment,
                            Node  => Child,
                            Field => Member));
                        Append (FU.Post_Tokens);
                     end if;

                     I := I + 1;
                  end if;
               end loop;
            end;

            --  Append fragments that follow the last field

            Append (Node_Unparser.Post_Tokens);

         when List =>
            for I in 1 .. Node.Children_Count loop
               if I > 1 and then Node_Unparser.Separator /= null then
                  Append (Node_Unparser.Separator);
               end if;

               Fragments.Append
                 ((Kind        => List_Child_Fragment,
                   Node        => Node.Child (I),
                   Child_Index => I));
            end loop;

         when Token =>
            Fragments.Append
              ((Kind       => Token_Fragment,
                Token_Kind => Token_Node_Kind (Node_Type),
                Token_Text => To_Unbounded_Text (Node.Text)));
      end case;
   end Unparse_To_Fragments;

end Langkit_Support.Generic_API.Unparsing;
