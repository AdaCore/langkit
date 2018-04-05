## vim: filetype=makoada

<%namespace name="unparsers"    file="unparsers_ada.mako" />

<% concrete_astnodes = [astnode for astnode in ctx.astnode_types
                        if not astnode.abstract] %>

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.Iconv;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

with ${ada_lib_name}.Lexer; use ${ada_lib_name}.Lexer;

package body ${ada_lib_name}.Unparsing.Implementation is

   % if ctx.generate_unparser:
      procedure Unparse_Dispatch
        (Node                : access Abstract_Node_Type'Class;
         Preserve_Formatting : Boolean;
         Result              : in out Unbounded_Wide_Wide_String);
      --  Dispatch over Node's kind and call the corresponding unparsing
      --  procedure.

      % for astnode in concrete_astnodes:
         procedure Unparse_${astnode.name}
           (Node                : access Abstract_Node_Type'Class;
            Preserve_Formatting : Boolean;
            Result              : in out Unbounded_Wide_Wide_String);
      % endfor
   % endif

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : access Abstract_Node_Type'Class;
      Unit                : Analysis_Unit;
      Preserve_Formatting : Boolean) return String
   is
      Result : String_Access := Unparse (Node, Unit, Preserve_Formatting);
      R      : constant String := Result.all;
   begin
      Free (Result);
      return R;
   end Unparse;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : access Abstract_Node_Type'Class;
      Unit                : Analysis_Unit;
      Preserve_Formatting : Boolean) return String_Access
   is
      Buffer : constant Text_Type := Unparse (Node, Preserve_Formatting);
   begin
      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.
      if Buffer'Length = 0 then
         return new String'("");
      end if;

      declare
         use GNATCOLL.Iconv;

         State  : Iconv_T := Iconv_Open
           (To_Code   => Get_Charset (Unit),
            From_Code => Internal_Charset);
         Status : Iconv_Result;

         To_Convert_String : constant String (1 .. 4 * Buffer'Length)
            with Import     => True,
                 Convention => Ada,
                 Address    => Buffer'Address;

         Output_Buffer : String_Access :=
            new String (1 .. 4 * To_Convert_String'Length);
         --  Encodings should not take more than 4 bytes per code point, so
         --  this should be enough to hold the conversion.

         Input_Index  : Positive := To_Convert_String'First;
         Output_Index : Positive := Output_Buffer'First;
      begin
         --  TODO??? Use GNATCOLL.Iconv to properly encode this wide wide
         --  string into a mere string using this unit's charset.
         Iconv
           (State, To_Convert_String, Input_Index, Output_Buffer.all,
            Output_Index, Status);
         Iconv_Close (State);
         case Status is
            when Success => null;
            when others => raise Program_Error with "cannot encode result";
         end case;

         declare
            Result_Slice : String renames
               Output_Buffer (Output_Buffer'First ..  Output_Index - 1);
            Result : constant String_Access :=
               new String (Result_Slice'Range);
         begin
            Result.all := Result_Slice;
            Free (Output_Buffer);
            return Result;
         end;
      end;
   end Unparse;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : access Abstract_Node_Type'Class;
      Preserve_Formatting : Boolean) return Text_Type
   is
      Buffer : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   begin
      % if ctx.generate_unparser:
         if Node = null then
            return (raise Program_Error with "cannot unparse null node");
         end if;

         Unparse_Dispatch (Node, Preserve_Formatting, Buffer);
         return To_Wide_Wide_String (Buffer);
      % else:
         return (raise Program_Error with "Unparser not generated");
      % endif
   end Unparse;

   % if ctx.generate_unparser:

      ----------------------
      -- Unparse_Dispatch --
      ----------------------

      procedure Unparse_Dispatch
        (Node                : access Abstract_Node_Type'Class;
         Preserve_Formatting : Boolean;
         Result              : in out Unbounded_Wide_Wide_String) is
      begin
         case Node.Abstract_Kind is
            % for astnode in ctx.astnode_types:
               % if not astnode.abstract:
                  when ${astnode.ada_kind_name} =>
                     Unparse_${astnode.name}
                       (Node, Preserve_Formatting, Result);
               % endif
            % endfor
         end case;
      end Unparse_Dispatch;

      % for astnode in concrete_astnodes:
         procedure Unparse_${astnode.name}
           (Node                : access Abstract_Node_Type'Class;
            Preserve_Formatting : Boolean;
            Result              : in out Unbounded_Wide_Wide_String)
         is
         begin
            % if astnode.parser:
               ${unparsers.emit_unparser_code(astnode.parser, astnode, 'Node')}
            % endif

            ## Just in case this unparser emits nothing, provide a null
            ## statement to yield legal Ada code.
            null;

            ## Maybe the above will generate references to the Node, Result and
            ## Preserve_Formatting arguments, but maybe not (and that's fine).
            ## To avoid "unreferenced" warnings, let's use this pragma. And to
            ## avoid "referenced in spite of pragma Unreferenced" warnings, put
            ## this pragma at the end of the procedure.
            pragma Unreferenced (Node, Result, Preserve_Formatting);
         end Unparse_${astnode.name};
      % endfor

   % endif

end ${ada_lib_name}.Unparsing.Implementation;
