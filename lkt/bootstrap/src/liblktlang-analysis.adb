







with Ada.Containers;            use Ada.Containers;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with GNATCOLL.Traces;

with Liblktlang_Support.Generic_API.Analysis;

pragma Warnings (Off, "referenced");
with Liblktlang_Support.Symbols; use Liblktlang_Support.Symbols;
pragma Warnings (On, "referenced");

with Liblktlang_Support.Types;        use Liblktlang_Support.Types;

with Liblktlang.Common;
with Liblktlang.Generic_API;       use Liblktlang.Generic_API;
with Liblktlang.Private_Converters;
use Liblktlang.Private_Converters;
with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;


          with Liblktlang.Implementation.Extensions;
            use Liblktlang.Implementation.Extensions;


package body Liblktlang.Analysis is

   use Liblktlang.Implementation;
   use AST_Envs;

        
      function To_Public_Lkt_Node_Array
         (Value : Internal_Entity_Array_Access) return Lkt_Node_Array;


        

        
      function To_Public_Logic_Context_Array
         (Value : Internal_Logic_Context_Array_Access) return Logic_Context_Array;


        

        
      function To_Public_Solver_Diagnostic_Array
         (Value : Internal_Solver_Diagnostic_Array_Access) return Solver_Diagnostic_Array;


        



         
      function To_Public_Decoded_Char_Value
        (Value : Internal_Decoded_Char_Value) return Decoded_Char_Value;


         
      function To_Public_Decoded_String_Value
        (Value : Internal_Decoded_String_Value) return Decoded_String_Value;


         
      function To_Public_Logic_Context
        (Value : Internal_Logic_Context) return Logic_Context;


         
      function To_Public_Solver_Diagnostic
        (Value : Internal_Solver_Diagnostic) return Solver_Diagnostic;


         
      function To_Public_Solver_Result
        (Value : Internal_Solver_Result) return Solver_Result;



   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Self : in out Event_Handler_Interface'Class) is
   begin
      Self.Release;
   end Do_Release;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class) is
   begin
      Provider.Release;
   end Do_Release;

   ------------------------------------
   -- Create_Unit_Provider_Reference --
   ------------------------------------

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference
   is
   begin
      return Result : Unit_Provider_Reference do
         Result.Set (Provider);
      end return;
   end Create_Unit_Provider_Reference;

   ------------------------------------
   -- Create_Event_Handler_Reference --
   ------------------------------------

   function Create_Event_Handler_Reference
     (Handler : Event_Handler_Interface'Class) return Event_Handler_Reference
   is
   begin
      return Result : Event_Handler_Reference do
         Result.Set (Handler);
      end return;
   end Create_Event_Handler_Reference;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset       : String := Default_Charset;
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      Event_Handler : Event_Handler_Reference := No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8)
      return Analysis_Context
   is
      use Unit_Provider_References;

      FR     : Internal_File_Reader_Access :=
         Wrap_Public_File_Reader (File_Reader);
      UP     : Internal_Unit_Provider_Access :=
         Wrap_Public_Provider (Unit_Provider);
      EH     : Internal_Event_Handler_Access :=
         Wrap_Public_Event_Handler (Event_Handler);
      Result : Internal_Context := Allocate_Context;
   begin
      Initialize_Context (Result, Charset, FR, UP, EH, With_Trivia, Tab_Stop);

      --  Create_Context created ownership shares for itself, so don't forget
      --  to remove the shares on FR and UP.
      Dec_Ref (FR);
      Dec_Ref (UP);
      Dec_Ref (EH);

      return Context : constant Analysis_Context := Wrap_Context (Result)
      do
         --  Result has one ownership share and the call to Wrap_Context
         --  creates a new one, so don't forget to dec-ref before returning.
         Dec_Ref (Result);
      end return;
   end Create_Context;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context       : Analysis_Context'Class;
      Unit_Filename : String) return Boolean is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Has_Unit (Unwrap_Context (Context), Unit_Filename);
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_File (Unwrap_Context (Context), Filename, Charset,
                        Reparse, Rule));
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Buffer, Rule));
   end Get_From_Buffer;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Bytes       : Big_String_Access;
      Bytes_Count : Natural;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      Get_String (Buffer, Bytes, Bytes_Count);
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Bytes (1 .. Bytes_Count), Rule));
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : Text_Type;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Result : Internal_Unit;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      Result := Implementation.Get_With_Error
        (Unwrap_Context (Context), Filename, Error, Charset, Rule);
      return Wrap_Unit (Result);
   end Get_With_Error;


   -----------------------
   -- Get_From_Provider --
   -----------------------

   function Get_From_Provider
     (Context : Analysis_Context'Class;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_Provider (Unwrap_Context (Context), Name, Kind,
                            Charset, Reparse));
   end Get_From_Provider;


   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference
   is
      Provider : Internal_Unit_Provider_Access;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      --  By design, Unit_Provider_Wrapper is supposed to be the only
      --  implementation of the Internal_Unit_Provider interface.
      Provider := Unit_Provider (Unwrap_Context (Context));
      if Provider.all not in Unit_Provider_Wrapper'Class then
         raise Program_Error;
      end if;

      return Unit_Provider_Wrapper (Provider.all).Internal;
   end Unit_Provider;

   ----------
   -- Hash --
   ----------

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Context (Context));
   end Hash;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean
   is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      return Has_With_Trivia (Unwrap_Context (Context));
   end Has_With_Trivia;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean) is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      Discard_Errors_In_Populate_Lexical_Env
        (Unwrap_Context (Context), Discard);
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural) is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      Set_Logic_Resolution_Timeout (Unwrap_Context (Context), Timeout);
   end Set_Logic_Resolution_Timeout;

   ---------------------------
   -- Set_Lookup_Cache_Mode --
   ---------------------------

   procedure Set_Lookup_Cache_Mode (Mode : Lookup_Cache_Kind) is
   begin
      Lookup_Cache_Mode := Mode;
   end Set_Lookup_Cache_Mode;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      return Has_Rewriting_Handle (Unwrap_Context (Context));
   end Has_Rewriting_Handle;

   -------------
   -- Context --
   -------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Wrap_Context (Context (Unwrap_Unit (Unit)));
   end Context;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Unit (Unit));
   end Hash;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "") is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Reparse (Unwrap_Unit (Unit), Charset);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit : Analysis_Unit'Class; Charset : String := ""; Buffer  : String) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Reparse (Unwrap_Unit (Unit), Charset, Buffer);
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env
     (Unit : Analysis_Unit'Class
     ) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Populate_Lexical_Env
        (Unwrap_Unit (Unit), 1);
   end Populate_Lexical_Env;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Analysis_Unit'Class) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Filename (Unwrap_Unit (Unit));
   end Get_Filename;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Analysis_Unit'Class) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Charset (Unwrap_Unit (Unit));
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Has_Diagnostics (Unwrap_Unit (Unit));
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array
   is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Implementation.Diagnostics (Unwrap_Unit (Unit));
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Format_GNU_Diagnostic (Unwrap_Unit (Unit), D);
   end Format_GNU_Diagnostic;

   ----------
   -- Root --
   ----------

   function Root (Unit : Analysis_Unit'Class) return Lkt_Node is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Wrap_Node (Root (Unwrap_Unit (Unit)));
   end Root;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return First_Token (Unwrap_Unit (Unit));
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Last_Token (Unwrap_Unit (Unit));
   end Last_Token;

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Token_Count (Unwrap_Unit (Unit));
   end Token_Count;

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Trivia_Count (Unwrap_Unit (Unit));
   end Trivia_Count;

   ----------
   -- Unit --
   ----------

   function Unit (Token : Token_Reference) return Analysis_Unit is
   begin
      return Wrap_Unit (Get_Token_Unit (Token));
   end Unit;

   ----------
   -- Text --
   ----------

   function Text (Unit : Analysis_Unit'Class) return Text_Type is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Implementation.Text (Unwrap_Unit (Unit));
   end Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Lookup_Token (Unwrap_Unit (Unit), Sloc);
   end Lookup_Token;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Unit : Analysis_Unit; Line_Number : Positive) return Text_Type is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Line (Unwrap_Unit (Unit), Line_Number);
   end Get_Line;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Dump_Lexical_Env (Unwrap_Unit (Unit));
   end Dump_Lexical_Env;

   ------------------------
   -- Trigger_Envs_Debug --
   ------------------------

   procedure Trigger_Envs_Debug (Is_Active : Boolean) is
   begin
      Liblktlang_Support.Lexical_Envs.Me.Set_Active (Is_Active);
   end Trigger_Envs_Debug;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True)
   is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Print (Unwrap_Unit (Unit), Show_Slocs);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      PP_Trivia (Unwrap_Unit (Unit));
   end PP_Trivia;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Lkt_Node'Class) return Boolean is
     (Node.Internal.Node = null);

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : Lkt_Node'Class) return Boolean
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Is_Token_Node (Node.Internal.Node);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : Lkt_Node'Class) return Boolean
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Is_Synthetic (Node.Internal.Node);
   end Is_Synthetic;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Lkt_Node'Class) return Boolean is
   begin
      Check_Safety_Net (L);
      Check_Safety_Net (R);
      return Compare_Entity (L.Internal, R.Internal);
   end "=";

   ------------
   -- Equals --
   ------------

   function Equals (L, R : Lkt_Node) return Boolean is
   begin
      Check_Safety_Net (L);
      Check_Safety_Net (R);
      return Compare_Entity (L.Internal, R.Internal);
   end Equals;

   -----------
   -- Image --
   -----------

   function Image (Node : Lkt_Node'Class) return String is
   begin
      Check_Safety_Net (Node);
      return Image (Node.Internal);
   end Image;

   -----------------------
   -- Entity converters --
   -----------------------

      function As_Lkt_Node
        (Node : Lkt_Node'Class) return Lkt_Node
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lkt_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         

      end;
      function As_Expr
        (Node : Lkt_Node'Class) return Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Expr then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Expr";
         
            end if;
      end;
      function As_Any_Of
        (Node : Lkt_Node'Class) return Any_Of
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Any_Of;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Any_Of_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AnyOf";
         
            end if;
      end;
      function As_Lkt_Node_Base_List
        (Node : Lkt_Node'Class) return Lkt_Node_Base_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lkt_Node_Base_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lkt_Node_Base_List then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LktNodeBaseList";
         
            end if;
      end;
      function As_Expr_List
        (Node : Lkt_Node'Class) return Expr_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Expr_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[Expr]";
         
            end if;
      end;
      function As_Any_Of_List
        (Node : Lkt_Node'Class) return Any_Of_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Any_Of_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Any_Of_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AnyOfList";
         
            end if;
      end;
      function As_Decl
        (Node : Lkt_Node'Class) return Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Decl";
         
            end if;
      end;
      function As_Type_Decl
        (Node : Lkt_Node'Class) return Type_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Type_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TypeDecl";
         
            end if;
      end;
      function As_Any_Type_Decl
        (Node : Lkt_Node'Class) return Any_Type_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Any_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Any_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AnyTypeDecl";
         
            end if;
      end;
      function As_Argument
        (Node : Lkt_Node'Class) return Argument
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Argument;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Argument_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Argument";
         
            end if;
      end;
      function As_Argument_List
        (Node : Lkt_Node'Class) return Argument_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Argument_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Argument_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[Argument]";
         
            end if;
      end;
      function As_Array_Literal
        (Node : Lkt_Node'Class) return Array_Literal
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Array_Literal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Array_Literal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ArrayLiteral";
         
            end if;
      end;
      function As_Base_Call_Expr
        (Node : Lkt_Node'Class) return Base_Call_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Call_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Base_Call_Expr then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseCallExpr";
         
            end if;
      end;
      function As_Base_Grammar_Rule_Decl
        (Node : Lkt_Node'Class) return Base_Grammar_Rule_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Grammar_Rule_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Base_Grammar_Rule_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseGrammarRuleDecl";
         
            end if;
      end;
      function As_Base_Lexer_Case_Rule_Alt
        (Node : Lkt_Node'Class) return Base_Lexer_Case_Rule_Alt
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Lexer_Case_Rule_Alt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Base_Lexer_Case_Rule_Alt then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseLexerCaseRuleAlt";
         
            end if;
      end;
      function As_Base_Lexer_Case_Rule_Alt_List
        (Node : Lkt_Node'Class) return Base_Lexer_Case_Rule_Alt_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Lexer_Case_Rule_Alt_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Base_Lexer_Case_Rule_Alt_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[BaseLexerCaseRuleAlt]";
         
            end if;
      end;
      function As_Base_Match_Branch
        (Node : Lkt_Node'Class) return Base_Match_Branch
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Match_Branch;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Base_Match_Branch then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseMatchBranch";
         
            end if;
      end;
      function As_Base_Match_Branch_List
        (Node : Lkt_Node'Class) return Base_Match_Branch_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Match_Branch_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Base_Match_Branch_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[BaseMatchBranch]";
         
            end if;
      end;
      function As_Base_Pattern
        (Node : Lkt_Node'Class) return Base_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Base_Pattern then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BasePattern";
         
            end if;
      end;
      function As_Base_Pattern_List
        (Node : Lkt_Node'Class) return Base_Pattern_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Pattern_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Base_Pattern_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[BasePattern]";
         
            end if;
      end;
      function As_Base_Val_Decl
        (Node : Lkt_Node'Class) return Base_Val_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Val_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Base_Val_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseValDecl";
         
            end if;
      end;
      function As_Named_Type_Decl
        (Node : Lkt_Node'Class) return Named_Type_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Named_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Named_Type_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NamedTypeDecl";
         
            end if;
      end;
      function As_Basic_Class_Decl
        (Node : Lkt_Node'Class) return Basic_Class_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Basic_Class_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Basic_Class_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BasicClassDecl";
         
            end if;
      end;
      function As_Lit
        (Node : Lkt_Node'Class) return Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lit then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Lit";
         
            end if;
      end;
      function As_Big_Num_Lit
        (Node : Lkt_Node'Class) return Big_Num_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Big_Num_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Big_Num_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BigNumLit";
         
            end if;
      end;
      function As_Bin_Op
        (Node : Lkt_Node'Class) return Bin_Op
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Bin_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Bin_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BinOp";
         
            end if;
      end;
      function As_Binding_Pattern
        (Node : Lkt_Node'Class) return Binding_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Binding_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Binding_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BindingPattern";
         
            end if;
      end;
      function As_User_Val_Decl
        (Node : Lkt_Node'Class) return User_Val_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_User_Val_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_User_Val_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to UserValDecl";
         
            end if;
      end;
      function As_Binding_Val_Decl
        (Node : Lkt_Node'Class) return Binding_Val_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Binding_Val_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Binding_Val_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BindingValDecl";
         
            end if;
      end;
      function As_Lkt_Node_List
        (Node : Lkt_Node'Class) return Lkt_Node_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lkt_Node_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lkt_Node_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[LktNode]";
         
            end if;
      end;
      function As_Block_Decl_List
        (Node : Lkt_Node'Class) return Block_Decl_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Block_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Block_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BlockDeclList";
         
            end if;
      end;
      function As_Block_Expr
        (Node : Lkt_Node'Class) return Block_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Block_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Block_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BlockExpr";
         
            end if;
      end;
      function As_Block_String_Line
        (Node : Lkt_Node'Class) return Block_String_Line
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Block_String_Line;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Block_String_Line_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BlockStringLine";
         
            end if;
      end;
      function As_Block_String_Line_List
        (Node : Lkt_Node'Class) return Block_String_Line_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Block_String_Line_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Block_String_Line_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[BlockStringLine]";
         
            end if;
      end;
      function As_String_Lit
        (Node : Lkt_Node'Class) return String_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_String_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_String_Lit then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to StringLit";
         
            end if;
      end;
      function As_Block_String_Lit
        (Node : Lkt_Node'Class) return Block_String_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Block_String_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Block_String_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BlockStringLit";
         
            end if;
      end;
      function As_Value_Pattern
        (Node : Lkt_Node'Class) return Value_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Value_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Value_Pattern then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ValuePattern";
         
            end if;
      end;
      function As_Bool_Pattern
        (Node : Lkt_Node'Class) return Bool_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Bool_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Bool_Pattern then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BoolPattern";
         
            end if;
      end;
      function As_Bool_Pattern_False
        (Node : Lkt_Node'Class) return Bool_Pattern_False
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Bool_Pattern_False;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Bool_Pattern_False_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BoolPattern.False";
         
            end if;
      end;
      function As_Bool_Pattern_True
        (Node : Lkt_Node'Class) return Bool_Pattern_True
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Bool_Pattern_True;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Bool_Pattern_True_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BoolPattern.True";
         
            end if;
      end;
      function As_Call_Expr
        (Node : Lkt_Node'Class) return Call_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Call_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Call_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CallExpr";
         
            end if;
      end;
      function As_Call_Expr_List
        (Node : Lkt_Node'Class) return Call_Expr_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Call_Expr_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Call_Expr_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[CallExpr]";
         
            end if;
      end;
      function As_Cast_Expr
        (Node : Lkt_Node'Class) return Cast_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Cast_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Cast_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CastExpr";
         
            end if;
      end;
      function As_Char_Lit
        (Node : Lkt_Node'Class) return Char_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Char_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Char_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CharLit";
         
            end if;
      end;
      function As_Class_Decl
        (Node : Lkt_Node'Class) return Class_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Class_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Class_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ClassDecl";
         
            end if;
      end;
      function As_Class_Qualifier
        (Node : Lkt_Node'Class) return Class_Qualifier
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Class_Qualifier;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Class_Qualifier then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ClassQualifier";
         
            end if;
      end;
      function As_Class_Qualifier_Absent
        (Node : Lkt_Node'Class) return Class_Qualifier_Absent
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Class_Qualifier_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Class_Qualifier_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ClassQualifier.Absent";
         
            end if;
      end;
      function As_Class_Qualifier_Present
        (Node : Lkt_Node'Class) return Class_Qualifier_Present
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Class_Qualifier_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Class_Qualifier_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ClassQualifier.Present";
         
            end if;
      end;
      function As_Explicitly_Typed_Decl
        (Node : Lkt_Node'Class) return Explicitly_Typed_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Explicitly_Typed_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Explicitly_Typed_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExplicitlyTypedDecl";
         
            end if;
      end;
      function As_Component_Decl
        (Node : Lkt_Node'Class) return Component_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Component_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Component_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ComponentDecl";
         
            end if;
      end;
      function As_Decl_Annotation
        (Node : Lkt_Node'Class) return Decl_Annotation
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decl_Annotation;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Decl_Annotation_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DeclAnnotation";
         
            end if;
      end;
      function As_Decl_Annotation_Args
        (Node : Lkt_Node'Class) return Decl_Annotation_Args
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decl_Annotation_Args;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Decl_Annotation_Args_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DeclAnnotationArgs";
         
            end if;
      end;
      function As_Decl_Annotation_List
        (Node : Lkt_Node'Class) return Decl_Annotation_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decl_Annotation_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Decl_Annotation_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[DeclAnnotation]";
         
            end if;
      end;
      function As_Full_Decl_List
        (Node : Lkt_Node'Class) return Full_Decl_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Full_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Full_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[FullDecl]";
         
            end if;
      end;
      function As_Decl_Block
        (Node : Lkt_Node'Class) return Decl_Block
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decl_Block;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Decl_Block_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DeclBlock";
         
            end if;
      end;
      function As_Id
        (Node : Lkt_Node'Class) return Id
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Id;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Id_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Id";
         
            end if;
      end;
      function As_Def_Id
        (Node : Lkt_Node'Class) return Def_Id
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Def_Id;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Def_Id_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DefId";
         
            end if;
      end;
      function As_Type_Ref
        (Node : Lkt_Node'Class) return Type_Ref
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Type_Ref then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TypeRef";
         
            end if;
      end;
      function As_Default_List_Type_Ref
        (Node : Lkt_Node'Class) return Default_List_Type_Ref
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Default_List_Type_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Default_List_Type_Ref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DefaultListTypeRef";
         
            end if;
      end;
      function As_Dot_Expr
        (Node : Lkt_Node'Class) return Dot_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dot_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Dot_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DotExpr";
         
            end if;
      end;
      function As_Dyn_Env_Wrapper
        (Node : Lkt_Node'Class) return Dyn_Env_Wrapper
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dyn_Env_Wrapper;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Dyn_Env_Wrapper_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DynEnvWrapper";
         
            end if;
      end;
      function As_Dyn_Var_Decl
        (Node : Lkt_Node'Class) return Dyn_Var_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dyn_Var_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Dyn_Var_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DynVarDecl";
         
            end if;
      end;
      function As_Elsif_Branch
        (Node : Lkt_Node'Class) return Elsif_Branch
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Elsif_Branch;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Elsif_Branch_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ElsifBranch";
         
            end if;
      end;
      function As_Elsif_Branch_List
        (Node : Lkt_Node'Class) return Elsif_Branch_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Elsif_Branch_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Elsif_Branch_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[ElsifBranch]";
         
            end if;
      end;
      function As_Enum_Class_Alt_Decl
        (Node : Lkt_Node'Class) return Enum_Class_Alt_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Class_Alt_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Enum_Class_Alt_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumClassAltDecl";
         
            end if;
      end;
      function As_Enum_Class_Alt_Decl_List
        (Node : Lkt_Node'Class) return Enum_Class_Alt_Decl_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Class_Alt_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Enum_Class_Alt_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[EnumClassAltDecl]";
         
            end if;
      end;
      function As_Enum_Class_Case
        (Node : Lkt_Node'Class) return Enum_Class_Case
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Class_Case;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Enum_Class_Case_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumClassCase";
         
            end if;
      end;
      function As_Enum_Class_Case_List
        (Node : Lkt_Node'Class) return Enum_Class_Case_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Class_Case_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Enum_Class_Case_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[EnumClassCase]";
         
            end if;
      end;
      function As_Enum_Class_Decl
        (Node : Lkt_Node'Class) return Enum_Class_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Class_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Enum_Class_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumClassDecl";
         
            end if;
      end;
      function As_Enum_Lit_Decl
        (Node : Lkt_Node'Class) return Enum_Lit_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Lit_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Enum_Lit_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumLitDecl";
         
            end if;
      end;
      function As_Enum_Lit_Decl_List
        (Node : Lkt_Node'Class) return Enum_Lit_Decl_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Lit_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Enum_Lit_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[EnumLitDecl]";
         
            end if;
      end;
      function As_Enum_Type_Decl
        (Node : Lkt_Node'Class) return Enum_Type_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Enum_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumTypeDecl";
         
            end if;
      end;
      function As_Env_Spec_Decl
        (Node : Lkt_Node'Class) return Env_Spec_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Env_Spec_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Env_Spec_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnvSpecDecl";
         
            end if;
      end;
      function As_Error_On_Null
        (Node : Lkt_Node'Class) return Error_On_Null
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Error_On_Null;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Error_On_Null_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ErrorOnNull";
         
            end if;
      end;
      function As_Excludes_Null
        (Node : Lkt_Node'Class) return Excludes_Null
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Excludes_Null;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Excludes_Null then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExcludesNull";
         
            end if;
      end;
      function As_Excludes_Null_Absent
        (Node : Lkt_Node'Class) return Excludes_Null_Absent
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Excludes_Null_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Excludes_Null_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExcludesNull.Absent";
         
            end if;
      end;
      function As_Excludes_Null_Present
        (Node : Lkt_Node'Class) return Excludes_Null_Present
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Excludes_Null_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Excludes_Null_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExcludesNull.Present";
         
            end if;
      end;
      function As_Node_Pattern
        (Node : Lkt_Node'Class) return Node_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Node_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Node_Pattern then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NodePattern";
         
            end if;
      end;
      function As_Extended_Node_Pattern
        (Node : Lkt_Node'Class) return Extended_Node_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Extended_Node_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Extended_Node_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExtendedNodePattern";
         
            end if;
      end;
      function As_Field_Decl
        (Node : Lkt_Node'Class) return Field_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Field_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Field_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to FieldDecl";
         
            end if;
      end;
      function As_Filtered_Pattern
        (Node : Lkt_Node'Class) return Filtered_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Filtered_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Filtered_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to FilteredPattern";
         
            end if;
      end;
      function As_Full_Decl
        (Node : Lkt_Node'Class) return Full_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Full_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Full_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to FullDecl";
         
            end if;
      end;
      function As_Fun_Decl
        (Node : Lkt_Node'Class) return Fun_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Fun_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Fun_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to FunDecl";
         
            end if;
      end;
      function As_Fun_Param_Decl
        (Node : Lkt_Node'Class) return Fun_Param_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Fun_Param_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Fun_Param_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to FunParamDecl";
         
            end if;
      end;
      function As_Fun_Param_Decl_List
        (Node : Lkt_Node'Class) return Fun_Param_Decl_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Fun_Param_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Fun_Param_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[FunParamDecl]";
         
            end if;
      end;
      function As_Function_Type
        (Node : Lkt_Node'Class) return Function_Type
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Function_Type;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Function_Type_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to FunctionType";
         
            end if;
      end;
      function As_Function_Type_Ref
        (Node : Lkt_Node'Class) return Function_Type_Ref
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Function_Type_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Function_Type_Ref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to FunctionTypeRef";
         
            end if;
      end;
      function As_Generic_Decl
        (Node : Lkt_Node'Class) return Generic_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Generic_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericDecl";
         
            end if;
      end;
      function As_Generic_Instantiation
        (Node : Lkt_Node'Class) return Generic_Instantiation
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Instantiation;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Generic_Instantiation_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericInstantiation";
         
            end if;
      end;
      function As_Generic_Param_Decl_List
        (Node : Lkt_Node'Class) return Generic_Param_Decl_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Param_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Generic_Param_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericParamDeclList";
         
            end if;
      end;
      function As_Generic_Param_Type_Decl
        (Node : Lkt_Node'Class) return Generic_Param_Type_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Param_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Generic_Param_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericParamTypeDecl";
         
            end if;
      end;
      function As_Generic_Type_Ref
        (Node : Lkt_Node'Class) return Generic_Type_Ref
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Type_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Generic_Type_Ref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericTypeRef";
         
            end if;
      end;
      function As_Grammar_Expr
        (Node : Lkt_Node'Class) return Grammar_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Expr then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarExpr";
         
            end if;
      end;
      function As_Grammar_Cut
        (Node : Lkt_Node'Class) return Grammar_Cut
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Cut;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Cut_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarCut";
         
            end if;
      end;
      function As_Grammar_Decl
        (Node : Lkt_Node'Class) return Grammar_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarDecl";
         
            end if;
      end;
      function As_Grammar_Discard
        (Node : Lkt_Node'Class) return Grammar_Discard
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Discard;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Discard_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarDiscard";
         
            end if;
      end;
      function As_Grammar_Dont_Skip
        (Node : Lkt_Node'Class) return Grammar_Dont_Skip
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Dont_Skip;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Dont_Skip_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarDontSkip";
         
            end if;
      end;
      function As_Grammar_Expr_List
        (Node : Lkt_Node'Class) return Grammar_Expr_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Expr_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Expr_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[GrammarExpr]";
         
            end if;
      end;
      function As_Grammar_Expr_List_List
        (Node : Lkt_Node'Class) return Grammar_Expr_List_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Expr_List_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Expr_List_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[ASTList[GrammarExpr]]";
         
            end if;
      end;
      function As_Grammar_Pick
        (Node : Lkt_Node'Class) return Grammar_Pick
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Pick;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Pick_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarPick";
         
            end if;
      end;
      function As_Grammar_Implicit_Pick
        (Node : Lkt_Node'Class) return Grammar_Implicit_Pick
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Implicit_Pick;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Implicit_Pick_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarImplicitPick";
         
            end if;
      end;
      function As_Grammar_List
        (Node : Lkt_Node'Class) return Grammar_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarList";
         
            end if;
      end;
      function As_Grammar_List_Sep
        (Node : Lkt_Node'Class) return Grammar_List_Sep
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_List_Sep;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_List_Sep_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarListSep";
         
            end if;
      end;
      function As_Grammar_Null
        (Node : Lkt_Node'Class) return Grammar_Null
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Null;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Null_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarNull";
         
            end if;
      end;
      function As_Grammar_Opt
        (Node : Lkt_Node'Class) return Grammar_Opt
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Opt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Opt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarOpt";
         
            end if;
      end;
      function As_Grammar_Opt_Error
        (Node : Lkt_Node'Class) return Grammar_Opt_Error
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Opt_Error;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Opt_Error_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarOptError";
         
            end if;
      end;
      function As_Grammar_Opt_Error_Group
        (Node : Lkt_Node'Class) return Grammar_Opt_Error_Group
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Opt_Error_Group;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Opt_Error_Group_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarOptErrorGroup";
         
            end if;
      end;
      function As_Grammar_Opt_Group
        (Node : Lkt_Node'Class) return Grammar_Opt_Group
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Opt_Group;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Opt_Group_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarOptGroup";
         
            end if;
      end;
      function As_Grammar_Or_Expr
        (Node : Lkt_Node'Class) return Grammar_Or_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Or_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Or_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarOrExpr";
         
            end if;
      end;
      function As_Grammar_Predicate
        (Node : Lkt_Node'Class) return Grammar_Predicate
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Predicate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Predicate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarPredicate";
         
            end if;
      end;
      function As_Grammar_Rule_Decl
        (Node : Lkt_Node'Class) return Grammar_Rule_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Rule_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Rule_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarRuleDecl";
         
            end if;
      end;
      function As_Grammar_Rule_Ref
        (Node : Lkt_Node'Class) return Grammar_Rule_Ref
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Rule_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Rule_Ref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarRuleRef";
         
            end if;
      end;
      function As_Grammar_Skip
        (Node : Lkt_Node'Class) return Grammar_Skip
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Skip;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Skip_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarSkip";
         
            end if;
      end;
      function As_Grammar_Stop_Cut
        (Node : Lkt_Node'Class) return Grammar_Stop_Cut
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Grammar_Stop_Cut;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Grammar_Stop_Cut_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GrammarStopCut";
         
            end if;
      end;
      function As_If_Expr
        (Node : Lkt_Node'Class) return If_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_If_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_If_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to IfExpr";
         
            end if;
      end;
      function As_Import
        (Node : Lkt_Node'Class) return Import
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Import;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Import_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Import";
         
            end if;
      end;
      function As_Import_List
        (Node : Lkt_Node'Class) return Import_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Import_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Import_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[Import]";
         
            end if;
      end;
      function As_Integer_Pattern
        (Node : Lkt_Node'Class) return Integer_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Integer_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Integer_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to IntegerPattern";
         
            end if;
      end;
      function As_Isa
        (Node : Lkt_Node'Class) return Isa
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Isa;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Isa_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Isa";
         
            end if;
      end;
      function As_Keep_Expr
        (Node : Lkt_Node'Class) return Keep_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Keep_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Keep_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to KeepExpr";
         
            end if;
      end;
      function As_Lambda_Expr
        (Node : Lkt_Node'Class) return Lambda_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lambda_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lambda_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LambdaExpr";
         
            end if;
      end;
      function As_Lambda_Param_Decl
        (Node : Lkt_Node'Class) return Lambda_Param_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lambda_Param_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lambda_Param_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LambdaParamDecl";
         
            end if;
      end;
      function As_Lambda_Param_Decl_List
        (Node : Lkt_Node'Class) return Lambda_Param_Decl_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lambda_Param_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lambda_Param_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[LambdaParamDecl]";
         
            end if;
      end;
      function As_Langkit_Root
        (Node : Lkt_Node'Class) return Langkit_Root
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Langkit_Root;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Langkit_Root_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LangkitRoot";
         
            end if;
      end;
      function As_Lexer_Case_Rule
        (Node : Lkt_Node'Class) return Lexer_Case_Rule
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lexer_Case_Rule;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lexer_Case_Rule_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LexerCaseRule";
         
            end if;
      end;
      function As_Lexer_Case_Rule_Cond_Alt
        (Node : Lkt_Node'Class) return Lexer_Case_Rule_Cond_Alt
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lexer_Case_Rule_Cond_Alt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lexer_Case_Rule_Cond_Alt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LexerCaseRuleCondAlt";
         
            end if;
      end;
      function As_Lexer_Case_Rule_Default_Alt
        (Node : Lkt_Node'Class) return Lexer_Case_Rule_Default_Alt
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lexer_Case_Rule_Default_Alt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lexer_Case_Rule_Default_Alt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LexerCaseRuleDefaultAlt";
         
            end if;
      end;
      function As_Lexer_Case_Rule_Send
        (Node : Lkt_Node'Class) return Lexer_Case_Rule_Send
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lexer_Case_Rule_Send;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lexer_Case_Rule_Send_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LexerCaseRuleSend";
         
            end if;
      end;
      function As_Lexer_Decl
        (Node : Lkt_Node'Class) return Lexer_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lexer_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lexer_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LexerDecl";
         
            end if;
      end;
      function As_Lexer_Family_Decl
        (Node : Lkt_Node'Class) return Lexer_Family_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lexer_Family_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Lexer_Family_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LexerFamilyDecl";
         
            end if;
      end;
      function As_List_Kind
        (Node : Lkt_Node'Class) return List_Kind
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_List_Kind;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_List_Kind then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ListKind";
         
            end if;
      end;
      function As_List_Kind_One
        (Node : Lkt_Node'Class) return List_Kind_One
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_List_Kind_One;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_List_Kind_One_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ListKind.One";
         
            end if;
      end;
      function As_List_Kind_Zero
        (Node : Lkt_Node'Class) return List_Kind_Zero
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_List_Kind_Zero;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_List_Kind_Zero_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ListKind.Zero";
         
            end if;
      end;
      function As_List_Pattern
        (Node : Lkt_Node'Class) return List_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_List_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_List_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ListPattern";
         
            end if;
      end;
      function As_Logic_Assign
        (Node : Lkt_Node'Class) return Logic_Assign
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Logic_Assign;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Logic_Assign_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LogicAssign";
         
            end if;
      end;
      function As_Logic_Call_Expr
        (Node : Lkt_Node'Class) return Logic_Call_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Logic_Call_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Logic_Call_Expr then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LogicCallExpr";
         
            end if;
      end;
      function As_Logic_Expr
        (Node : Lkt_Node'Class) return Logic_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Logic_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Logic_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LogicExpr";
         
            end if;
      end;
      function As_Logic_Predicate
        (Node : Lkt_Node'Class) return Logic_Predicate
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Logic_Predicate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Logic_Predicate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LogicPredicate";
         
            end if;
      end;
      function As_Logic_Propagate
        (Node : Lkt_Node'Class) return Logic_Propagate
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Logic_Propagate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Logic_Propagate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LogicPropagate";
         
            end if;
      end;
      function As_Logic_Propagate_Call
        (Node : Lkt_Node'Class) return Logic_Propagate_Call
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Logic_Propagate_Call;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Logic_Propagate_Call_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LogicPropagateCall";
         
            end if;
      end;
      function As_Logic_Unify
        (Node : Lkt_Node'Class) return Logic_Unify
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Logic_Unify;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Logic_Unify_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LogicUnify";
         
            end if;
      end;
      function As_Match_Branch
        (Node : Lkt_Node'Class) return Match_Branch
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Match_Branch;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Match_Branch_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to MatchBranch";
         
            end if;
      end;
      function As_Match_Expr
        (Node : Lkt_Node'Class) return Match_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Match_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Match_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to MatchExpr";
         
            end if;
      end;
      function As_Match_Val_Decl
        (Node : Lkt_Node'Class) return Match_Val_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Match_Val_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Match_Val_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to MatchValDecl";
         
            end if;
      end;
      function As_Module_Ref_Id
        (Node : Lkt_Node'Class) return Module_Ref_Id
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Module_Ref_Id;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Module_Ref_Id_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ModuleRefId";
         
            end if;
      end;
      function As_Node_Decl
        (Node : Lkt_Node'Class) return Node_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Node_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Node_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NodeDecl";
         
            end if;
      end;
      function As_Node_Pattern_Detail
        (Node : Lkt_Node'Class) return Node_Pattern_Detail
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Node_Pattern_Detail;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Node_Pattern_Detail then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NodePatternDetail";
         
            end if;
      end;
      function As_Node_Pattern_Detail_List
        (Node : Lkt_Node'Class) return Node_Pattern_Detail_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Node_Pattern_Detail_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Node_Pattern_Detail_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[NodePatternDetail]";
         
            end if;
      end;
      function As_Node_Pattern_Field
        (Node : Lkt_Node'Class) return Node_Pattern_Field
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Node_Pattern_Field;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Node_Pattern_Field_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NodePatternField";
         
            end if;
      end;
      function As_Node_Pattern_Property
        (Node : Lkt_Node'Class) return Node_Pattern_Property
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Node_Pattern_Property;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Node_Pattern_Property_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NodePatternProperty";
         
            end if;
      end;
      function As_Node_Pattern_Selector
        (Node : Lkt_Node'Class) return Node_Pattern_Selector
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Node_Pattern_Selector;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Node_Pattern_Selector_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NodePatternSelector";
         
            end if;
      end;
      function As_Not_Expr
        (Node : Lkt_Node'Class) return Not_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Not_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Not_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NotExpr";
         
            end if;
      end;
      function As_Not_Pattern
        (Node : Lkt_Node'Class) return Not_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Not_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Not_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NotPattern";
         
            end if;
      end;
      function As_Null_Cond_Qualifier
        (Node : Lkt_Node'Class) return Null_Cond_Qualifier
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Cond_Qualifier;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Null_Cond_Qualifier then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullCondQualifier";
         
            end if;
      end;
      function As_Null_Cond_Qualifier_Absent
        (Node : Lkt_Node'Class) return Null_Cond_Qualifier_Absent
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Cond_Qualifier_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Null_Cond_Qualifier_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullCondQualifier.Absent";
         
            end if;
      end;
      function As_Null_Cond_Qualifier_Present
        (Node : Lkt_Node'Class) return Null_Cond_Qualifier_Present
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Cond_Qualifier_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Null_Cond_Qualifier_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullCondQualifier.Present";
         
            end if;
      end;
      function As_Null_Lit
        (Node : Lkt_Node'Class) return Null_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Null_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullLit";
         
            end if;
      end;
      function As_Null_Pattern
        (Node : Lkt_Node'Class) return Null_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Null_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullPattern";
         
            end if;
      end;
      function As_Num_Lit
        (Node : Lkt_Node'Class) return Num_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Num_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Num_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NumLit";
         
            end if;
      end;
      function As_Op
        (Node : Lkt_Node'Class) return Op
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op";
         
            end if;
      end;
      function As_Op_Amp
        (Node : Lkt_Node'Class) return Op_Amp
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Amp;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Amp_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Amp";
         
            end if;
      end;
      function As_Op_And
        (Node : Lkt_Node'Class) return Op_And
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_And;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_And_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.And";
         
            end if;
      end;
      function As_Op_Div
        (Node : Lkt_Node'Class) return Op_Div
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Div;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Div_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Div";
         
            end if;
      end;
      function As_Op_Eq
        (Node : Lkt_Node'Class) return Op_Eq
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Eq;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Eq_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Eq";
         
            end if;
      end;
      function As_Op_Gt
        (Node : Lkt_Node'Class) return Op_Gt
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Gt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Gt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Gt";
         
            end if;
      end;
      function As_Op_Gte
        (Node : Lkt_Node'Class) return Op_Gte
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Gte;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Gte_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Gte";
         
            end if;
      end;
      function As_Op_Logic_And
        (Node : Lkt_Node'Class) return Op_Logic_And
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Logic_And;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Logic_And_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.LogicAnd";
         
            end if;
      end;
      function As_Op_Logic_Or
        (Node : Lkt_Node'Class) return Op_Logic_Or
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Logic_Or;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Logic_Or_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.LogicOr";
         
            end if;
      end;
      function As_Op_Lt
        (Node : Lkt_Node'Class) return Op_Lt
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Lt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Lt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Lt";
         
            end if;
      end;
      function As_Op_Lte
        (Node : Lkt_Node'Class) return Op_Lte
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Lte;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Lte_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Lte";
         
            end if;
      end;
      function As_Op_Minus
        (Node : Lkt_Node'Class) return Op_Minus
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Minus;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Minus_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Minus";
         
            end if;
      end;
      function As_Op_Mult
        (Node : Lkt_Node'Class) return Op_Mult
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Mult;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Mult_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Mult";
         
            end if;
      end;
      function As_Op_Ne
        (Node : Lkt_Node'Class) return Op_Ne
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Ne;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Ne_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Ne";
         
            end if;
      end;
      function As_Op_Or
        (Node : Lkt_Node'Class) return Op_Or
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Or;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Or_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Or";
         
            end if;
      end;
      function As_Op_Or_Int
        (Node : Lkt_Node'Class) return Op_Or_Int
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Or_Int;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Or_Int_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.OrInt";
         
            end if;
      end;
      function As_Op_Plus
        (Node : Lkt_Node'Class) return Op_Plus
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Plus;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Op_Plus_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Plus";
         
            end if;
      end;
      function As_Or_Pattern
        (Node : Lkt_Node'Class) return Or_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Or_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Or_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to OrPattern";
         
            end if;
      end;
      function As_Paren_Expr
        (Node : Lkt_Node'Class) return Paren_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Paren_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Paren_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ParenExpr";
         
            end if;
      end;
      function As_Paren_Pattern
        (Node : Lkt_Node'Class) return Paren_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Paren_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Paren_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ParenPattern";
         
            end if;
      end;
      function As_Parse_Node_Expr
        (Node : Lkt_Node'Class) return Parse_Node_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Parse_Node_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Parse_Node_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ParseNodeExpr";
         
            end if;
      end;
      function As_Pattern_Match_Branch
        (Node : Lkt_Node'Class) return Pattern_Match_Branch
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pattern_Match_Branch;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Pattern_Match_Branch_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to PatternMatchBranch";
         
            end if;
      end;
      function As_Single_Line_String_Lit
        (Node : Lkt_Node'Class) return Single_Line_String_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Single_Line_String_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Single_Line_String_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SingleLineStringLit";
         
            end if;
      end;
      function As_Pattern_Single_Line_String_Lit
        (Node : Lkt_Node'Class) return Pattern_Single_Line_String_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pattern_Single_Line_String_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Pattern_Single_Line_String_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to PatternSingleLineStringLit";
         
            end if;
      end;
      function As_Raise_Expr
        (Node : Lkt_Node'Class) return Raise_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Raise_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Raise_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to RaiseExpr";
         
            end if;
      end;
      function As_Ref_Id
        (Node : Lkt_Node'Class) return Ref_Id
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ref_Id;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Ref_Id_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to RefId";
         
            end if;
      end;
      function As_Ref_Id_List
        (Node : Lkt_Node'Class) return Ref_Id_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ref_Id_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Ref_Id_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[RefId]";
         
            end if;
      end;
      function As_Regex_Pattern
        (Node : Lkt_Node'Class) return Regex_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Regex_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Regex_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to RegexPattern";
         
            end if;
      end;
      function As_Selector_Call
        (Node : Lkt_Node'Class) return Selector_Call
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Selector_Call;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Selector_Call_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SelectorCall";
         
            end if;
      end;
      function As_Self_Decl
        (Node : Lkt_Node'Class) return Self_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Self_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Self_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SelfDecl";
         
            end if;
      end;
      function As_Simple_Type_Ref
        (Node : Lkt_Node'Class) return Simple_Type_Ref
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Simple_Type_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Simple_Type_Ref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SimpleTypeRef";
         
            end if;
      end;
      function As_Splat_Pattern
        (Node : Lkt_Node'Class) return Splat_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Splat_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Splat_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SplatPattern";
         
            end if;
      end;
      function As_Struct_Decl
        (Node : Lkt_Node'Class) return Struct_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Struct_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Struct_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to StructDecl";
         
            end if;
      end;
      function As_Subscript_Expr
        (Node : Lkt_Node'Class) return Subscript_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subscript_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Subscript_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubscriptExpr";
         
            end if;
      end;
      function As_Synth_Fun_Decl
        (Node : Lkt_Node'Class) return Synth_Fun_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synth_Fun_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Synth_Fun_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SynthFunDecl";
         
            end if;
      end;
      function As_Synth_Param_Decl
        (Node : Lkt_Node'Class) return Synth_Param_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synth_Param_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Synth_Param_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SynthParamDecl";
         
            end if;
      end;
      function As_Synthetic_Lexer_Decl
        (Node : Lkt_Node'Class) return Synthetic_Lexer_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Lexer_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Synthetic_Lexer_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticLexerDecl";
         
            end if;
      end;
      function As_Type_Ref_List
        (Node : Lkt_Node'Class) return Type_Ref_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Ref_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Type_Ref_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ASTList[TypeRef]";
         
            end if;
      end;
      function As_Synthetic_Type_Ref_List
        (Node : Lkt_Node'Class) return Synthetic_Type_Ref_List
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Type_Ref_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Synthetic_Type_Ref_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticTypeRefList";
         
            end if;
      end;
      function As_Token_Lit
        (Node : Lkt_Node'Class) return Token_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Token_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Token_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TokenLit";
         
            end if;
      end;
      function As_Token_No_Case_Lit
        (Node : Lkt_Node'Class) return Token_No_Case_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Token_No_Case_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Token_No_Case_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TokenNoCaseLit";
         
            end if;
      end;
      function As_Token_Pattern_Concat
        (Node : Lkt_Node'Class) return Token_Pattern_Concat
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Token_Pattern_Concat;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Token_Pattern_Concat_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TokenPatternConcat";
         
            end if;
      end;
      function As_Token_Pattern_Lit
        (Node : Lkt_Node'Class) return Token_Pattern_Lit
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Token_Pattern_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Token_Pattern_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TokenPatternLit";
         
            end if;
      end;
      function As_Token_Ref
        (Node : Lkt_Node'Class) return Token_Ref
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Token_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Token_Ref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TokenRef";
         
            end if;
      end;
      function As_Trait_Decl
        (Node : Lkt_Node'Class) return Trait_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Trait_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Trait_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TraitDecl";
         
            end if;
      end;
      function As_Try_Expr
        (Node : Lkt_Node'Class) return Try_Expr
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Try_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Try_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TryExpr";
         
            end if;
      end;
      function As_Tuple_Pattern
        (Node : Lkt_Node'Class) return Tuple_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Tuple_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Tuple_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TuplePattern";
         
            end if;
      end;
      function As_Type_Pattern
        (Node : Lkt_Node'Class) return Type_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Type_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TypePattern";
         
            end if;
      end;
      function As_Un_Op
        (Node : Lkt_Node'Class) return Un_Op
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Un_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Un_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to UnOp";
         
            end if;
      end;
      function As_Universal_Pattern
        (Node : Lkt_Node'Class) return Universal_Pattern
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Universal_Pattern;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Universal_Pattern_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to UniversalPattern";
         
            end if;
      end;
      function As_Val_Decl
        (Node : Lkt_Node'Class) return Val_Decl
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Val_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Val_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ValDecl";
         
            end if;
      end;
      function As_Var_Bind
        (Node : Lkt_Node'Class) return Var_Bind
      is
         N : constant Bare_Lkt_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Var_Bind;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Lkt_Var_Bind_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Liblktlang: invalid type conversion from "
              & Node.Kind_Name
              & " to VarBind";
         
            end if;
      end;

   -----------------------
   -- Entity primitives --
   -----------------------

   ----------
   -- Hash --
   ----------

   function Hash
     (Node : Lkt_Node) return Ada.Containers.Hash_Type is
   begin
      Check_Safety_Net (Node);
      return Hash_Entity (Node.Internal);
   end Hash;

   ----------
   -- Kind --
   ----------

   function Kind (Node : Lkt_Node'Class) return Lkt_Node_Kind_Type
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Node.Internal.Node.Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : Lkt_Node'Class) return String is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Kind_Name (Node.Internal.Node);
   end Kind_Name;

      


      


      


      


      
      function To_Public_Lkt_Node_Array
         (Value : Internal_Entity_Array_Access) return Lkt_Node_Array is
      begin
         return Result : Lkt_Node_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
               ;
            end loop;
         end return;
      end;


      


      


      


      


      


      


      


      


      
      function To_Public_Logic_Context_Array
         (Value : Internal_Logic_Context_Array_Access) return Logic_Context_Array is
      begin
         return Result : Logic_Context_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := To_Public_Logic_Context (Value.Items (I))
               ;
            end loop;
         end return;
      end;


      


      


      
      function To_Public_Solver_Diagnostic_Array
         (Value : Internal_Solver_Diagnostic_Array_Access) return Solver_Diagnostic_Array is
      begin
         return Result : Solver_Diagnostic_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := To_Public_Solver_Diagnostic (Value.Items (I))
               ;
            end loop;
         end return;
      end;


      


      


      


      


      


      




         

      
   function Value
     (Self : Decoded_Char_Value)
      return Character_Type
 is
         Record_Ref : constant Boxed_Decoded_Char_Value.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Value
            ;
      end;
      
   function Has_Error
     (Self : Decoded_Char_Value)
      return Boolean
 is
         Record_Ref : constant Boxed_Decoded_Char_Value.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Has_Error
            ;
      end;
      
   function Error_Sloc
     (Self : Decoded_Char_Value)
      return Source_Location
 is
         Record_Ref : constant Boxed_Decoded_Char_Value.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Error_Sloc
            ;
      end;
      
   function Error_Message
     (Self : Decoded_Char_Value)
      return Text_Type
 is
         Record_Ref : constant Boxed_Decoded_Char_Value.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Error_Message
                  .all
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Decoded_Char_Value_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Decoded_Char_Value_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Internal_Decoded_Char_Value_Record) is
         
      begin
            Free (Self.Internal_Error_Message);
      end Release;

      function To_Public_Decoded_Char_Value
        (Value : Internal_Decoded_Char_Value) return Decoded_Char_Value
      is
         Result : constant Decoded_Char_Value :=
            Decoded_Char_Value (Boxed_Decoded_Char_Value.Create_Element);
         Record_Ref : constant Boxed_Decoded_Char_Value.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Value := Value.Value;
            
               Record_Ref.Internal_Has_Error := Value.Has_Error;
            
               Record_Ref.Internal_Error_Sloc := Value.Error_Sloc;
            
               Record_Ref.Internal_Error_Message := new Text_Type'(Value.Error_Message.Content);
         return Result;
      end;


   
   
   function Create_Decoded_Char_Value
     (Value : Character_Type; Has_Error : Boolean; Error_Sloc : Source_Location; Error_Message : Text_Type)
     return Decoded_Char_Value
 is
      Result     : constant Decoded_Char_Value :=
         Decoded_Char_Value (Boxed_Decoded_Char_Value.Create_Element);
      Record_Def : constant Boxed_Decoded_Char_Value.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Value := Value;
         
            Record_Def.Internal_Has_Error := Has_Error;
         
            Record_Def.Internal_Error_Sloc := Error_Sloc;
         
            Record_Def.Internal_Error_Message := new Text_Type'(Error_Message);
      return Result;
   end;

         

      
   function Value
     (Self : Decoded_String_Value)
      return Text_Type
 is
         Record_Ref : constant Boxed_Decoded_String_Value.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Value
                  .all
            ;
      end;
      
   function Has_Error
     (Self : Decoded_String_Value)
      return Boolean
 is
         Record_Ref : constant Boxed_Decoded_String_Value.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Has_Error
            ;
      end;
      
   function Error_Sloc
     (Self : Decoded_String_Value)
      return Source_Location
 is
         Record_Ref : constant Boxed_Decoded_String_Value.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Error_Sloc
            ;
      end;
      
   function Error_Message
     (Self : Decoded_String_Value)
      return Text_Type
 is
         Record_Ref : constant Boxed_Decoded_String_Value.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Error_Message
                  .all
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Decoded_String_Value_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Decoded_String_Value_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Internal_Decoded_String_Value_Record) is
         
      begin
            Free (Self.Internal_Value);
            Free (Self.Internal_Error_Message);
      end Release;

      function To_Public_Decoded_String_Value
        (Value : Internal_Decoded_String_Value) return Decoded_String_Value
      is
         Result : constant Decoded_String_Value :=
            Decoded_String_Value (Boxed_Decoded_String_Value.Create_Element);
         Record_Ref : constant Boxed_Decoded_String_Value.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Value := new Text_Type'(Value.Value.Content);
            
               Record_Ref.Internal_Has_Error := Value.Has_Error;
            
               Record_Ref.Internal_Error_Sloc := Value.Error_Sloc;
            
               Record_Ref.Internal_Error_Message := new Text_Type'(Value.Error_Message.Content);
         return Result;
      end;


   
   
   function Create_Decoded_String_Value
     (Value : Text_Type; Has_Error : Boolean; Error_Sloc : Source_Location; Error_Message : Text_Type)
     return Decoded_String_Value
 is
      Result     : constant Decoded_String_Value :=
         Decoded_String_Value (Boxed_Decoded_String_Value.Create_Element);
      Record_Def : constant Boxed_Decoded_String_Value.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Value := new Text_Type'(Value);
         
            Record_Def.Internal_Has_Error := Has_Error;
         
            Record_Def.Internal_Error_Sloc := Error_Sloc;
         
            Record_Def.Internal_Error_Message := new Text_Type'(Error_Message);
      return Result;
   end;

         

      
   function Ref_Node
     (Self : Logic_Context)
      return Lkt_Node'Class
 is
         Record_Ref : constant Boxed_Logic_Context.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Ref_Node
            ;
      end;
      
   function Decl_Node
     (Self : Logic_Context)
      return Lkt_Node'Class
 is
         Record_Ref : constant Boxed_Logic_Context.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Decl_Node
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Logic_Context_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Logic_Context_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;


      function To_Public_Logic_Context
        (Value : Internal_Logic_Context) return Logic_Context
      is
         Result : constant Logic_Context :=
            Logic_Context (Boxed_Logic_Context.Create_Element);
         Record_Ref : constant Boxed_Logic_Context.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Ref_Node := Wrap_Node (Value.Ref_Node.Node, Value.Ref_Node.Info);
            
               Record_Ref.Internal_Decl_Node := Wrap_Node (Value.Decl_Node.Node, Value.Decl_Node.Info);
         return Result;
      end;


   
   
   function Create_Logic_Context
     (Ref_Node : Lkt_Node'Class; Decl_Node : Lkt_Node'Class)
     return Logic_Context
 is
      Result     : constant Logic_Context :=
         Logic_Context (Boxed_Logic_Context.Create_Element);
      Record_Def : constant Boxed_Logic_Context.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Ref_Node := Ref_Node.As_Lkt_Node;
         
            Record_Def.Internal_Decl_Node := Decl_Node.As_Lkt_Node;
      return Result;
   end;

         

      
   function Message_Template
     (Self : Solver_Diagnostic)
      return Text_Type
 is
         Record_Ref : constant Boxed_Solver_Diagnostic.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Message_Template
                  .all
            ;
      end;
      
   function Args
     (Self : Solver_Diagnostic)
      return Lkt_Node_Array
 is
         Record_Ref : constant Boxed_Solver_Diagnostic.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Args
                  .all
            ;
      end;
      
   function Location
     (Self : Solver_Diagnostic)
      return Lkt_Node'Class
 is
         Record_Ref : constant Boxed_Solver_Diagnostic.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Location
            ;
      end;
      
   function Contexts
     (Self : Solver_Diagnostic)
      return Logic_Context_Array
 is
         Record_Ref : constant Boxed_Solver_Diagnostic.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Contexts
                  .all
            ;
      end;
      
   function Round
     (Self : Solver_Diagnostic)
      return Integer
 is
         Record_Ref : constant Boxed_Solver_Diagnostic.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Round
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Solver_Diagnostic_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Solver_Diagnostic_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Internal_Solver_Diagnostic_Record) is
         
      begin
            Free (Self.Internal_Message_Template);
            Free (Self.Internal_Args);
            Free (Self.Internal_Contexts);
      end Release;

      function To_Public_Solver_Diagnostic
        (Value : Internal_Solver_Diagnostic) return Solver_Diagnostic
      is
         Result : constant Solver_Diagnostic :=
            Solver_Diagnostic (Boxed_Solver_Diagnostic.Create_Element);
         Record_Ref : constant Boxed_Solver_Diagnostic.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Message_Template := new Text_Type'(Value.Message_Template.Content);
            
               Record_Ref.Internal_Args := new Lkt_Node_Array'(To_Public_Lkt_Node_Array (Value.Args));
            
               Record_Ref.Internal_Location := Wrap_Node (Value.Location, No_Entity_Info);
            
               Record_Ref.Internal_Contexts := new Logic_Context_Array'(To_Public_Logic_Context_Array (Value.Contexts));
            
               Record_Ref.Internal_Round := Value.Round;
         return Result;
      end;


   
   
   function Create_Solver_Diagnostic
     (Message_Template : Text_Type; Args : Lkt_Node_Array; Location : Lkt_Node'Class; Contexts : Logic_Context_Array; Round : Integer)
     return Solver_Diagnostic
 is
      Result     : constant Solver_Diagnostic :=
         Solver_Diagnostic (Boxed_Solver_Diagnostic.Create_Element);
      Record_Def : constant Boxed_Solver_Diagnostic.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Message_Template := new Text_Type'(Message_Template);
         
            Record_Def.Internal_Args := new Lkt_Node_Array'(Args);
         
            Record_Def.Internal_Location := Location.As_Lkt_Node;
         
            Record_Def.Internal_Contexts := new Logic_Context_Array'(Contexts);
         
            Record_Def.Internal_Round := Round;
      return Result;
   end;

         

      
   function Success
     (Self : Solver_Result)
      return Boolean
 is
         Record_Ref : constant Boxed_Solver_Result.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Success
            ;
      end;
      
   function Diagnostics
     (Self : Solver_Result)
      return Solver_Diagnostic_Array
 is
         Record_Ref : constant Boxed_Solver_Result.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Diagnostics
                  .all
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Solver_Result_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Solver_Result_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Internal_Solver_Result_Record) is
         
      begin
            Free (Self.Internal_Diagnostics);
      end Release;

      function To_Public_Solver_Result
        (Value : Internal_Solver_Result) return Solver_Result
      is
         Result : constant Solver_Result :=
            Solver_Result (Boxed_Solver_Result.Create_Element);
         Record_Ref : constant Boxed_Solver_Result.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Success := Value.Success;
            
               Record_Ref.Internal_Diagnostics := new Solver_Diagnostic_Array'(To_Public_Solver_Diagnostic_Array (Value.Diagnostics));
         return Result;
      end;


   
   
   function Create_Solver_Result
     (Success : Boolean; Diagnostics : Solver_Diagnostic_Array)
     return Solver_Result
 is
      Result     : constant Solver_Result :=
         Solver_Result (Boxed_Solver_Result.Create_Element);
      Record_Def : constant Boxed_Solver_Result.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Success := Success;
         
            Record_Def.Internal_Diagnostics := new Solver_Diagnostic_Array'(Diagnostics);
      return Result;
   end;






         
   function Parent
     (Node : Lkt_Node'Class) return Lkt_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Parent
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Lkt_Node := Wrap_Node (Property_Result.Node, Property_Result.Info) do


            null;
      end return;

   end;

         
   function Parents
     (Node : Lkt_Node'Class;
      With_Self : Boolean := True) return Lkt_Node_Array is
      


         Internal_Arg_With_Self : Boolean;
      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_With_Self :=
            With_Self;

      
      Property_Result :=
         Liblktlang.Implementation.Parents
            (Bare_Lkt_Node (Node.Internal.Node), Internal_Arg_With_Self, E_Info => Node.Internal.Info);

      return Result : Lkt_Node_Array := To_Public_Lkt_Node_Array (Property_Result) do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function Children
     (Node : Lkt_Node'Class) return Lkt_Node_Array is
      


      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Children
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Lkt_Node_Array := To_Public_Lkt_Node_Array (Property_Result) do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function Token_Start
     (Node : Lkt_Node'Class) return Token_Reference is
      


      Property_Result : Token_Reference;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Token_Start
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Token_Reference := Property_Result do


            null;
      end return;

   end;

         
   function Token_End
     (Node : Lkt_Node'Class) return Token_Reference is
      


      Property_Result : Token_Reference;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Token_End
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Token_Reference := Property_Result do


            null;
      end return;

   end;

         
   function Child_Index
     (Node : Lkt_Node'Class) return Integer is
      


      Property_Result : Integer;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Child_Index
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Integer := Property_Result do


            null;
      end return;

   end;

         
   function Previous_Sibling
     (Node : Lkt_Node'Class) return Lkt_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Previous_Sibling
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Lkt_Node := Wrap_Node (Property_Result.Node, Property_Result.Info) do


            null;
      end return;

   end;

         
   function Next_Sibling
     (Node : Lkt_Node'Class) return Lkt_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Next_Sibling
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Lkt_Node := Wrap_Node (Property_Result.Node, Property_Result.Info) do


            null;
      end return;

   end;

         
   function Unit
     (Node : Lkt_Node'Class) return Analysis_Unit is
      


      Property_Result : Internal_Unit;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Unit
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Analysis_Unit := Wrap_Unit (Property_Result) do


            null;
      end return;

   end;

         
   function Is_Ghost
     (Node : Lkt_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Is_Ghost
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;

         
   function Full_Sloc_Image
     (Node : Lkt_Node'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Full_Sloc_Image
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Text_Type := Property_Result.Content do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function Completion_Item_Kind_To_Int
     (Node : Lkt_Node'Class;
      Kind : Completion_Item_Kind) return Integer is
      


         Internal_Arg_Kind : Completion_Item_Kind;
      Property_Result : Integer;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Kind :=
            Kind;

      
      Property_Result :=
         Liblktlang.Implementation.Completion_Item_Kind_To_Int
            (Bare_Lkt_Node (Node.Internal.Node), Internal_Arg_Kind);

      return Result : Integer := Property_Result do


            null;
      end return;

   end;

         
   function P_Set_Solver_Debug_Mode
     (Node : Lkt_Node'Class;
      Enable : Boolean) return Boolean is
      


         Internal_Arg_Enable : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Enable :=
            Enable;

      
      Property_Result :=
         Liblktlang.Implementation.Extensions.Lkt_Node_P_Set_Solver_Debug_Mode
            (Bare_Lkt_Node (Node.Internal.Node), Internal_Arg_Enable);

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;

         
   function P_Basic_Trait_Gen
     (Node : Lkt_Node'Class) return Generic_Decl is
      


      Property_Result : Internal_Entity_Generic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Basic_Trait_Gen
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Generic_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Generic_Decl do


            null;
      end return;

   end;

         
   function P_Basic_Trait
     (Node : Lkt_Node'Class) return Trait_Decl is
      


      Property_Result : Internal_Entity_Trait_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Basic_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Trait_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Trait_Decl do


            null;
      end return;

   end;

         
   function P_Node_Gen_Trait
     (Node : Lkt_Node'Class) return Generic_Decl is
      


      Property_Result : Internal_Entity_Generic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Node_Gen_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Generic_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Generic_Decl do


            null;
      end return;

   end;

         
   function P_Node_Trait
     (Node : Lkt_Node'Class) return Trait_Decl is
      


      Property_Result : Internal_Entity_Trait_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Node_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Trait_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Trait_Decl do


            null;
      end return;

   end;

         
   function P_Indexable_Gen_Trait
     (Node : Lkt_Node'Class) return Generic_Decl is
      


      Property_Result : Internal_Entity_Generic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Indexable_Gen_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Generic_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Generic_Decl do


            null;
      end return;

   end;

         
   function P_Indexable_Trait
     (Node : Lkt_Node'Class) return Trait_Decl is
      


      Property_Result : Internal_Entity_Trait_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Indexable_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Trait_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Trait_Decl do


            null;
      end return;

   end;

         
   function P_Token_Node_Trait
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Token_Node_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Error_Node_Trait
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Error_Node_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Char_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Char_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Int_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Int_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Bool_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Bool_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Bigint_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Bigint_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_String_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_String_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Symbol_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Symbol_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Property_Error_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Property_Error_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Regexp_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Regexp_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Entity_Gen_Type
     (Node : Lkt_Node'Class) return Generic_Decl is
      


      Property_Result : Internal_Entity_Generic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Entity_Gen_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Generic_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Generic_Decl do


            null;
      end return;

   end;

         
   function P_Entity_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Entity_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Logicvar_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Logicvar_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Equation_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Equation_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Array_Gen_Type
     (Node : Lkt_Node'Class) return Generic_Decl is
      


      Property_Result : Internal_Entity_Generic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Array_Gen_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Generic_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Generic_Decl do


            null;
      end return;

   end;

         
   function P_Array_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Array_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Astlist_Gen_Type
     (Node : Lkt_Node'Class) return Generic_Decl is
      


      Property_Result : Internal_Entity_Generic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Astlist_Gen_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Generic_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Generic_Decl do


            null;
      end return;

   end;

         
   function P_Astlist_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Astlist_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Node_Builder_Gen_Type
     (Node : Lkt_Node'Class) return Generic_Decl is
      


      Property_Result : Internal_Entity_Generic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Node_Builder_Gen_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Generic_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Generic_Decl do


            null;
      end return;

   end;

         
   function P_Node_Builder_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl is
      


      Property_Result : Internal_Entity_Named_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Node_Builder_Type
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Named_Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Named_Type_Decl do


            null;
      end return;

   end;

         
   function P_Iterator_Gen_Trait
     (Node : Lkt_Node'Class) return Generic_Decl is
      


      Property_Result : Internal_Entity_Generic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Iterator_Gen_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Generic_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Generic_Decl do


            null;
      end return;

   end;

         
   function P_Iterator_Trait
     (Node : Lkt_Node'Class) return Trait_Decl is
      


      Property_Result : Internal_Entity_Trait_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Iterator_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Trait_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Trait_Decl do


            null;
      end return;

   end;

         
   function P_Analysis_Unit_Gen_Trait
     (Node : Lkt_Node'Class) return Generic_Decl is
      


      Property_Result : Internal_Entity_Generic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Analysis_Unit_Gen_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Generic_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Generic_Decl do


            null;
      end return;

   end;

         
   function P_Analysis_Unit_Trait
     (Node : Lkt_Node'Class) return Trait_Decl is
      


      Property_Result : Internal_Entity_Trait_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Analysis_Unit_Trait
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Trait_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Trait_Decl do


            null;
      end return;

   end;

         
   function P_Topmost_Invalid_Decl
     (Node : Lkt_Node'Class) return Lkt_Node is
      


      Property_Result : Bare_Lkt_Node;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Topmost_Invalid_Decl
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Lkt_Node := Wrap_Node (Property_Result, No_Entity_Info) do
            Result := Wrap_Node
              (Property_Result, Node.Internal.Info)
              .As_Lkt_Node;


      end return;

   end;

         
   function P_Nameres_Diagnostics
     (Node : Lkt_Node'Class) return Solver_Diagnostic_Array is
      


      Property_Result : Internal_Solver_Diagnostic_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Nameres_Diagnostics
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Solver_Diagnostic_Array := To_Public_Solver_Diagnostic_Array (Property_Result) do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Solve_Enclosing_Context
     (Node : Lkt_Node'Class) return Solver_Result is
      


      Property_Result : Internal_Solver_Result;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Lkt_Node_P_Solve_Enclosing_Context
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Solver_Result := To_Public_Solver_Result (Property_Result) do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Xref_Entry_Point
     (Node : Lkt_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_Lkt_Node_P_Xref_Entry_Point
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;






         
   function P_Get_Type
     (Node : Expr'Class) return Type_Decl is
      


      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Expr_P_Get_Type
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl do


            null;
      end return;

   end;

         
   function P_Get_Generic_Type
     (Node : Expr'Class) return Type_Decl is
      


      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Expr_P_Get_Generic_Type
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl do


            null;
      end return;

   end;

         
   function P_Get_Expected_Type
     (Node : Expr'Class) return Type_Decl is
      


      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Expr_P_Get_Expected_Type
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl do


            null;
      end return;

   end;

         
   function P_Referenced_Decl
     (Node : Expr'Class) return Decl is
      


      Property_Result : Internal_Entity_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_Expr_P_Referenced_Decl
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Decl do


            null;
      end return;

   end;





         
   

   function F_Expr
     (Node : Any_Of'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Any_Of_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Values
     (Node : Any_Of'Class) return Any_Of_List
   is
      Result : Bare_Any_Of_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Any_Of_F_Values (Node.Internal.Node);
         if Result = null then
            return No_Any_Of_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Values;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Expr_List'Class; Index : Positive) return Expr
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Expr;
         end List_Child;

         

         function Expr_List_First (Node : Expr_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Expr_List_Next
           (Node : Expr_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Expr_List_Has_Element
           (Node : Expr_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Expr_List_Element
           (Node : Expr_List; Cursor : Positive) return Expr'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Expr'(Child.As_Expr);
         end;











         
   

   function F_Syn_Name
     (Node : Decl'Class) return Def_Id
   is
      Result : Bare_Def_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decl_F_Syn_Name (Node.Internal.Node);
         if Result = null then
            return No_Def_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Syn_Name;



         
   function P_Custom_Image
     (Node : Decl'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Extensions.Decl_P_Custom_Image
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Text_Type := Property_Result.Content do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Decl_Type_Name
     (Node : Decl'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_Decl_P_Decl_Type_Name
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Text_Type := Property_Result.Content do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_As_Bare_Decl
     (Node : Decl'Class) return Decl is
      


      Property_Result : Internal_Entity_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Decl_P_As_Bare_Decl
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Decl do


            null;
      end return;

   end;

         
   function P_Get_Type
     (Node : Decl'Class) return Type_Decl is
      


      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Decl_P_Get_Type
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl do


            null;
      end return;

   end;

         
   function P_Get_Cast_Type
     (Node : Decl'Class;
      Cast_To : Type_Decl'Class) return Type_Decl is
      


         Internal_Arg_Cast_To : Internal_Entity_Type_Decl;
      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Cast_To :=
            (Cast_To.Internal.Node, Cast_To.Internal.Info);

      
      Property_Result :=
         Liblktlang.Implementation.Decl_P_Get_Cast_Type
            (Bare_Lkt_Node (Node.Internal.Node), Internal_Arg_Cast_To, E_Info => Node.Internal.Info);

      return Result : Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl do


            null;
      end return;

   end;

         
   function P_Get_Keep_Type
     (Node : Decl'Class;
      Keep_Type : Type_Decl'Class) return Type_Decl is
      


         Internal_Arg_Keep_Type : Internal_Entity_Type_Decl;
      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Keep_Type :=
            (Keep_Type.Internal.Node, Keep_Type.Internal.Info);

      
      Property_Result :=
         Liblktlang.Implementation.Decl_P_Get_Keep_Type
            (Bare_Lkt_Node (Node.Internal.Node), Internal_Arg_Keep_Type, E_Info => Node.Internal.Info);

      return Result : Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl do


            null;
      end return;

   end;

         
   function P_Get_Suffix_Type
     (Node : Decl'Class;
      Prefix_Type : Type_Decl'Class) return Type_Decl is
      


         Internal_Arg_Prefix_Type : Internal_Entity_Type_Decl;
      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Prefix_Type :=
            (Prefix_Type.Internal.Node, Prefix_Type.Internal.Info);

      
      Property_Result :=
         Liblktlang.Implementation.Decl_P_Get_Suffix_Type
            (Bare_Lkt_Node (Node.Internal.Node), Internal_Arg_Prefix_Type, E_Info => Node.Internal.Info);

      return Result : Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl do


            null;
      end return;

   end;

         
   function P_Is_Generic
     (Node : Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Decl_P_Is_Generic
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;

         
   function P_Return_Type_Is_Instantiated
     (Node : Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Decl_P_Return_Type_Is_Instantiated
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;

         
   function P_Is_Instantiated
     (Node : Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Decl_P_Is_Instantiated
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;

         
   function P_Name
     (Node : Decl'Class) return Unbounded_Text_Type is
      


      Property_Result : Symbol_Type;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_Decl_P_Name
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Unbounded_Text_Type := To_Unbounded_Text (Image (Property_Result)) do


            null;
      end return;

   end;

         
   function P_Full_Name
     (Node : Decl'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_Decl_P_Full_Name
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Text_Type := Property_Result.Content do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Traits
     (Node : Type_Decl'Class) return Type_Ref_List
   is
      Result : Bare_Type_Ref_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Type_Decl_F_Traits (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Traits;


         
   

   function F_Syn_Base_Type
     (Node : Type_Decl'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Type_Decl_F_Syn_Base_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Syn_Base_Type;



         
   function P_Base_Type
     (Node : Type_Decl'Class) return Type_Ref is
      


      Property_Result : Internal_Entity_Type_Ref;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Type_Decl_P_Base_Type
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Type_Ref := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Ref do


            null;
      end return;

   end;

         
   function P_Base_Type_If_Entity
     (Node : Type_Decl'Class) return Type_Decl is
      


      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Type_Decl_P_Base_Type_If_Entity
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl do


            null;
      end return;

   end;










         
   

   function F_Name
     (Node : Argument'Class) return Ref_Id
   is
      Result : Bare_Ref_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Argument_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Ref_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Value
     (Node : Argument'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Argument_F_Value (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Value;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Argument_List'Class; Index : Positive) return Argument
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Argument;
         end List_Child;

         

         function Argument_List_First (Node : Argument_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Argument_List_Next
           (Node : Argument_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Argument_List_Has_Element
           (Node : Argument_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Argument_List_Element
           (Node : Argument_List; Cursor : Positive) return Argument'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Argument'(Child.As_Argument);
         end;






         
   

   function F_Exprs
     (Node : Array_Literal'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Array_Literal_F_Exprs (Node.Internal.Node);
         if Result = null then
            return No_Expr_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Exprs;


         
   

   function F_Element_Type
     (Node : Array_Literal'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Array_Literal_F_Element_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Element_Type;







         
   

   function F_Name
     (Node : Base_Call_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Call_Expr_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Args
     (Node : Base_Call_Expr'Class) return Argument_List
   is
      Result : Bare_Argument_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Call_Expr_F_Args (Node.Internal.Node);
         if Result = null then
            return No_Argument_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Args;







         
   

   function F_Expr
     (Node : Base_Grammar_Rule_Decl'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Grammar_Rule_Decl_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Send
     (Node : Base_Lexer_Case_Rule_Alt'Class) return Lexer_Case_Rule_Send
   is
      Result : Bare_Lexer_Case_Rule_Send;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Lexer_Case_Rule_Alt_F_Send (Node.Internal.Node);
         if Result = null then
            return No_Lexer_Case_Rule_Send;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Send;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Base_Lexer_Case_Rule_Alt_List'Class; Index : Positive) return Base_Lexer_Case_Rule_Alt
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Base_Lexer_Case_Rule_Alt;
         end List_Child;

         

         function Base_Lexer_Case_Rule_Alt_List_First (Node : Base_Lexer_Case_Rule_Alt_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Base_Lexer_Case_Rule_Alt_List_Next
           (Node : Base_Lexer_Case_Rule_Alt_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Base_Lexer_Case_Rule_Alt_List_Has_Element
           (Node : Base_Lexer_Case_Rule_Alt_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Base_Lexer_Case_Rule_Alt_List_Element
           (Node : Base_Lexer_Case_Rule_Alt_List; Cursor : Positive) return Base_Lexer_Case_Rule_Alt'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Base_Lexer_Case_Rule_Alt'(Child.As_Base_Lexer_Case_Rule_Alt);
         end;






         
   

   function F_Expr
     (Node : Base_Match_Branch'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Match_Branch_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;



         
   function P_Match_Part
     (Node : Base_Match_Branch'Class) return Lkt_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_Base_Match_Branch_P_Match_Part
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Lkt_Node := Wrap_Node (Property_Result.Node, Property_Result.Info) do


            null;
      end return;

   end;



         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Base_Match_Branch_List'Class; Index : Positive) return Base_Match_Branch
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Base_Match_Branch;
         end List_Child;

         

         function Base_Match_Branch_List_First (Node : Base_Match_Branch_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Base_Match_Branch_List_Next
           (Node : Base_Match_Branch_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Base_Match_Branch_List_Has_Element
           (Node : Base_Match_Branch_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Base_Match_Branch_List_Element
           (Node : Base_Match_Branch_List; Cursor : Positive) return Base_Match_Branch'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Base_Match_Branch'(Child.As_Base_Match_Branch);
         end;









         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Base_Pattern_List'Class; Index : Positive) return Base_Pattern
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Base_Pattern;
         end List_Child;

         

         function Base_Pattern_List_First (Node : Base_Pattern_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Base_Pattern_List_Next
           (Node : Base_Pattern_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Base_Pattern_List_Has_Element
           (Node : Base_Pattern_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Base_Pattern_List_Element
           (Node : Base_Pattern_List; Cursor : Positive) return Base_Pattern'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Base_Pattern'(Child.As_Base_Pattern);
         end;











         
   

   function F_Decls
     (Node : Named_Type_Decl'Class) return Decl_Block
   is
      Result : Bare_Decl_Block;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Named_Type_Decl_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Decl_Block;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;






















         
   

   function F_Left
     (Node : Bin_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Bin_Op_F_Left (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Left;


         
   

   function F_Op
     (Node : Bin_Op'Class) return Op
   is
      Result : Bare_Op;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Bin_Op_F_Op (Node.Internal.Node);
         if Result = null then
            return No_Op;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Op;

         function F_Op
           (Node : Bin_Op'Class) return Lkt_Op
         is (Op'(Node.F_Op).Kind);

         
   

   function F_Right
     (Node : Bin_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Bin_Op_F_Right (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Right;







         
   

   function F_Decl
     (Node : Binding_Pattern'Class) return Binding_Val_Decl
   is
      Result : Bare_Binding_Val_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Binding_Pattern_F_Decl (Node.Internal.Node);
         if Result = null then
            return No_Binding_Val_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl;


         
   

   function F_Value_Pattern
     (Node : Binding_Pattern'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Binding_Pattern_F_Value_Pattern (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Value_Pattern;
















         

         function Lkt_Node_List_First (Node : Lkt_Node_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Lkt_Node_List_Next
           (Node : Lkt_Node_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Lkt_Node_List_Has_Element
           (Node : Lkt_Node_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Lkt_Node_List_Element
           (Node : Lkt_Node_List; Cursor : Positive) return Lkt_Node'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Lkt_Node'(Child.As_Lkt_Node);
         end;











         
   

   function F_Val_Defs
     (Node : Block_Expr'Class) return Block_Decl_List
   is
      Result : Bare_Block_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Block_Expr_F_Val_Defs (Node.Internal.Node);
         if Result = null then
            return No_Block_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Val_Defs;


         
   

   function F_Expr
     (Node : Block_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Block_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Block_String_Line_List'Class; Index : Positive) return Block_String_Line
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Block_String_Line;
         end List_Child;

         

         function Block_String_Line_List_First (Node : Block_String_Line_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Block_String_Line_List_Next
           (Node : Block_String_Line_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Block_String_Line_List_Has_Element
           (Node : Block_String_Line_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Block_String_Line_List_Element
           (Node : Block_String_Line_List; Cursor : Positive) return Block_String_Line'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Block_String_Line'(Child.As_Block_String_Line);
         end;







         
   function P_Denoted_Value
     (Node : String_Lit'Class) return Decoded_String_Value is
      


      Property_Result : Internal_Decoded_String_Value;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_String_Lit_P_Denoted_Value
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Decoded_String_Value := To_Public_Decoded_String_Value (Property_Result) do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Is_Prefixed_String
     (Node : String_Lit'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_String_Lit_P_Is_Prefixed_String
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;

         
   function P_Prefix
     (Node : String_Lit'Class) return Character_Type is
      


      Property_Result : Character_Type;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_String_Lit_P_Prefix
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Character_Type := Property_Result do


            null;
      end return;

   end;

         
   function P_Is_Regexp_Literal
     (Node : String_Lit'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.String_Lit_P_Is_Regexp_Literal
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;





         
   

   function F_Lines
     (Node : Block_String_Lit'Class) return Block_String_Line_List
   is
      Result : Bare_Block_String_Line_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Block_String_Lit_F_Lines (Node.Internal.Node);
         if Result = null then
            return No_Block_String_Line_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Lines;






























         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Call_Expr_List'Class; Index : Positive) return Call_Expr
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Call_Expr;
         end List_Child;

         

         function Call_Expr_List_First (Node : Call_Expr_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Call_Expr_List_Next
           (Node : Call_Expr_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Call_Expr_List_Has_Element
           (Node : Call_Expr_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Call_Expr_List_Element
           (Node : Call_Expr_List; Cursor : Positive) return Call_Expr'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Call_Expr'(Child.As_Call_Expr);
         end;






         
   

   function F_Expr
     (Node : Cast_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Cast_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Null_Cond
     (Node : Cast_Expr'Class) return Null_Cond_Qualifier
   is
      Result : Bare_Null_Cond_Qualifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Cast_Expr_F_Null_Cond (Node.Internal.Node);
         if Result = null then
            return No_Null_Cond_Qualifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Null_Cond;

         function F_Null_Cond (Node : Cast_Expr'Class) return Boolean
         is (Null_Cond_Qualifier'(Node.F_Null_Cond).Kind
             = Lkt_Null_Cond_Qualifier_Present);


         
   

   function F_Excludes_Null
     (Node : Cast_Expr'Class) return Excludes_Null
   is
      Result : Bare_Excludes_Null;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Cast_Expr_F_Excludes_Null (Node.Internal.Node);
         if Result = null then
            return No_Excludes_Null;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Excludes_Null;

         function F_Excludes_Null (Node : Cast_Expr'Class) return Boolean
         is (Excludes_Null'(Node.F_Excludes_Null).Kind
             = Lkt_Excludes_Null_Present);


         
   

   function F_Dest_Type
     (Node : Cast_Expr'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Cast_Expr_F_Dest_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Dest_Type;








         
   function P_Denoted_Value
     (Node : Char_Lit'Class) return Decoded_Char_Value is
      


      Property_Result : Internal_Decoded_Char_Value;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Extensions.Char_Lit_P_Denoted_Value
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Decoded_Char_Value := To_Public_Decoded_Char_Value (Property_Result) do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;











         
   function P_As_Bool
     (Node : Class_Qualifier'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_Class_Qualifier_P_As_Bool
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;















         
   

   function F_Decl_Type
     (Node : Explicitly_Typed_Decl'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Explicitly_Typed_Decl_F_Decl_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl_Type;







         
   

   function F_Default_Val
     (Node : Component_Decl'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Decl_F_Default_Val (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Default_Val;







         
   

   function F_Name
     (Node : Decl_Annotation'Class) return Id
   is
      Result : Bare_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decl_Annotation_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Args
     (Node : Decl_Annotation'Class) return Decl_Annotation_Args
   is
      Result : Bare_Decl_Annotation_Args;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decl_Annotation_F_Args (Node.Internal.Node);
         if Result = null then
            return No_Decl_Annotation_Args;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Args;







         
   

   function F_Args
     (Node : Decl_Annotation_Args'Class) return Argument_List
   is
      Result : Bare_Argument_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decl_Annotation_Args_F_Args (Node.Internal.Node);
         if Result = null then
            return No_Argument_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Args;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Decl_Annotation_List'Class; Index : Positive) return Decl_Annotation
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Decl_Annotation;
         end List_Child;

         

         function Decl_Annotation_List_First (Node : Decl_Annotation_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Decl_Annotation_List_Next
           (Node : Decl_Annotation_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Decl_Annotation_List_Has_Element
           (Node : Decl_Annotation_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Decl_Annotation_List_Element
           (Node : Decl_Annotation_List; Cursor : Positive) return Decl_Annotation'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Decl_Annotation'(Child.As_Decl_Annotation);
         end;




         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Full_Decl_List'Class; Index : Positive) return Full_Decl
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Full_Decl;
         end List_Child;

         

         function Full_Decl_List_First (Node : Full_Decl_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Full_Decl_List_Next
           (Node : Full_Decl_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Full_Decl_List_Has_Element
           (Node : Full_Decl_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Full_Decl_List_Element
           (Node : Full_Decl_List; Cursor : Positive) return Full_Decl'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Full_Decl'(Child.As_Full_Decl);
         end;












         
   function P_Custom_Image
     (Node : Id'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Extensions.Id_P_Custom_Image
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Text_Type := Property_Result.Content do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;











         
   function P_Referenced_Decl
     (Node : Type_Ref'Class) return Type_Decl is
      


      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Type_Ref_P_Referenced_Decl
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Type_Decl := Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl do


            null;
      end return;

   end;










         
   

   function F_Prefix
     (Node : Dot_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dot_Expr_F_Prefix (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prefix;


         
   

   function F_Null_Cond
     (Node : Dot_Expr'Class) return Null_Cond_Qualifier
   is
      Result : Bare_Null_Cond_Qualifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dot_Expr_F_Null_Cond (Node.Internal.Node);
         if Result = null then
            return No_Null_Cond_Qualifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Null_Cond;

         function F_Null_Cond (Node : Dot_Expr'Class) return Boolean
         is (Null_Cond_Qualifier'(Node.F_Null_Cond).Kind
             = Lkt_Null_Cond_Qualifier_Present);


         
   

   function F_Suffix
     (Node : Dot_Expr'Class) return Ref_Id
   is
      Result : Bare_Ref_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dot_Expr_F_Suffix (Node.Internal.Node);
         if Result = null then
            return No_Ref_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Suffix;

















         
   

   function F_Cond_Expr
     (Node : Elsif_Branch'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Elsif_Branch_F_Cond_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cond_Expr;


         
   

   function F_Then_Expr
     (Node : Elsif_Branch'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Elsif_Branch_F_Then_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Then_Expr;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Elsif_Branch_List'Class; Index : Positive) return Elsif_Branch
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Elsif_Branch;
         end List_Child;

         

         function Elsif_Branch_List_First (Node : Elsif_Branch_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Elsif_Branch_List_Next
           (Node : Elsif_Branch_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Elsif_Branch_List_Has_Element
           (Node : Elsif_Branch_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Elsif_Branch_List_Element
           (Node : Elsif_Branch_List; Cursor : Positive) return Elsif_Branch'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Elsif_Branch'(Child.As_Elsif_Branch);
         end;









         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Enum_Class_Alt_Decl_List'Class; Index : Positive) return Enum_Class_Alt_Decl
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Enum_Class_Alt_Decl;
         end List_Child;

         

         function Enum_Class_Alt_Decl_List_First (Node : Enum_Class_Alt_Decl_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Enum_Class_Alt_Decl_List_Next
           (Node : Enum_Class_Alt_Decl_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Enum_Class_Alt_Decl_List_Has_Element
           (Node : Enum_Class_Alt_Decl_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Enum_Class_Alt_Decl_List_Element
           (Node : Enum_Class_Alt_Decl_List; Cursor : Positive) return Enum_Class_Alt_Decl'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Enum_Class_Alt_Decl'(Child.As_Enum_Class_Alt_Decl);
         end;






         
   

   function F_Decls
     (Node : Enum_Class_Case'Class) return Enum_Class_Alt_Decl_List
   is
      Result : Bare_Enum_Class_Alt_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Enum_Class_Case_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Enum_Class_Alt_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Enum_Class_Case_List'Class; Index : Positive) return Enum_Class_Case
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Enum_Class_Case;
         end List_Child;

         

         function Enum_Class_Case_List_First (Node : Enum_Class_Case_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Enum_Class_Case_List_Next
           (Node : Enum_Class_Case_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Enum_Class_Case_List_Has_Element
           (Node : Enum_Class_Case_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Enum_Class_Case_List_Element
           (Node : Enum_Class_Case_List; Cursor : Positive) return Enum_Class_Case'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Enum_Class_Case'(Child.As_Enum_Class_Case);
         end;






         
   

   function F_Branches
     (Node : Enum_Class_Decl'Class) return Enum_Class_Case_List
   is
      Result : Bare_Enum_Class_Case_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Enum_Class_Decl_F_Branches (Node.Internal.Node);
         if Result = null then
            return No_Enum_Class_Case_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Branches;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Enum_Lit_Decl_List'Class; Index : Positive) return Enum_Lit_Decl
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Enum_Lit_Decl;
         end List_Child;

         

         function Enum_Lit_Decl_List_First (Node : Enum_Lit_Decl_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Enum_Lit_Decl_List_Next
           (Node : Enum_Lit_Decl_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Enum_Lit_Decl_List_Has_Element
           (Node : Enum_Lit_Decl_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Enum_Lit_Decl_List_Element
           (Node : Enum_Lit_Decl_List; Cursor : Positive) return Enum_Lit_Decl'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Enum_Lit_Decl'(Child.As_Enum_Lit_Decl);
         end;






         
   

   function F_Literals
     (Node : Enum_Type_Decl'Class) return Enum_Lit_Decl_List
   is
      Result : Bare_Enum_Lit_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Enum_Type_Decl_F_Literals (Node.Internal.Node);
         if Result = null then
            return No_Enum_Lit_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Literals;







         
   

   function F_Actions
     (Node : Env_Spec_Decl'Class) return Call_Expr_List
   is
      Result : Bare_Call_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Env_Spec_Decl_F_Actions (Node.Internal.Node);
         if Result = null then
            return No_Call_Expr_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Actions;







         
   

   function F_Expr
     (Node : Error_On_Null'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Error_On_Null_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;








         
   function P_As_Bool
     (Node : Excludes_Null'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_Excludes_Null_P_As_Bool
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;




















         
   

   function F_Node_Pattern
     (Node : Extended_Node_Pattern'Class) return Value_Pattern
   is
      Result : Bare_Value_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Extended_Node_Pattern_F_Node_Pattern (Node.Internal.Node);
         if Result = null then
            return No_Value_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Node_Pattern;


         
   

   function F_Details
     (Node : Extended_Node_Pattern'Class) return Node_Pattern_Detail_List
   is
      Result : Bare_Node_Pattern_Detail_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Extended_Node_Pattern_F_Details (Node.Internal.Node);
         if Result = null then
            return No_Node_Pattern_Detail_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Details;







         
   

   function F_Trait_Ref
     (Node : Field_Decl'Class) return Dot_Expr
   is
      Result : Bare_Dot_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Field_Decl_F_Trait_Ref (Node.Internal.Node);
         if Result = null then
            return No_Dot_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Trait_Ref;







         
   

   function F_Pattern
     (Node : Filtered_Pattern'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Filtered_Pattern_F_Pattern (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pattern;


         
   

   function F_Predicate
     (Node : Filtered_Pattern'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Filtered_Pattern_F_Predicate (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Predicate;







         
   

   function F_Doc
     (Node : Full_Decl'Class) return String_Lit
   is
      Result : Bare_String_Lit;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Full_Decl_F_Doc (Node.Internal.Node);
         if Result = null then
            return No_String_Lit;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Doc;


         
   

   function F_Decl_Annotations
     (Node : Full_Decl'Class) return Decl_Annotation_List
   is
      Result : Bare_Decl_Annotation_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Full_Decl_F_Decl_Annotations (Node.Internal.Node);
         if Result = null then
            return No_Decl_Annotation_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl_Annotations;


         
   

   function F_Decl
     (Node : Full_Decl'Class) return Decl
   is
      Result : Bare_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Full_Decl_F_Decl (Node.Internal.Node);
         if Result = null then
            return No_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl;



         
   function P_Has_Annotation
     (Node : Full_Decl'Class;
      Name : Unbounded_Text_Type) return Boolean is
      


         Internal_Arg_Name : Symbol_Type;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

      
      Property_Result :=
         Liblktlang.Implementation.Full_Decl_P_Has_Annotation
            (Bare_Lkt_Node (Node.Internal.Node), Internal_Arg_Name);

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;





         
   

   function F_Params
     (Node : Fun_Decl'Class) return Fun_Param_Decl_List
   is
      Result : Bare_Fun_Param_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Fun_Decl_F_Params (Node.Internal.Node);
         if Result = null then
            return No_Fun_Param_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Params;


         
   

   function F_Return_Type
     (Node : Fun_Decl'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Fun_Decl_F_Return_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Return_Type;


         
   

   function F_Trait_Ref
     (Node : Fun_Decl'Class) return Dot_Expr
   is
      Result : Bare_Dot_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Fun_Decl_F_Trait_Ref (Node.Internal.Node);
         if Result = null then
            return No_Dot_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Trait_Ref;


         
   

   function F_Body
     (Node : Fun_Decl'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Fun_Decl_F_Body (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Body;



         
   function P_Is_Dynamic_Combiner
     (Node : Fun_Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Fun_Decl_P_Is_Dynamic_Combiner
            (Bare_Lkt_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;





         
   

   function F_Decl_Annotations
     (Node : Fun_Param_Decl'Class) return Decl_Annotation_List
   is
      Result : Bare_Decl_Annotation_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Fun_Param_Decl_F_Decl_Annotations (Node.Internal.Node);
         if Result = null then
            return No_Decl_Annotation_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl_Annotations;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Fun_Param_Decl_List'Class; Index : Positive) return Fun_Param_Decl
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Fun_Param_Decl;
         end List_Child;

         

         function Fun_Param_Decl_List_First (Node : Fun_Param_Decl_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Fun_Param_Decl_List_Next
           (Node : Fun_Param_Decl_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Fun_Param_Decl_List_Has_Element
           (Node : Fun_Param_Decl_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Fun_Param_Decl_List_Element
           (Node : Fun_Param_Decl_List; Cursor : Positive) return Fun_Param_Decl'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Fun_Param_Decl'(Child.As_Fun_Param_Decl);
         end;











         
   

   function F_Param_Types
     (Node : Function_Type_Ref'Class) return Type_Ref_List
   is
      Result : Bare_Type_Ref_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Function_Type_Ref_F_Param_Types (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Param_Types;


         
   

   function F_Return_Type
     (Node : Function_Type_Ref'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Function_Type_Ref_F_Return_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Return_Type;







         
   

   function F_Generic_Param_Decls
     (Node : Generic_Decl'Class) return Generic_Param_Decl_List
   is
      Result : Bare_Generic_Param_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Decl_F_Generic_Param_Decls (Node.Internal.Node);
         if Result = null then
            return No_Generic_Param_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Generic_Param_Decls;


         
   

   function F_Decl
     (Node : Generic_Decl'Class) return Decl
   is
      Result : Bare_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Decl_F_Decl (Node.Internal.Node);
         if Result = null then
            return No_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl;







         
   

   function F_Name
     (Node : Generic_Instantiation'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Instantiation_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Args
     (Node : Generic_Instantiation'Class) return Type_Ref_List
   is
      Result : Bare_Type_Ref_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Instantiation_F_Args (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Args;












         
   

   function F_Has_Class
     (Node : Generic_Param_Type_Decl'Class) return Class_Qualifier
   is
      Result : Bare_Class_Qualifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Param_Type_Decl_F_Has_Class (Node.Internal.Node);
         if Result = null then
            return No_Class_Qualifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Class;

         function F_Has_Class (Node : Generic_Param_Type_Decl'Class) return Boolean
         is (Class_Qualifier'(Node.F_Has_Class).Kind
             = Lkt_Class_Qualifier_Present);







         
   

   function F_Type_Name
     (Node : Generic_Type_Ref'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Type_Ref_F_Type_Name (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Name;


         
   

   function F_Args
     (Node : Generic_Type_Ref'Class) return Type_Ref_List
   is
      Result : Bare_Type_Ref_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Type_Ref_F_Args (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Args;

















         
   

   function F_Rules
     (Node : Grammar_Decl'Class) return Full_Decl_List
   is
      Result : Bare_Full_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Decl_F_Rules (Node.Internal.Node);
         if Result = null then
            return No_Full_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Rules;







         
   

   function F_Expr
     (Node : Grammar_Discard'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Discard_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Expr
     (Node : Grammar_Dont_Skip'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Dont_Skip_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Dont_Skip
     (Node : Grammar_Dont_Skip'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Dont_Skip_F_Dont_Skip (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Dont_Skip;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Grammar_Expr_List'Class; Index : Positive) return Grammar_Expr
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Grammar_Expr;
         end List_Child;

         

         function Grammar_Expr_List_First (Node : Grammar_Expr_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Grammar_Expr_List_Next
           (Node : Grammar_Expr_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Grammar_Expr_List_Has_Element
           (Node : Grammar_Expr_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Grammar_Expr_List_Element
           (Node : Grammar_Expr_List; Cursor : Positive) return Grammar_Expr'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Grammar_Expr'(Child.As_Grammar_Expr);
         end;




         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Grammar_Expr_List_List'Class; Index : Positive) return Grammar_Expr_List
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Grammar_Expr_List;
         end List_Child;

         

         function Grammar_Expr_List_List_First (Node : Grammar_Expr_List_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Grammar_Expr_List_List_Next
           (Node : Grammar_Expr_List_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Grammar_Expr_List_List_Has_Element
           (Node : Grammar_Expr_List_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Grammar_Expr_List_List_Element
           (Node : Grammar_Expr_List_List; Cursor : Positive) return Grammar_Expr_List'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Grammar_Expr_List'(Child.As_Grammar_Expr_List);
         end;






         
   

   function F_Exprs
     (Node : Grammar_Pick'Class) return Grammar_Expr_List
   is
      Result : Bare_Grammar_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Pick_F_Exprs (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Exprs;












         
   

   function F_List_Type
     (Node : Grammar_List'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_List_F_List_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_List_Type;


         
   

   function F_Kind
     (Node : Grammar_List'Class) return List_Kind
   is
      Result : Bare_List_Kind;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_List_F_Kind (Node.Internal.Node);
         if Result = null then
            return No_List_Kind;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Kind;

         function F_Kind
           (Node : Grammar_List'Class) return Lkt_List_Kind
         is (List_Kind'(Node.F_Kind).Kind);

         
   

   function F_Expr
     (Node : Grammar_List'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_List_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Sep
     (Node : Grammar_List'Class) return Grammar_List_Sep
   is
      Result : Bare_Grammar_List_Sep;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_List_F_Sep (Node.Internal.Node);
         if Result = null then
            return No_Grammar_List_Sep;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Sep;







         
   

   function F_Token
     (Node : Grammar_List_Sep'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_List_Sep_F_Token (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Token;


         
   

   function F_Extra
     (Node : Grammar_List_Sep'Class) return Id
   is
      Result : Bare_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_List_Sep_F_Extra (Node.Internal.Node);
         if Result = null then
            return No_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Extra;







         
   

   function F_Name
     (Node : Grammar_Null'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Null_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;







         
   

   function F_Expr
     (Node : Grammar_Opt'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Opt_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Expr
     (Node : Grammar_Opt_Error'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Opt_Error_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Expr
     (Node : Grammar_Opt_Error_Group'Class) return Grammar_Expr_List
   is
      Result : Bare_Grammar_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Opt_Error_Group_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Expr
     (Node : Grammar_Opt_Group'Class) return Grammar_Expr_List
   is
      Result : Bare_Grammar_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Opt_Group_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Sub_Exprs
     (Node : Grammar_Or_Expr'Class) return Grammar_Expr_List_List
   is
      Result : Bare_Grammar_Expr_List_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Or_Expr_F_Sub_Exprs (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr_List_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Sub_Exprs;







         
   

   function F_Expr
     (Node : Grammar_Predicate'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Predicate_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Prop_Ref
     (Node : Grammar_Predicate'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Predicate_F_Prop_Ref (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prop_Ref;












         
   

   function F_Node_Name
     (Node : Grammar_Rule_Ref'Class) return Ref_Id
   is
      Result : Bare_Ref_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Rule_Ref_F_Node_Name (Node.Internal.Node);
         if Result = null then
            return No_Ref_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Node_Name;







         
   

   function F_Name
     (Node : Grammar_Skip'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Skip_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;







         
   

   function F_Expr
     (Node : Grammar_Stop_Cut'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Grammar_Stop_Cut_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Cond_Expr
     (Node : If_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Cond_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cond_Expr;


         
   

   function F_Then_Expr
     (Node : If_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Then_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Then_Expr;


         
   

   function F_Alternatives
     (Node : If_Expr'Class) return Elsif_Branch_List
   is
      Result : Bare_Elsif_Branch_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Alternatives (Node.Internal.Node);
         if Result = null then
            return No_Elsif_Branch_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Alternatives;


         
   

   function F_Else_Expr
     (Node : If_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Else_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Else_Expr;







         
   

   function F_Name
     (Node : Import'Class) return Module_Ref_Id
   is
      Result : Bare_Module_Ref_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Import_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Module_Ref_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;



         
   function P_Referenced_Unit
     (Node : Import'Class) return Analysis_Unit is
      


      Property_Result : Internal_Unit;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Import_P_Referenced_Unit
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Analysis_Unit := Wrap_Unit (Property_Result) do


            null;
      end return;

   end;



         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Import_List'Class; Index : Positive) return Import
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Import;
         end List_Child;

         

         function Import_List_First (Node : Import_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Import_List_Next
           (Node : Import_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Import_List_Has_Element
           (Node : Import_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Import_List_Element
           (Node : Import_List; Cursor : Positive) return Import'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Import'(Child.As_Import);
         end;











         
   

   function F_Expr
     (Node : Isa'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Isa_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Pattern
     (Node : Isa'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Isa_F_Pattern (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pattern;







         
   

   function F_Expr
     (Node : Keep_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Keep_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Null_Cond
     (Node : Keep_Expr'Class) return Null_Cond_Qualifier
   is
      Result : Bare_Null_Cond_Qualifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Keep_Expr_F_Null_Cond (Node.Internal.Node);
         if Result = null then
            return No_Null_Cond_Qualifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Null_Cond;

         function F_Null_Cond (Node : Keep_Expr'Class) return Boolean
         is (Null_Cond_Qualifier'(Node.F_Null_Cond).Kind
             = Lkt_Null_Cond_Qualifier_Present);


         
   

   function F_Keep_Type
     (Node : Keep_Expr'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Keep_Expr_F_Keep_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Keep_Type;







         
   

   function F_Params
     (Node : Lambda_Expr'Class) return Lambda_Param_Decl_List
   is
      Result : Bare_Lambda_Param_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lambda_Expr_F_Params (Node.Internal.Node);
         if Result = null then
            return No_Lambda_Param_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Params;


         
   

   function F_Return_Type
     (Node : Lambda_Expr'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lambda_Expr_F_Return_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Return_Type;


         
   

   function F_Body
     (Node : Lambda_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lambda_Expr_F_Body (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Body;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Lambda_Param_Decl_List'Class; Index : Positive) return Lambda_Param_Decl
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Lambda_Param_Decl;
         end List_Child;

         

         function Lambda_Param_Decl_List_First (Node : Lambda_Param_Decl_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Lambda_Param_Decl_List_Next
           (Node : Lambda_Param_Decl_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Lambda_Param_Decl_List_Has_Element
           (Node : Lambda_Param_Decl_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Lambda_Param_Decl_List_Element
           (Node : Lambda_Param_Decl_List; Cursor : Positive) return Lambda_Param_Decl'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Lambda_Param_Decl'(Child.As_Lambda_Param_Decl);
         end;






         
   

   function F_Imports
     (Node : Langkit_Root'Class) return Import_List
   is
      Result : Bare_Import_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Langkit_Root_F_Imports (Node.Internal.Node);
         if Result = null then
            return No_Import_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Imports;


         
   

   function F_Decls
     (Node : Langkit_Root'Class) return Full_Decl_List
   is
      Result : Bare_Full_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Langkit_Root_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Full_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;



         
   function P_Fetch_Prelude
     (Node : Langkit_Root'Class) return Analysis_Unit is
      


      Property_Result : Internal_Unit;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Extensions.Langkit_Root_P_Fetch_Prelude
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Analysis_Unit := Wrap_Unit (Property_Result) do


            null;
      end return;

   end;





         
   

   function F_Expr
     (Node : Lexer_Case_Rule'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lexer_Case_Rule_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Alts
     (Node : Lexer_Case_Rule'Class) return Base_Lexer_Case_Rule_Alt_List
   is
      Result : Bare_Base_Lexer_Case_Rule_Alt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lexer_Case_Rule_F_Alts (Node.Internal.Node);
         if Result = null then
            return No_Base_Lexer_Case_Rule_Alt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Alts;







         
   

   function F_Cond_Exprs
     (Node : Lexer_Case_Rule_Cond_Alt'Class) return Ref_Id_List
   is
      Result : Bare_Ref_Id_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs (Node.Internal.Node);
         if Result = null then
            return No_Ref_Id_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cond_Exprs;












         
   

   function F_Sent
     (Node : Lexer_Case_Rule_Send'Class) return Ref_Id
   is
      Result : Bare_Ref_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lexer_Case_Rule_Send_F_Sent (Node.Internal.Node);
         if Result = null then
            return No_Ref_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Sent;


         
   

   function F_Match_Size
     (Node : Lexer_Case_Rule_Send'Class) return Num_Lit
   is
      Result : Bare_Num_Lit;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lexer_Case_Rule_Send_F_Match_Size (Node.Internal.Node);
         if Result = null then
            return No_Num_Lit;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Match_Size;







         
   

   function F_Rules
     (Node : Lexer_Decl'Class) return Lkt_Node_List
   is
      Result : Bare_Lkt_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lexer_Decl_F_Rules (Node.Internal.Node);
         if Result = null then
            return No_Lkt_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Rules;







         
   

   function F_Rules
     (Node : Lexer_Family_Decl'Class) return Full_Decl_List
   is
      Result : Bare_Full_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lexer_Family_Decl_F_Rules (Node.Internal.Node);
         if Result = null then
            return No_Full_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Rules;






















         
   

   function F_Patterns
     (Node : List_Pattern'Class) return Base_Pattern_List
   is
      Result : Bare_Base_Pattern_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.List_Pattern_F_Patterns (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Patterns;







         
   

   function F_Dest_Var
     (Node : Logic_Assign'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Logic_Assign_F_Dest_Var (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Dest_Var;


         
   

   function F_Value
     (Node : Logic_Assign'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Logic_Assign_F_Value (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Value;












         
   

   function F_Expr
     (Node : Logic_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Logic_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;












         
   

   function F_Dest_Var
     (Node : Logic_Propagate'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Logic_Propagate_F_Dest_Var (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Dest_Var;


         
   

   function F_Call
     (Node : Logic_Propagate'Class) return Logic_Propagate_Call
   is
      Result : Bare_Logic_Propagate_Call;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Logic_Propagate_F_Call (Node.Internal.Node);
         if Result = null then
            return No_Logic_Propagate_Call;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Call;












         
   

   function F_Lhs
     (Node : Logic_Unify'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Logic_Unify_F_Lhs (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Lhs;


         
   

   function F_Rhs
     (Node : Logic_Unify'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Logic_Unify_F_Rhs (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Rhs;







         
   

   function F_Decl
     (Node : Match_Branch'Class) return Match_Val_Decl
   is
      Result : Bare_Match_Val_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Match_Branch_F_Decl (Node.Internal.Node);
         if Result = null then
            return No_Match_Val_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl;







         
   

   function F_Match_Expr
     (Node : Match_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Match_Expr_F_Match_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Match_Expr;


         
   

   function F_Branches
     (Node : Match_Expr'Class) return Base_Match_Branch_List
   is
      Result : Bare_Base_Match_Branch_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Match_Expr_F_Branches (Node.Internal.Node);
         if Result = null then
            return No_Base_Match_Branch_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Branches;

























         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Node_Pattern_Detail_List'Class; Index : Positive) return Node_Pattern_Detail
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Node_Pattern_Detail;
         end List_Child;

         

         function Node_Pattern_Detail_List_First (Node : Node_Pattern_Detail_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Node_Pattern_Detail_List_Next
           (Node : Node_Pattern_Detail_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Node_Pattern_Detail_List_Has_Element
           (Node : Node_Pattern_Detail_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Node_Pattern_Detail_List_Element
           (Node : Node_Pattern_Detail_List; Cursor : Positive) return Node_Pattern_Detail'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Node_Pattern_Detail'(Child.As_Node_Pattern_Detail);
         end;






         
   

   function F_Id
     (Node : Node_Pattern_Field'Class) return Id
   is
      Result : Bare_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Node_Pattern_Field_F_Id (Node.Internal.Node);
         if Result = null then
            return No_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Id;


         
   

   function F_Expected_Value
     (Node : Node_Pattern_Field'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Node_Pattern_Field_F_Expected_Value (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expected_Value;







         
   

   function F_Call
     (Node : Node_Pattern_Property'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Node_Pattern_Property_F_Call (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Call;


         
   

   function F_Expected_Value
     (Node : Node_Pattern_Property'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Node_Pattern_Property_F_Expected_Value (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expected_Value;







         
   

   function F_Call
     (Node : Node_Pattern_Selector'Class) return Selector_Call
   is
      Result : Bare_Selector_Call;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Node_Pattern_Selector_F_Call (Node.Internal.Node);
         if Result = null then
            return No_Selector_Call;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Call;


         
   

   function F_Pattern
     (Node : Node_Pattern_Selector'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Node_Pattern_Selector_F_Pattern (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pattern;







         
   

   function F_Expr
     (Node : Not_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Not_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Pattern
     (Node : Not_Pattern'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Not_Pattern_F_Pattern (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pattern;








         
   function P_As_Bool
     (Node : Null_Cond_Qualifier'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Dispatcher_Null_Cond_Qualifier_P_As_Bool
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Boolean := Property_Result do


            null;
      end return;

   end;















         
   

   function F_Dest_Type
     (Node : Null_Lit'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Null_Lit_F_Dest_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Dest_Type;






































































































         
   

   function F_Left
     (Node : Or_Pattern'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Or_Pattern_F_Left (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Left;


         
   

   function F_Right
     (Node : Or_Pattern'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Or_Pattern_F_Right (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Right;







         
   

   function F_Expr
     (Node : Paren_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Paren_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Pattern
     (Node : Paren_Pattern'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Paren_Pattern_F_Pattern (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pattern;







         
   

   function F_Node_Name
     (Node : Parse_Node_Expr'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Parse_Node_Expr_F_Node_Name (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Node_Name;


         
   

   function F_Sub_Exprs
     (Node : Parse_Node_Expr'Class) return Grammar_Expr_List
   is
      Result : Bare_Grammar_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Parse_Node_Expr_F_Sub_Exprs (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Sub_Exprs;







         
   

   function F_Pattern
     (Node : Pattern_Match_Branch'Class) return Base_Pattern
   is
      Result : Bare_Base_Pattern;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Pattern_Match_Branch_F_Pattern (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pattern;

















         
   

   function F_Dest_Type
     (Node : Raise_Expr'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Raise_Expr_F_Dest_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Dest_Type;


         
   

   function F_Except_Expr
     (Node : Raise_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Raise_Expr_F_Except_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Except_Expr;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Ref_Id_List'Class; Index : Positive) return Ref_Id
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Ref_Id;
         end List_Child;

         

         function Ref_Id_List_First (Node : Ref_Id_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Ref_Id_List_Next
           (Node : Ref_Id_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Ref_Id_List_Has_Element
           (Node : Ref_Id_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Ref_Id_List_Element
           (Node : Ref_Id_List; Cursor : Positive) return Ref_Id'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Ref_Id'(Child.As_Ref_Id);
         end;











         
   

   function F_Quantifier
     (Node : Selector_Call'Class) return Id
   is
      Result : Bare_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Selector_Call_F_Quantifier (Node.Internal.Node);
         if Result = null then
            return No_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Quantifier;


         
   

   function F_Binding
     (Node : Selector_Call'Class) return Id
   is
      Result : Bare_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Selector_Call_F_Binding (Node.Internal.Node);
         if Result = null then
            return No_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Binding;


         
   

   function F_Selector_Call
     (Node : Selector_Call'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Selector_Call_F_Selector_Call (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Selector_Call;












         
   

   function F_Type_Name
     (Node : Simple_Type_Ref'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Simple_Type_Ref_F_Type_Name (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Name;







         
   

   function F_Binding
     (Node : Splat_Pattern'Class) return Id
   is
      Result : Bare_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Splat_Pattern_F_Binding (Node.Internal.Node);
         if Result = null then
            return No_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Binding;












         
   

   function F_Prefix
     (Node : Subscript_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subscript_Expr_F_Prefix (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prefix;


         
   

   function F_Null_Cond
     (Node : Subscript_Expr'Class) return Null_Cond_Qualifier
   is
      Result : Bare_Null_Cond_Qualifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subscript_Expr_F_Null_Cond (Node.Internal.Node);
         if Result = null then
            return No_Null_Cond_Qualifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Null_Cond;

         function F_Null_Cond (Node : Subscript_Expr'Class) return Boolean
         is (Null_Cond_Qualifier'(Node.F_Null_Cond).Kind
             = Lkt_Null_Cond_Qualifier_Present);


         
   

   function F_Index
     (Node : Subscript_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subscript_Expr_F_Index (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Index;




















         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Type_Ref_List'Class; Index : Positive) return Type_Ref
         is
            Result : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Type_Ref;
         end List_Child;

         

         function Type_Ref_List_First (Node : Type_Ref_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Type_Ref_List_Next
           (Node : Type_Ref_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Type_Ref_List_Has_Element
           (Node : Type_Ref_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Type_Ref_List_Element
           (Node : Type_Ref_List; Cursor : Positive) return Type_Ref'Class
         is
            Child : Lkt_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Type_Ref'(Child.As_Type_Ref);
         end;












         
   function P_Denoted_Value
     (Node : Token_Lit'Class) return Decoded_String_Value is
      


      Property_Result : Internal_Decoded_String_Value;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Extensions.Token_Lit_P_Denoted_Value
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Decoded_String_Value := To_Public_Decoded_String_Value (Property_Result) do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Lit
     (Node : Token_No_Case_Lit'Class) return Token_Lit
   is
      Result : Bare_Token_Lit;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Token_No_Case_Lit_F_Lit (Node.Internal.Node);
         if Result = null then
            return No_Token_Lit;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Lit;







         
   

   function F_Left
     (Node : Token_Pattern_Concat'Class) return Grammar_Expr
   is
      Result : Bare_Grammar_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Token_Pattern_Concat_F_Left (Node.Internal.Node);
         if Result = null then
            return No_Grammar_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Left;


         
   

   function F_Right
     (Node : Token_Pattern_Concat'Class) return Token_Pattern_Lit
   is
      Result : Bare_Token_Pattern_Lit;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Token_Pattern_Concat_F_Right (Node.Internal.Node);
         if Result = null then
            return No_Token_Pattern_Lit;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Right;








         
   function P_Denoted_Value
     (Node : Token_Pattern_Lit'Class) return Decoded_String_Value is
      


      Property_Result : Internal_Decoded_String_Value;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Liblktlang.Implementation.Extensions.Token_Pattern_Lit_P_Denoted_Value
            (Bare_Lkt_Node (Node.Internal.Node));

      return Result : Decoded_String_Value := To_Public_Decoded_String_Value (Property_Result) do

            Free_Internal;

      end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Token_Name
     (Node : Token_Ref'Class) return Ref_Id
   is
      Result : Bare_Ref_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Token_Ref_F_Token_Name (Node.Internal.Node);
         if Result = null then
            return No_Ref_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Token_Name;


         
   

   function F_Expr
     (Node : Token_Ref'Class) return Token_Lit
   is
      Result : Bare_Token_Lit;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Token_Ref_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Token_Lit;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;












         
   

   function F_Try_Expr
     (Node : Try_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Try_Expr_F_Try_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Try_Expr;


         
   

   function F_Or_Expr
     (Node : Try_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Try_Expr_F_Or_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Or_Expr;







         
   

   function F_Patterns
     (Node : Tuple_Pattern'Class) return Base_Pattern_List
   is
      Result : Bare_Base_Pattern_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Tuple_Pattern_F_Patterns (Node.Internal.Node);
         if Result = null then
            return No_Base_Pattern_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Patterns;







         
   

   function F_Type_Name
     (Node : Type_Pattern'Class) return Type_Ref
   is
      Result : Bare_Type_Ref;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Type_Pattern_F_Type_Name (Node.Internal.Node);
         if Result = null then
            return No_Type_Ref;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Name;







         
   

   function F_Op
     (Node : Un_Op'Class) return Op
   is
      Result : Bare_Op;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Un_Op_F_Op (Node.Internal.Node);
         if Result = null then
            return No_Op;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Op;

         function F_Op
           (Node : Un_Op'Class) return Lkt_Op
         is (Op'(Node.F_Op).Kind);

         
   

   function F_Expr
     (Node : Un_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Un_Op_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;












         
   

   function F_Expr
     (Node : Val_Decl'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Val_Decl_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Name
     (Node : Var_Bind'Class) return Ref_Id
   is
      Result : Bare_Ref_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Var_Bind_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Ref_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Expr
     (Node : Var_Bind'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Var_Bind_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;





   --------------------
   -- Children_Count --
   --------------------

   function Children_Count
     (Node : Lkt_Node'Class) return Natural is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Children_Count (Node.Internal.Node);
   end Children_Count;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index
     (Node : Lkt_Node'Class) return Natural is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return First_Child_Index (Node.Internal.Node);
   end First_Child_Index;

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index
     (Node : Lkt_Node'Class) return Natural is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Last_Child_Index (Node.Internal.Node);
   end Last_Child_Index;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Node : Lkt_Node'Class) return Lkt_Node is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

      return Node.Child (First_Child_Index (Node.Internal.Node));
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Node : Lkt_Node'Class) return Lkt_Node is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

      return Node.Child (Last_Child_Index (Node.Internal.Node));
   end Last_Child;

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : Lkt_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Lkt_Node)
   is
      N : Bare_Lkt_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Get_Child (Node.Internal.Node, Index, Index_In_Bounds, N);
      Result := Wrap_Node (N, Node.Internal.Info);
   end Get_Child;

   -----------
   -- Child --
   -----------

   function Child
     (Node  : Lkt_Node'Class;
      Index : Positive) return Lkt_Node
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Wrap_Node (Child (Node.Internal.Node, Index), Node.Internal.Info);
   end Child;

   ---------------------------
   -- Closest_Common_Parent --
   ---------------------------

   function Closest_Common_Parent
     (Self, Other : Lkt_Node'Class)
      return Lkt_Node
   is
      use Liblktlang_Support.Generic_API.Analysis;
      Lk_Self   : constant Lk_Node := To_Generic_Node (Self);
      Lk_Other  : constant Lk_Node := To_Generic_Node (Other);
      Lk_Result : constant Lk_Node := Lk_Self.Closest_Common_Parent (Lk_Other);
   begin
      return From_Generic_Node (Lk_Result);
   end Closest_Common_Parent;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : Lkt_Node'Class) return Source_Location_Range is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Sloc_Range (Node.Internal.Node);
   end Sloc_Range;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : Lkt_Node'Class;
      Sloc : Source_Location) return Relative_Position is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Compare (Node.Internal.Node, Sloc);
   end Compare;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : Lkt_Node'Class;
      Sloc : Source_Location) return Lkt_Node is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Wrap_Node (Lookup (Node.Internal.Node, Sloc));
   end Lookup;

   ----------
   -- Text --
   ----------

   function Text (Node : Lkt_Node'Class) return Text_Type is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Implementation.Text (Node.Internal.Node);
   end Text;

   -----------------
   -- Token_Range --
   -----------------

   function Token_Range
     (Node : Lkt_Node'Class) return Token_Iterator is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Token_Iterator'(Node.As_Lkt_Node,
                             Node.Internal.Node.Token_End_Index);
   end Token_Range;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Lkt_Node'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node);
      Print (Node.Internal.Node, Show_Slocs, Line_Prefix);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node : Lkt_Node'Class; Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node);
      PP_Trivia (Node.Internal.Node, Line_Prefix);
   end PP_Trivia;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : Lkt_Node'Class;
      Visit : access function (Node : Lkt_Node'Class)
              return Visit_Status)
      return Visit_Status
   is
      Info : constant Internal_Entity_Info := Node.Internal.Info;

      -------------
      -- Wrapper --
      -------------

      function Wrapper (Node : Bare_Lkt_Node) return Visit_Status
      is
         Public_Node : constant Lkt_Node :=
           Wrap_Node (Bare_Lkt_Node (Node), Info);
      begin
         return Visit (Public_Node);
      end Wrapper;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Traverse (Node.Internal.Node, Wrapper'Access);
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : Lkt_Node'Class;
      Visit : access function (Node : Lkt_Node'Class)
                               return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : Lkt_Node'Class)
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Assign_Names_To_Logic_Vars (Node.Internal.Node);
   end Assign_Names_To_Logic_Vars;

   -----------
   -- First --
   -----------

   function First (Self : Children_Array) return Natural is
   begin
      return Self.Children.First_Index;
   end First;

   ----------
   -- Last --
   ----------

   function Last (Self : Children_Array) return Natural is
   begin
      return Self.Children.Last_Index;
   end Last;

   ----------
   -- Next --
   ----------

   function Next (Self : Children_Array; Pos  : Natural) return Natural is
   begin
      pragma Unreferenced (Self);
      return Pos + 1;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Self : Children_Array; Pos  : Natural) return Natural is
   begin
      pragma Unreferenced (Self);
      return Pos - 1;
   end Previous;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : Children_Array; Pos : Natural) return Boolean
   is
   begin
      return Pos in First (Self) .. Last (Self);
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (Self : Children_Array; Pos : Natural) return Child_Record
   is
   begin
      return Self.Children (Pos);
   end Element;

   -------------------------
   -- Children_And_Trivia --
   -------------------------

   function Children_And_Trivia
     (Node : Lkt_Node'Class) return Children_Array is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      declare
         Bare_Result : constant Bare_Children_Vector :=
            Children_And_Trivia (Unwrap_Node (Node));
         Result      : Children_Array;
      begin
         for C of Bare_Result loop
            Result.Children.Append
              (Child_Record'
                 (case C.Kind is
                  when Child => (Child, Wrap_Node (C.Node)),
                  when Trivia => (Trivia, C.Trivia)));
         end loop;
         return Result;
      end;
   end Children_And_Trivia;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Self : Token_Iterator) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Token_Start (Self.Node);
   end First_Token;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Next (Tok);
   end Next_Token;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Self.Node);
      return Get_Token_Index (Tok).Token <= Self.Last;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Tok;
   end Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Context : in out Analysis_Context) is
   begin
      Context.Internal := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Context : in out Analysis_Context) is
   begin
      Inc_Ref (Unwrap_Context (Context));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Context : in out Analysis_Context) is
      Ctx : Internal_Context := Unwrap_Context (Context);
   begin
      Dec_Ref (Ctx);
      Context.Internal := null;
   end Finalize;

   ----------------------------------------------------
   -- Soft links for public/internal type converters --
   ----------------------------------------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context;
   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context;

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit;
   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit;

   function Wrap_Node
     (Node : Bare_Lkt_Node;
      Info : Internal_Entity_Info := No_Entity_Info)
      return Lkt_Node;
   function Unwrap_Node
     (Node : Lkt_Node'Class) return Bare_Lkt_Node;
   function Unwrap_Entity
     (Entity : Lkt_Node'Class) return Internal_Entity;

   ------------------
   -- Wrap_Context --
   ------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context
   is
   begin
      Inc_Ref (Context);
      return (Ada.Finalization.Controlled with
              Internal => Internal_Context_Access (Context));
   end Wrap_Context;

   --------------------
   -- Unwrap_Context --
   --------------------

   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context
   is (Internal_Context (Context.Internal));

   ---------------
   -- Wrap_Unit --
   ---------------

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit
   is (if Unit = null
       then No_Analysis_Unit
       else (Internal => Internal_Unit_Access (Unit),
             Context  => Wrap_Context (Context (Unit))));

   -----------------
   -- Unwrap_Unit --
   -----------------

   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit
   is (Internal_Unit (Unit.Internal));

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Lkt_Node'Class) is
      R  : Env_Rebindings renames Self.Internal.Info.Rebindings;
      SN : Node_Safety_Net renames Self.Safety_Net;
   begin
      if SN.Context = null then
         return;
      end if;

      --  Check that SN's context has not been released (see the Context_Pool)
      if SN.Context.Serial_Number /= SN.Context_Serial then
         raise Stale_Reference_Error with "context was released";

      --  Then check that the unit version is the same
      elsif SN.Unit.Unit_Version /= SN.Unit_Version then
         raise Stale_Reference_Error with "unit was reparsed";

      --  Then check that the R rebindings reference, if not-null, is not stale
      elsif R /= null and then R.Version /= SN.Rebindings_Version then
         raise Stale_Reference_Error with "related unit was reparsed";
      end if;
   end Check_Safety_Net;

   ---------------
   -- Wrap_Node --
   ---------------

   function Wrap_Node
     (Node : Bare_Lkt_Node;
      Info : Internal_Entity_Info := No_Entity_Info)
      return Lkt_Node is
   begin
      if Node = null then
         return No_Lkt_Node;
      end if;

      declare
         Unit               : constant Internal_Unit := Node.Unit;
         Context            : constant Internal_Context := Unit.Context;
         Rebindings_Version : constant Version_Number :=
           (if Info.Rebindings = null
            then 0
            else Info.Rebindings.Version);
      begin
         return ((Internal   => (Node, Info),
                  Safety_Net => (Context            => Context,
                                 Context_Serial     => Context.Serial_Number,
                                 Unit               => Unit,
                                 Unit_Version       => Unit.Unit_Version,
                                 Rebindings_Version => Rebindings_Version)));
      end;
   end;

   -----------------
   -- Unwrap_Node --
   -----------------

   function Unwrap_Node
     (Node : Lkt_Node'Class) return Bare_Lkt_Node
   is (Node.Internal.Node);

   -------------------
   -- Unwrap_Entity --
   -------------------

   function Unwrap_Entity
     (Entity : Lkt_Node'Class) return Internal_Entity
   is ((Entity.Internal));

   


begin
   Public_Converters.Wrap_Context := Wrap_Context'Access;
   Public_Converters.Unwrap_Context := Unwrap_Context'Access;
   Public_Converters.Wrap_Unit := Wrap_Unit'Access;
   Public_Converters.Unwrap_Unit := Unwrap_Unit'Access;
   Public_Converters.Wrap_Node := Wrap_Node'Access;
   Public_Converters.Unwrap_Node := Unwrap_Node'Access;
   Public_Converters.Unwrap_Entity := Unwrap_Entity'Access;
end Liblktlang.Analysis;
