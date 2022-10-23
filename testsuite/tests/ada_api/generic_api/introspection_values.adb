with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Langkit_Support.Errors;      use Langkit_Support.Errors;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;

with Libfoolang.Generic_API;

procedure Introspection_Values is

   Id : Language_Id renames Libfoolang.Generic_API.Foo_Lang_Id;

   procedure Put_Title (Label : String);
   --  Print a section title

   procedure Put_Exc (Exc : Exception_Occurrence);
   --  Print info about the given exception occurrence

   procedure Inspect (Value : Value_Ref);
   procedure Check_Match (Value : Value_Ref; T : Type_Ref);
   procedure Check_Match (Node : Lk_Node; T : Type_Ref);
   --  Helpers to test value primitives

   function Starts_With (S, Prefix : String) return Boolean
   is (S'Length >= Prefix'Length
       and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);
   --  Return whether ``S`` starts with the ``Prefix`` substring

   function Ends_With (S, Suffix : String) return Boolean
   is (S'Length >= Suffix'Length
       and then S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix);
   --  Return whether ``S`` ends with the ``Suffix`` substring

   ---------------
   -- Put_Title --
   ---------------

   procedure Put_Title (Label : String) is
   begin
      Put_Line (Label);
      Put_Line ((Label'Range => '='));
      New_Line;
   end Put_Title;

   -------------
   -- Put_Exc --
   -------------

   procedure Put_Exc (Exc : Exception_Occurrence) is
   begin
      Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
   end Put_Exc;

   -------------
   -- Inspect --
   -------------

   procedure Inspect (Value : Value_Ref) is
      Img : constant String := Image (Value);
   begin
      --  Print the debug image for this value. The image for analysis units
      --  contains an absolute path, which is inconvenient for automatic
      --  testing: try to extract the invariant part just to check that it is
      --  sensible.
      Put ("Inspect: ");
      if Value /= No_Value_Ref
         and then Debug_Name (Type_Of (Value)) = "AnalysisUnit"
         and then As_Unit (Value) /= No_Lk_Unit
      then
         declare
            Prefix : constant String := "<Unit for ";
            Suffix : constant String := "example.txt>";
         begin
            if Starts_With (Img, Prefix) and then Ends_With (Img, Suffix) then
               Put_Line (Prefix & Suffix);
            else
               raise Program_Error;
            end if;
         end;
      else
         Put_Line (Img);
      end if;

      Put ("  Type_Of: ");
      declare
         T : Type_Ref;
      begin
         T := Type_Of (Value);
         Put_Line (Debug_Name (T));
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
   end Inspect;

   -----------------
   -- Check_Match --
   -----------------

   procedure Check_Match (Value : Value_Ref; T : Type_Ref) is
   begin
      Put (Image (Value) & " matches " & Debug_Name (T) & "? ");
      if Type_Matches (Value, T) then
         Put_Line ("True");
      else
         Put_Line ("False");
      end if;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end Check_Match;

   -----------------
   -- Check_Match --
   -----------------

   procedure Check_Match (Node : Lk_Node; T : Type_Ref) is
   begin
      Put (Image (Node) & " matches " & Debug_Name (T) & "? ");
      if Type_Matches (Node, T) then
         Put_Line ("True");
      else
         Put_Line ("False");
      end if;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end Check_Match;

   Ctx : constant Lk_Context := Create_Context (Id);
   U   : constant Lk_Unit := Ctx.Get_From_File ("example.txt");
   N   : constant Lk_Node := U.Root;

   Enum     : Type_Ref := No_Type_Ref;
   Enum_Val : Enum_Value_Ref;
   Value    : Value_Ref;

   Array_Of_Node, Array_Of_Expr, Array_Of_Bigint : Type_Ref;
   Point_Struct, Node_Result_Struct              : Type_Ref;

   Node_Result_N, Point_Label : Struct_Member_Ref;
   P_Id_Bool                  : Struct_Member_Ref;
   P_Id_Int                   : Struct_Member_Ref;
   P_Id_Bigint                : Struct_Member_Ref;
   P_Id_Char                  : Struct_Member_Ref;
   P_Id_Token                 : Struct_Member_Ref;
   P_Id_Sym                   : Struct_Member_Ref;
   P_Id_Unit                  : Struct_Member_Ref;
   P_Id_Root_Node             : Struct_Member_Ref;
   P_Id_Name                  : Struct_Member_Ref;

   P_Id_Unit_Kind             : Struct_Member_Ref;

   P_Id_Node_Array            : Struct_Member_Ref;
   P_Id_Expr_Array            : Struct_Member_Ref;
   P_Id_Bigint_Array          : Struct_Member_Ref;

   P_Create_Bigint_Iterator   : Struct_Member_Ref;
   P_Id_Bigint_Iterator       : Struct_Member_Ref;

   Int_Type, Bool_Type : Type_Ref;
   False_Bool          : constant Value_Ref := From_Bool (Id, False);
   True_Bool           : constant Value_Ref := From_Bool (Id, True);
   Point_Struct_Value  : Value_Ref;
   Example_Value       : constant Value_Ref := From_Node (Id, Child (N, 1));
   Name_Value          : constant Value_Ref :=
     From_Node (Id, Child (Child (N, 2), 2));
   Bigint_Array_Value  : Value_Ref;

   function Create_Iterator return Value_Ref
   is (Eval_Member (Example_Value, P_Create_Bigint_Iterator));

begin
   --  Look for the first enum type (Enum) and build an enum value ref for it
   --  (Enum_Val). Also initialize Array_Of_Node, Array_Of_Bigint,
   --  Point_Struct, Node_Result_Struct and Example_Node.

   for TI in 1 .. Last_Type (Id) loop
      declare
         T  : constant Type_Ref := From_Index (Id, TI);
         DN : constant String := Debug_Name (T);
      begin
         if DN = "BigInt.array" then
            Array_Of_Bigint := T;
         elsif DN = "FooNode.array" then
            Array_Of_Node := T;
         elsif DN = "Expr.array" then
            Array_Of_Expr := T;
         elsif DN = "Point" then
            Point_Struct := T;
         elsif DN = "NodeResult" then
            Node_Result_Struct := T;
         elsif Is_Enum_Type (T) and then Enum = No_Type_Ref then
            Enum := T;
         end if;
      end;
   end loop;
   Enum_Val := From_Index (Enum, 1);

   --  Search for the NodeResult.n and Point.label members

   for MI in 1 .. Last_Struct_Member (Id) loop
      declare
         M  : constant Struct_Member_Ref := From_Index (Id, MI);
         DN : constant String := Debug_Name (M);
      begin
         if DN = "NodeResult.n" then
            Node_Result_N := M;
         elsif DN = "Point.label" then
            Point_Label := M;
         elsif DN = "BaseExample.p_id_bool" then
            P_Id_Bool := M;
         elsif DN = "Example.p_id_int" then
            P_Id_Int := M;
         elsif DN = "Example.p_id_bigint" then
            P_Id_Bigint := M;
         elsif DN = "Example.p_id_char" then
            P_Id_Char := M;
         elsif DN = "Example.p_id_token" then
            P_Id_Token := M;
         elsif DN = "Example.p_id_sym" then
            P_Id_Sym := M;
         elsif DN = "Example.p_id_unit" then
            P_Id_Unit := M;
         elsif DN = "Example.p_id_root_node" then
            P_Id_Root_Node := M;
         elsif DN = "Example.p_id_name" then
            P_Id_Name := M;
         elsif DN = "Example.p_id_unit_kind" then
            P_Id_Unit_Kind := M;
         elsif DN = "Example.p_id_node_array" then
            P_Id_Node_Array := M;
         elsif DN = "Example.p_id_expr_array" then
            P_Id_Expr_Array := M;
         elsif DN = "Example.p_id_bigint_array" then
            P_Id_Bigint_Array := M;
         elsif DN = "Example.p_create_bigint_iterator" then
            P_Create_Bigint_Iterator := M;
         elsif DN = "Example.p_id_bigint_iterator" then
            P_Id_Bigint_Iterator := M;
         end if;
      end;
   end loop;

   Bigint_Array_Value := Create_Array
     (Array_Of_Bigint,
      (From_Big_Int (Id, Make ("10")),
       From_Big_Int (Id, Make ("20")),
       From_Big_Int (Id, Make ("30"))));

   Put_Title ("Value comparisons");

   declare
      type Operands is record
         Left, Right : Value_Ref;
      end record;

      Checks : constant array (Positive range <>) of Operands :=
        ((No_Value_Ref, No_Value_Ref),
         (No_Value_Ref, False_Bool),
         (False_Bool, From_Bool (Id, False)),
         (True_Bool, False_Bool),
         (False_Bool, From_Int (Id, 1)));
   begin
      for C of Checks loop
         declare
            Equal : constant Boolean := C.Left = C.Right;
         begin
            Put_Line (Image (C.Left) & " = " & Image (C.Right)
                      & " => " & Equal'Image);
         end;
      end loop;
   end;
   New_Line;

   Put_Title ("Value constructors/getters");

   Inspect (No_Value_Ref);

   Value := From_Unit (Id, No_Lk_Unit);
   Inspect (Value);
   if As_Unit (Value) /= No_Lk_Unit then
      raise Program_Error;
   end if;

   Value := From_Unit (Id, U);
   Inspect (Value);
   if As_Unit (Value) /= U then
      raise Program_Error;
   end if;

   declare
      BI : Big_Integer;
   begin
      BI.Set ("9111111111124567890");
      Value := From_Big_Int (Id, BI);
      Inspect (Value);
      if As_Big_Int (Value) /= BI then
         raise Program_Error;
      end if;
   end;

   Value := From_Bool (Id, True);
   Inspect (Value);
   if not As_Bool (Value) then
      raise Program_Error;
   end if;
   Value := From_Bool (Id, False);
   Inspect (Value);
   if As_Bool (Value) then
      raise Program_Error;
   end if;
   Bool_Type := Type_Of (Value);

   Value := From_Char (Id, 'A');
   Inspect (Value);
   if As_Char (Value) /= 'A' then
      raise Program_Error;
   end if;

   Value := From_Int (Id, 42);
   Inspect (Value);
   if As_Int (Value) /= 42 then
      raise Program_Error;
   end if;
   Int_Type := Type_Of (Value);

   declare
      SR : constant Source_Location_Range :=
        Langkit_Support.Slocs.Value (String'("1:2-3:4"));
   begin
      Value := From_Source_Location_Range (Id, SR);
      Inspect (Value);
      if As_Source_Location_Range (Value) /= SR then
         raise Program_Error;
      end if;
   end;

   Value := From_String (Id, "hello, world!");
   Inspect (Value);
   if As_String (Value) /= "hello, world!" then
      raise Program_Error;
   end if;

   declare
      Token : constant Lk_Token := U.First_Token;
   begin
      Value := From_Token (Id, Token);
      Inspect (Value);
      if As_Token (Value) /= Token then
         raise Program_Error;
      end if;
   end;

   Value := From_Symbol (Id, "foo_bar42");
   Inspect (Value);
   if As_Symbol (Value) /= "foo_bar42" then
      raise Program_Error;
   end if;

   Value := From_Node (Id, No_Lk_Node);
   Inspect (Value);
   if As_Node (Value) /= No_Lk_Node then
      raise Program_Error;
   end if;

   Value := From_Node (Id, N);
   Inspect (Value);
   if As_Node (Value) /= N then
      raise Program_Error;
   end if;
   New_Line;

   Put_Title ("Enum values introspection");

   Put ("Create_Enum: null enum value ref: ");
   begin
      Value := Create_Enum (No_Enum_Value_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Value := Create_Enum (Enum_Val);
   Inspect (Value);
   if As_Enum (Value) /= Enum_Val then
      raise Program_Error;
   end if;
   New_Line;

   Put_Title ("Array values introspection");

   Put ("Create_Array: null array type: ");
   begin
      Value := Create_Array (No_Type_Ref, (1 .. 0 => <>));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Create_Array: null value reference: ");
   begin
      Value := Create_Array (Array_Of_Node, (1 => No_Value_Ref));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Create_Array: value type mismatch: ");
   begin
      Value := Create_Array (Array_Of_Node, (1 => True_Bool));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Value := Create_Array (Array_Of_Node, (1 .. 0 => <>));
   Inspect (Value);
   if Array_Length (Value) /= 0
      or else As_Array (Value)'Length /= 0
   then
      raise Program_Error;
   end if;

   declare
      N : constant Value_Ref := From_Node (Id, No_Lk_Node);
   begin
      Value := Create_Array (Array_Of_Node, (1 => N));
      Inspect (Value);
      if Array_Length (Value) /= 1
         or else As_Array (Value)'Length /= 1
         or else Array_Item (Value, 1) /= N
      then
         raise Program_Error;
      end if;
   end;

   Put ("As_Array: null value: ");
   declare
      Dummy : Integer;
   begin
      Dummy := As_Array (No_Value_Ref)'Length;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("As_Array: value type mismatch: ");
   declare
      Dummy : Integer;
   begin
      Dummy := As_Array (True_Bool)'Length;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Array_Length: null value: ");
   declare
      Dummy : Integer;
   begin
      Dummy := Array_Length (No_Value_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Array_Length: value type mismatch: ");
   declare
      Dummy : Integer;
   begin
      Dummy := Array_Length (True_Bool);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Array_Item: null value: ");
   begin
      Value := Array_Item (No_Value_Ref, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Array_Item: value type mismatch: ");
   begin
      Value := Array_Item (True_Bool, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Array_Item: index checks");
   declare
      Item    : Value_Ref;
      Success : Boolean;
   begin
      Put_Line ("  array: " & Image (Bigint_Array_Value));
      for I in 1 .. 5 loop
         Put ("  (" & I'Image & "): ");
         begin
            Item := Array_Item (Bigint_Array_Value, I);
            Success := True;
         exception
            when Exc : Precondition_Failure =>
               Put_Exc (Exc);
               Success := False;
         end;
         if Success then
            Put_Line (Image (Item));
         end if;
      end loop;
   end;
   New_Line;

   Put_Title ("Iterator values introspection");

   Put ("Iterator_Next: null value: ");
   begin
      Value := Iterator_Next (No_Value_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Iterator_Next: value type mismatch: ");
   begin
      Value := Iterator_Next (True_Bool);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Full iteration through Iterator_Next:");
   declare
      It  : constant Value_Ref := Create_Iterator;
      Elt : Value_Ref;
   begin
      Inspect (It);
      New_Line;
      loop
         Elt := Iterator_Next (It);
         exit when Elt = No_Value_Ref;
         Inspect (Elt);
      end loop;
   end;
   New_Line;

   Put_Title ("Struct values introspection");

   Put ("Create_Struct: null struct type: ");
   begin
      Value := Create_Struct (No_Type_Ref, (1 .. 0 => <>));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Create_Struct: invalid struct type: ");
   begin
      Value := Create_Struct (Root_Node_Type (Id), (1 .. 0 => <>));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Create_Struct: null value reference: ");
   begin
      Value := Create_Struct
        (Node_Result_Struct, (From_Node (Id, No_Lk_Node), No_Value_Ref));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Create_Struct: value type mismatch: ");
   begin
      Value := Create_Struct
        (Node_Result_Struct, (From_Node (Id, No_Lk_Node), False_Bool));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Create_Struct: value count mismatch: ");
   begin
      Value := Create_Struct
        (Node_Result_Struct, (1 => From_Node (Id, No_Lk_Node)));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   declare
      X, Y : Big_Integer;
   begin
      X.Set ("10");
      Y.Set ("20");
      Point_Struct_Value := Create_Struct
        (Point_Struct, (From_String (Id, "hello world!"),
                        From_Big_Int (Id, X),
                        From_Big_Int (Id, Y)));
   end;
   Inspect (Point_Struct_Value);

   Put ("Eval_Member: null struct value: ");
   begin
      Value := Eval_Member (No_Value_Ref, Point_Label);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Eval_Member: invalid struct type: ");
   begin
      Value := Eval_Member (True_Bool, Point_Label);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Eval_Member: no such member: ");
   begin
      Value := Eval_Member (Point_Struct_Value, Node_Result_N);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Eval_Member: too many arguments: ");
   begin
      Value := Eval_Member (Point_Struct_Value, Point_Label, (1 => True_Bool));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Eval_Member: argument type mismatch: ");
   begin
      Value := Eval_Member (Example_Value, P_Id_Int, (1 => True_Bool));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Eval_Member: no such node member: ");
   begin
      Value := Eval_Member (From_Node (Id, N), P_Id_Bool, (1 => True_Bool));
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put_Line
     ("Eval_Member: Point_Label on " & Image (Point_Struct_Value) & ":");
   Value := Eval_Member (Point_Struct_Value, Point_Label);
   Inspect (Value);
   New_Line;

   declare
      type Eval_Test is record
         Member   : Struct_Member_Ref;
         Input    : Value_Ref;
         Disabled : Boolean := False;
      end record;

      Tests : constant array (Positive range <>) of Eval_Test :=
        ((P_Id_Bool, True_Bool, False),
         (P_Id_Int, From_Int (Id, 42), False),
         (P_Id_Bigint,
          From_Big_Int (Id, Make ("1234567890987654321")),
          False),
         (P_Id_Char, From_Char (Id, 'A'), False),
         (P_Id_Token, From_Token (Id, N.Token_Start), False),
         (P_Id_Sym, From_Symbol (Id, "foobar"), False),
         (P_Id_Unit, From_Unit (Id, U), False),
         (P_Id_Root_Node, From_Node (Id, N), False),
         (P_Id_Name, From_Node (Id, No_Lk_Node), False),
         (P_Id_Name, Name_Value, False),

         (P_Id_Unit_Kind, Create_Enum (Enum_Val), False),

         (P_Id_Node_Array,
          Create_Array (Array_Of_Node, (1 => Example_Value)),
          False),
         (P_Id_Expr_Array,
          Create_Array (Array_Of_Expr, (1 => From_Node (Id, No_Lk_Node))),
          False),

         --  TODO (UB16-045)??? Enable this. Due to a GNAT bug, this currently
         --  runs invalid generated machine code.
         (P_Id_Bigint_Array, Bigint_Array_Value, True),

         (P_Id_Bigint_Iterator, Create_Iterator, False));
   begin
      for T of Tests loop
         if not T.Disabled then
            Put_Line ("Eval_Member: " & Debug_Name (T.Member) & ":");
            Value := Eval_Member (Example_Value, T.Member, (1 => T.Input));
            Inspect (Value);
            New_Line;
         end if;
      end loop;
   end;

   Put ("Eval_Node_Member: null node value: ");
   begin
      Value := Eval_Node_Member (No_Lk_Node, P_Id_Bool, (1 => True_Bool));
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   Put_Line ("Eval_Node_Member: P_Id_Bool:");
   Value := Eval_Node_Member
     (As_Node (Example_Value), P_Id_Bool, (1 => True_Bool));
   Inspect (Value);
   New_Line;

   Put_Title ("Type matching");

   Put_Line ("Basic cases:");
   Value := From_Int (Id, 32);
   Check_Match (Value, Int_Type);
   Check_Match (Value, Bool_Type);
   Value := From_Node (Id, N);
   Check_Match (Value, Bool_Type);
   New_Line;

   declare
      RT : constant Type_Ref := Root_Node_Type (Id);
      DT : constant Type_Ref := From_Index (Id, Last_Derived_Type (RT));
   begin
      Put_Line ("Nodes:");
      Put_Line ("... with the Value_Ref API:");
      Check_Match (Value, RT);
      Check_Match (Value, DT);

      Value := From_Node (Id, No_Lk_Node);
      Check_Match (Value, RT);
      Check_Match (Value, DT);
      New_Line;

      Put_Line ("... with the Lk_Node API:");
      Check_Match (N, RT);
      Check_Match (N, DT);

      Check_Match (No_Lk_Node, RT);
      Check_Match (No_Lk_Node, DT);
      New_Line;
   end;

   Put_Line ("Error cases:");
   Check_Match (No_Value_Ref, Int_Type);
   Check_Match (Value, No_Type_Ref);
   New_Line;
end Introspection_Values;
