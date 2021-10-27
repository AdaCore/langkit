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
   --  Print info about the given exception occurence

   procedure Inspect (Value : Value_Ref);
   procedure Check_Match (Value : Value_Ref; T : Type_Ref);
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

   Ctx : constant Lk_Context := Create_Context (Id);
   U   : constant Lk_Unit := Ctx.Get_From_File ("example.txt");
   N   : constant Lk_Node := U.Root;

   Value : Value_Ref;

   Int_Type, Bool_Type : Type_Ref;

begin
   Put_Title ("Value constructors/getters");

   Inspect (No_Value_Ref);

   Value := Create_Unit (Id, No_Lk_Unit);
   Inspect (Value);
   if As_Unit (Value) /= No_Lk_Unit then
      raise Program_Error;
   end if;

   Value := Create_Unit (Id, U);
   Inspect (Value);
   if As_Unit (Value) /= U then
      raise Program_Error;
   end if;

   declare
      BI : Big_Integer;
   begin
      BI.Set ("9111111111124567890");
      Value := Create_Big_Int (Id, BI);
      Inspect (Value);
      if As_Big_Int (Value) /= BI then
         raise Program_Error;
      end if;
   end;

   Value := Create_Bool (Id, True);
   Inspect (Value);
   if not As_Bool (Value) then
      raise Program_Error;
   end if;
   Value := Create_Bool (Id, False);
   Inspect (Value);
   if As_Bool (Value) then
      raise Program_Error;
   end if;
   Bool_Type := Type_Of (Value);

   Value := Create_Char (Id, 'A');
   Inspect (Value);
   if As_Char (Value) /= 'A' then
      raise Program_Error;
   end if;

   Value := Create_Int (Id, 42);
   Inspect (Value);
   if As_Int (Value) /= 42 then
      raise Program_Error;
   end if;
   Int_Type := Type_Of (Value);

   declare
      SR : constant Source_Location_Range :=
        Langkit_Support.Slocs.Value (String'("1:2-3:4"));
   begin
      Value := Create_Source_Location_Range (Id, SR);
      Inspect (Value);
      if As_Source_Location_Range (Value) /= SR then
         raise Program_Error;
      end if;
   end;

   Value := Create_String (Id, "hello, world!");
   Inspect (Value);
   if As_String (Value) /= "hello, world!" then
      raise Program_Error;
   end if;

   declare
      Token : constant Lk_Token := U.First_Token;
   begin
      Value := Create_Token (Id, Token);
      Inspect (Value);
      if As_Token (Value) /= Token then
         raise Program_Error;
      end if;
   end;

   Value := Create_Symbol (Id, "foo_bar42");
   Inspect (Value);
   if As_Symbol (Value) /= "foo_bar42" then
      raise Program_Error;
   end if;

   Value := Create_Node (Id, No_Lk_Node);
   Inspect (Value);
   if As_Node (Value) /= No_Lk_Node then
      raise Program_Error;
   end if;

   Value := Create_Node (Id, N);
   Inspect (Value);
   if As_Node (Value) /= N then
      raise Program_Error;
   end if;
   New_Line;

   Put_Title ("Type matching");

   Put_Line ("Basic cases:");
   Value := Create_Int (Id, 32);
   Check_Match (Value, Int_Type);
   Check_Match (Value, Bool_Type);
   Value := Create_Node (Id, N);
   Check_Match (Value, Bool_Type);
   New_Line;

   Put_Line ("Nodes:");
   declare
      RT : constant Type_Ref := Root_Node_Type (Id);
      DT : constant Type_Ref := From_Index (Id, Last_Derived_Type (RT));
   begin
      Check_Match (Value, RT);
      Check_Match (Value, DT);

      Value := Create_Node (Id, No_Lk_Node);
      Check_Match (Value, RT);
      Check_Match (Value, DT);
   end;
   New_Line;

   Put_Line ("Error cases:");
   Check_Match (No_Value_Ref, Int_Type);
   Check_Match (Value, No_Type_Ref);
   New_Line;
end Introspection_Values;
