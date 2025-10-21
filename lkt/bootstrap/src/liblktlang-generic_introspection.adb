
pragma Warnings (Off, "referenced");
with Liblktlang_Support.Internal.Analysis; use Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Internal.Conversions;
use Liblktlang_Support.Internal.Conversions;
with Liblktlang_Support.Slocs;             use Liblktlang_Support.Slocs;

with Liblktlang.Implementation;
with Liblktlang.Generic_API;       use Liblktlang.Generic_API;
with Liblktlang.Generic_Impl;      use Liblktlang.Generic_Impl;
with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;
with Liblktlang.Private_Converters;
use Liblktlang.Private_Converters;
pragma Warnings (On, "referenced");

package body Liblktlang.Generic_Introspection is

   


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Analysis_Unit_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Analysis_Unit_Kind) return Type_Index is
      begin
         return Type_Index_For_Analysis_Unit_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Analysis_Unit_Kind) return String is
      begin
         return "Analysis_Unit_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Analysis_Unit_Kind) return Enum_Value_Index
      is
      begin
         return Analysis_Unit_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Completion_Item_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Completion_Item_Kind) return Type_Index is
      begin
         return Type_Index_For_Completion_Item_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Completion_Item_Kind) return String is
      begin
         return "Completion_Item_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Completion_Item_Kind) return Enum_Value_Index
      is
      begin
         return Completion_Item_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Designated_Env_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Designated_Env_Kind) return Type_Index is
      begin
         return Type_Index_For_Designated_Env_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Designated_Env_Kind) return String is
      begin
         return "Designated_Env_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Designated_Env_Kind) return Enum_Value_Index
      is
      begin
         return Designated_Env_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Grammar_Rule) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Grammar_Rule) return Type_Index is
      begin
         return Type_Index_For_Grammar_Rule;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Grammar_Rule) return String is
      begin
         return "Grammar_Rule(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Grammar_Rule) return Enum_Value_Index
      is
      begin
         return Grammar_Rule'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Lookup_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Lookup_Kind) return Type_Index is
      begin
         return Type_Index_For_Lookup_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Lookup_Kind) return String is
      begin
         return "Lookup_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Lookup_Kind) return Enum_Value_Index
      is
      begin
         return Lookup_Kind'Pos (Value.Value) + 1;
      end Value_Index;


   -----------------
   -- Create_Enum --
   -----------------

   function Create_Enum
     (Enum_Type   : Type_Index;
      Value_Index : Enum_Value_Index) return Internal_Value_Access
   is
   begin
      case Enum_Type is
            when Type_Index_For_Analysis_Unit_Kind =>
               declare
                  Result : constant Internal_Acc_Analysis_Unit_Kind :=
                    new Internal_Rec_Analysis_Unit_Kind;
               begin
                  Result.Value := Analysis_Unit_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Completion_Item_Kind =>
               declare
                  Result : constant Internal_Acc_Completion_Item_Kind :=
                    new Internal_Rec_Completion_Item_Kind;
               begin
                  Result.Value := Completion_Item_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Designated_Env_Kind =>
               declare
                  Result : constant Internal_Acc_Designated_Env_Kind :=
                    new Internal_Rec_Designated_Env_Kind;
               begin
                  Result.Value := Designated_Env_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Grammar_Rule =>
               declare
                  Result : constant Internal_Acc_Grammar_Rule :=
                    new Internal_Rec_Grammar_Rule;
               begin
                  Result.Value := Grammar_Rule'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Lookup_Kind =>
               declare
                  Result : constant Internal_Acc_Lookup_Kind :=
                    new Internal_Rec_Lookup_Kind;
               begin
                  Result.Value := Lookup_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-enum types.
            raise Program_Error;
      end case;
   end Create_Enum;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Complete_Item_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Complete_Item_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Complete_Item_Array) return Type_Index is
      begin
         return Type_Index_For_Complete_Item_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Complete_Item_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Complete_Item_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Analysis.Complete_Item renames Value.Value.all (Index);

         
            Result : Internal_Acc_Complete_Item :=  new Internal_Rec_Complete_Item;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Complete_Item_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Complete_Item_Array := new Internal_Rec_Complete_Item_Array do
            Result.Value := new Analysis.Complete_Item_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Analysis.Complete_Item renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Complete_Item renames
                    Internal_Acc_Complete_Item (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Lkt_Node_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Lkt_Node_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Lkt_Node_Array) return Type_Index is
      begin
         return Type_Index_For_Lkt_Node_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Lkt_Node_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Lkt_Node_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Analysis.Lkt_Node renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Lkt_Node_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Lkt_Node_Array := new Internal_Rec_Lkt_Node_Array do
            Result.Value := new Analysis.Lkt_Node_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Analysis.Lkt_Node renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value);
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Def_Id_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Def_Id_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Def_Id_Array) return Type_Index is
      begin
         return Type_Index_For_Def_Id_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Def_Id_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Def_Id_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Analysis.Def_Id renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Def_Id_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Def_Id_Array := new Internal_Rec_Def_Id_Array do
            Result.Value := new Analysis.Def_Id_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Analysis.Def_Id renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Def_Id;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Fun_Decl_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Fun_Decl_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Fun_Decl_Array) return Type_Index is
      begin
         return Type_Index_For_Fun_Decl_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Fun_Decl_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Fun_Decl_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Analysis.Fun_Decl renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Fun_Decl_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Fun_Decl_Array := new Internal_Rec_Fun_Decl_Array do
            Result.Value := new Analysis.Fun_Decl_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Analysis.Fun_Decl renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Fun_Decl;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Logic_Context_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Logic_Context_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Logic_Context_Array) return Type_Index is
      begin
         return Type_Index_For_Logic_Context_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Logic_Context_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Logic_Context_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Analysis.Logic_Context renames Value.Value.all (Index);

         
            Result : Internal_Acc_Logic_Context :=  new Internal_Rec_Logic_Context;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Logic_Context_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Logic_Context_Array := new Internal_Rec_Logic_Context_Array do
            Result.Value := new Analysis.Logic_Context_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Analysis.Logic_Context renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Logic_Context renames
                    Internal_Acc_Logic_Context (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Ref_Result_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Ref_Result_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Ref_Result_Array) return Type_Index is
      begin
         return Type_Index_For_Ref_Result_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Ref_Result_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Ref_Result_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Analysis.Ref_Result renames Value.Value.all (Index);

         
            Result : Internal_Acc_Ref_Result :=  new Internal_Rec_Ref_Result;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Ref_Result_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Ref_Result_Array := new Internal_Rec_Ref_Result_Array do
            Result.Value := new Analysis.Ref_Result_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Analysis.Ref_Result renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Ref_Result renames
                    Internal_Acc_Ref_Result (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Solver_Diagnostic_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Solver_Diagnostic_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Solver_Diagnostic_Array) return Type_Index is
      begin
         return Type_Index_For_Solver_Diagnostic_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Solver_Diagnostic_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Solver_Diagnostic_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Analysis.Solver_Diagnostic renames Value.Value.all (Index);

         
            Result : Internal_Acc_Solver_Diagnostic :=  new Internal_Rec_Solver_Diagnostic;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Solver_Diagnostic_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Solver_Diagnostic_Array := new Internal_Rec_Solver_Diagnostic_Array do
            Result.Value := new Analysis.Solver_Diagnostic_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Analysis.Solver_Diagnostic renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Solver_Diagnostic renames
                    Internal_Acc_Solver_Diagnostic (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Analysis_Unit_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Analysis_Unit_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Analysis_Unit_Array) return Type_Index is
      begin
         return Type_Index_For_Analysis_Unit_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Analysis_Unit_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Analysis_Unit_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Analysis.Analysis_Unit renames Value.Value.all (Index);

         
            Result : Internal_Acc_Analysis_Unit :=  new Internal_Rec_Analysis_Unit;
      begin
            Set_Unit (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Analysis_Unit_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Analysis_Unit_Array := new Internal_Rec_Analysis_Unit_Array do
            Result.Value := new Analysis.Analysis_Unit_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Analysis.Analysis_Unit renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Analysis_Unit renames
                    Internal_Acc_Analysis_Unit (Values (I)).all;
               begin
                     Result_Item := Get_Unit (Value);
               end;
            end loop;
         end return;
      end Create_Array;


   ------------------
   -- Create_Array --
   ------------------

   function Create_Array
     (Array_Type : Type_Index;
      Values     : Internal_Value_Array) return Internal_Value_Access is
   begin
      case Array_Type is
            when Type_Index_For_Complete_Item_Array =>
               declare
                  Result : constant Internal_Acc_Complete_Item_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Lkt_Node_Array =>
               declare
                  Result : constant Internal_Acc_Lkt_Node_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Def_Id_Array =>
               declare
                  Result : constant Internal_Acc_Def_Id_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Fun_Decl_Array =>
               declare
                  Result : constant Internal_Acc_Fun_Decl_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Logic_Context_Array =>
               declare
                  Result : constant Internal_Acc_Logic_Context_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Ref_Result_Array =>
               declare
                  Result : constant Internal_Acc_Ref_Result_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Solver_Diagnostic_Array =>
               declare
                  Result : constant Internal_Acc_Solver_Diagnostic_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Analysis_Unit_Array =>
               declare
                  Result : constant Internal_Acc_Analysis_Unit_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-array types.
            raise Program_Error;
      end case;
   end Create_Array;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Complete_Item) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Complete_Item) return Type_Index is
      begin
         return Type_Index_For_Complete_Item;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Complete_Item
      is
         
            F_Declaration : Analysis.Decl := Get_Node (Internal_Acc_Node (Values (1)).all).As_Decl;
      begin

         return Result : constant Internal_Acc_Complete_Item := new Internal_Rec_Complete_Item do
            Result.Value := Create_Complete_Item
                (F_Declaration)
            ;
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Complete_Item;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Complete_Item_Declaration =>
                  declare
                     Item : constant Decl
                           'Class
                     := Analysis.Declaration (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               return (raise Program_Error);
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Decoded_Char_Value) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Decoded_Char_Value) return Type_Index is
      begin
         return Type_Index_For_Decoded_Char_Value;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Decoded_Char_Value
      is
         
            F_Value : Character_Type renames Internal_Acc_Char (Values (1)).Value;
            F_Has_Error : Boolean renames Internal_Acc_Bool (Values (2)).Value;
            F_Error_Sloc : Source_Location renames Internal_Acc_Source_Location (Values (3)).Value;
            F_Error_Message : Text_Type := To_Text (Internal_Acc_String (Values (4)).Value);
      begin

         return Result : constant Internal_Acc_Decoded_Char_Value := new Internal_Rec_Decoded_Char_Value do
            Result.Value := Create_Decoded_Char_Value
                (F_Value, F_Has_Error, F_Error_Sloc, F_Error_Message)
            ;
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Decoded_Char_Value;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Decoded_Char_Value_Value =>
                  declare
                     Item : constant Character_Type
                     := Analysis.Value (Value.Value);

                     

                        Result : Internal_Acc_Char :=  new Internal_Rec_Char;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Decoded_Char_Value_Has_Error =>
                  declare
                     Item : constant Boolean
                     := Analysis.Has_Error (Value.Value);

                     

                        Result : Internal_Acc_Bool :=  new Internal_Rec_Bool;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Decoded_Char_Value_Error_Sloc =>
                  declare
                     Item : constant Source_Location
                     := Analysis.Error_Sloc (Value.Value);

                     

                        Result : Internal_Acc_Source_Location :=  new Internal_Rec_Source_Location;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Decoded_Char_Value_Error_Message =>
                  declare
                     Item : constant Text_Type
                     := Analysis.Error_Message (Value.Value);

                     

                        Result : Internal_Acc_String :=  new Internal_Rec_String;
                  begin
                        Result.Value := To_Unbounded_Text (Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               return (raise Program_Error);
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Decoded_String_Value) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Decoded_String_Value) return Type_Index is
      begin
         return Type_Index_For_Decoded_String_Value;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Decoded_String_Value
      is
         
            F_Value : Text_Type := To_Text (Internal_Acc_String (Values (1)).Value);
            F_Has_Error : Boolean renames Internal_Acc_Bool (Values (2)).Value;
            F_Error_Sloc : Source_Location renames Internal_Acc_Source_Location (Values (3)).Value;
            F_Error_Message : Text_Type := To_Text (Internal_Acc_String (Values (4)).Value);
      begin

         return Result : constant Internal_Acc_Decoded_String_Value := new Internal_Rec_Decoded_String_Value do
            Result.Value := Create_Decoded_String_Value
                (F_Value, F_Has_Error, F_Error_Sloc, F_Error_Message)
            ;
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Decoded_String_Value;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Decoded_String_Value_Value =>
                  declare
                     Item : constant Text_Type
                     := Analysis.Value (Value.Value);

                     

                        Result : Internal_Acc_String :=  new Internal_Rec_String;
                  begin
                        Result.Value := To_Unbounded_Text (Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Decoded_String_Value_Has_Error =>
                  declare
                     Item : constant Boolean
                     := Analysis.Has_Error (Value.Value);

                     

                        Result : Internal_Acc_Bool :=  new Internal_Rec_Bool;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Decoded_String_Value_Error_Sloc =>
                  declare
                     Item : constant Source_Location
                     := Analysis.Error_Sloc (Value.Value);

                     

                        Result : Internal_Acc_Source_Location :=  new Internal_Rec_Source_Location;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Decoded_String_Value_Error_Message =>
                  declare
                     Item : constant Text_Type
                     := Analysis.Error_Message (Value.Value);

                     

                        Result : Internal_Acc_String :=  new Internal_Rec_String;
                  begin
                        Result.Value := To_Unbounded_Text (Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               return (raise Program_Error);
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Logic_Context) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Logic_Context) return Type_Index is
      begin
         return Type_Index_For_Logic_Context;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Logic_Context
      is
         
            F_Ref_Node : Analysis.Lkt_Node := Get_Node (Internal_Acc_Node (Values (1)).all);
            F_Decl_Node : Analysis.Lkt_Node := Get_Node (Internal_Acc_Node (Values (2)).all);
      begin

         return Result : constant Internal_Acc_Logic_Context := new Internal_Rec_Logic_Context do
            Result.Value := Create_Logic_Context
                (F_Ref_Node, F_Decl_Node)
            ;
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Logic_Context;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Logic_Context_Ref_Node =>
                  declare
                     Item : constant Lkt_Node
                           'Class
                     := Analysis.Ref_Node (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Logic_Context_Decl_Node =>
                  declare
                     Item : constant Lkt_Node
                           'Class
                     := Analysis.Decl_Node (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               return (raise Program_Error);
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Ref_Result) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Ref_Result) return Type_Index is
      begin
         return Type_Index_For_Ref_Result;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Ref_Result
      is
         
            F_Ref : Analysis.Ref_Id := Get_Node (Internal_Acc_Node (Values (1)).all).As_Ref_Id;
      begin

         return Result : constant Internal_Acc_Ref_Result := new Internal_Rec_Ref_Result do
            Result.Value := Create_Ref_Result
                (F_Ref)
            ;
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Ref_Result;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Ref_Result_Ref =>
                  declare
                     Item : constant Ref_Id
                           'Class
                     := Analysis.Ref (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               return (raise Program_Error);
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Solver_Diagnostic) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Solver_Diagnostic) return Type_Index is
      begin
         return Type_Index_For_Solver_Diagnostic;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Solver_Diagnostic
      is
         
            F_Message_Template : Text_Type := To_Text (Internal_Acc_String (Values (1)).Value);
            F_Args : Analysis.Lkt_Node_Array renames Internal_Acc_Lkt_Node_Array (Values (2)).Value.all;
            F_Location : Analysis.Lkt_Node := Get_Node (Internal_Acc_Node (Values (3)).all);
            F_Contexts : Analysis.Logic_Context_Array renames Internal_Acc_Logic_Context_Array (Values (4)).Value.all;
            F_Round : Integer renames Internal_Acc_Int (Values (5)).Value;
      begin

         return Result : constant Internal_Acc_Solver_Diagnostic := new Internal_Rec_Solver_Diagnostic do
            Result.Value := Create_Solver_Diagnostic
                (F_Message_Template, F_Args, F_Location, F_Contexts, F_Round)
            ;
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Solver_Diagnostic;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Solver_Diagnostic_Message_Template =>
                  declare
                     Item : constant Text_Type
                     := Analysis.Message_Template (Value.Value);

                     

                        Result : Internal_Acc_String :=  new Internal_Rec_String;
                  begin
                        Result.Value := To_Unbounded_Text (Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Solver_Diagnostic_Args =>
                  declare
                     Item : constant Lkt_Node_Array
                     := Analysis.Args (Value.Value);

                     

                        Result : Internal_Acc_Lkt_Node_Array :=  new Internal_Rec_Lkt_Node_Array;
                  begin
                        Result.Value := new Analysis.Lkt_Node_Array'(Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Solver_Diagnostic_Location =>
                  declare
                     Item : constant Lkt_Node
                           'Class
                     := Analysis.Location (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Solver_Diagnostic_Contexts =>
                  declare
                     Item : constant Logic_Context_Array
                     := Analysis.Contexts (Value.Value);

                     

                        Result : Internal_Acc_Logic_Context_Array :=  new Internal_Rec_Logic_Context_Array;
                  begin
                        Result.Value := new Analysis.Logic_Context_Array'(Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Solver_Diagnostic_Round =>
                  declare
                     Item : constant Integer
                     := Analysis.Round (Value.Value);

                     

                        Result : Internal_Acc_Int :=  new Internal_Rec_Int;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               return (raise Program_Error);
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Solver_Result) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Solver_Result) return Type_Index is
      begin
         return Type_Index_For_Solver_Result;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Solver_Result
      is
         
            F_Success : Boolean renames Internal_Acc_Bool (Values (1)).Value;
            F_Diagnostics : Analysis.Solver_Diagnostic_Array renames Internal_Acc_Solver_Diagnostic_Array (Values (2)).Value.all;
      begin

         return Result : constant Internal_Acc_Solver_Result := new Internal_Rec_Solver_Result do
            Result.Value := Create_Solver_Result
                (F_Success, F_Diagnostics)
            ;
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Solver_Result;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Solver_Result_Success =>
                  declare
                     Item : constant Boolean
                     := Analysis.Success (Value.Value);

                     

                        Result : Internal_Acc_Bool :=  new Internal_Rec_Bool;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Solver_Result_Diagnostics =>
                  declare
                     Item : constant Solver_Diagnostic_Array
                     := Analysis.Diagnostics (Value.Value);

                     

                        Result : Internal_Acc_Solver_Diagnostic_Array :=  new Internal_Rec_Solver_Diagnostic_Array;
                  begin
                        Result.Value := new Analysis.Solver_Diagnostic_Array'(Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               return (raise Program_Error);
         end case;
      end Eval_Member;


   -------------------
   -- Create_Struct --
   -------------------

   function Create_Struct
     (Struct_Type : Type_Index;
      Values      : Internal_Value_Array) return Internal_Value_Access is
   begin

      case Struct_Type is
            when Type_Index_For_Complete_Item =>
               declare
                  Result : constant Internal_Acc_Complete_Item :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Decoded_Char_Value =>
               declare
                  Result : constant Internal_Acc_Decoded_Char_Value :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Decoded_String_Value =>
               declare
                  Result : constant Internal_Acc_Decoded_String_Value :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Logic_Context =>
               declare
                  Result : constant Internal_Acc_Logic_Context :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Ref_Result =>
               declare
                  Result : constant Internal_Acc_Ref_Result :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Solver_Diagnostic =>
               declare
                  Result : constant Internal_Acc_Solver_Diagnostic :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Solver_Result =>
               declare
                  Result : constant Internal_Acc_Solver_Result :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-array types.
            return (raise Program_Error);
      end case;
   end Create_Struct;

   ----------------------
   -- Eval_Node_Member --
   ----------------------

   function Eval_Node_Member
     (Node      : Internal_Acc_Node;
      Member    : Struct_Member_Index;
      Arguments : Internal_Value_Array) return Internal_Value_Access
   is
      Int_Entity : constant Implementation.Internal_Entity :=
        +Liblktlang_Support.Internal.Conversions.Unwrap_Node (Node.Value);
      N          : constant Lkt_Node :=
        Public_Converters.Wrap_Node.all (Int_Entity.Node, Int_Entity.Info);
      Kind       : constant Lkt_Node_Kind_Type := N.Kind;
      Result     : Internal_Value_Access;
   begin
      

      case Member is
when Member_Index_For_Parent =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.Parent);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Parents =>
declare
Arg_With_Self : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Lkt_Node_Array :=  new Internal_Rec_Lkt_Node_Array;
begin
R.Value := new Analysis.Lkt_Node_Array'(N.Parents (Arg_With_Self));
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when Member_Index_For_Children =>
declare
R : Internal_Acc_Lkt_Node_Array :=  new Internal_Rec_Lkt_Node_Array;
begin
R.Value := new Analysis.Lkt_Node_Array'(N.Children);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Token_Start =>
declare
R : Internal_Acc_Token :=  new Internal_Rec_Token;
begin
R.Value := To_Generic (N.Token_Start);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Token_End =>
declare
R : Internal_Acc_Token :=  new Internal_Rec_Token;
begin
R.Value := To_Generic (N.Token_End);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Child_Index =>
declare
R : Internal_Acc_Int :=  new Internal_Rec_Int;
begin
R.Value := N.Child_Index;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Previous_Sibling =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.Previous_Sibling);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Next_Sibling =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.Next_Sibling);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Unit =>
declare
R : Internal_Acc_Analysis_Unit :=  new Internal_Rec_Analysis_Unit;
begin
Set_Unit (R, N.Unit);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Is_Ghost =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N.Is_Ghost;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Full_Sloc_Image =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N.Full_Sloc_Image);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Completion_Item_Kind_To_Int =>
declare
Arg_Kind : Common.Completion_Item_Kind renames Internal_Acc_Completion_Item_Kind (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Int :=  new Internal_Rec_Int;
begin
R.Value := N.Completion_Item_Kind_To_Int (Arg_Kind);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when Member_Index_For_Lkt_Node_P_Set_Solver_Debug_Mode =>
declare
Arg_Enable : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N.P_Set_Solver_Debug_Mode (Arg_Enable);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when Member_Index_For_Lkt_Node_P_Basic_Trait_Gen =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Basic_Trait_Gen);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Basic_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Basic_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Node_Gen_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Node_Gen_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Node_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Node_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Indexable_Gen_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Indexable_Gen_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Indexable_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Indexable_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Token_Node_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Token_Node_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Error_Node_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Error_Node_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Char_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Char_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Int_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Int_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Bool_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Bool_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Bigint_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Bigint_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_String_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_String_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Symbol_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Symbol_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Property_Error_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Property_Error_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Regexp_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Regexp_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Entity_Gen_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Entity_Gen_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Entity_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Entity_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Logicvar_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Logicvar_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Equation_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Equation_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Array_Gen_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Array_Gen_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Array_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Array_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Astlist_Gen_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Astlist_Gen_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Astlist_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Astlist_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Node_Builder_Gen_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Node_Builder_Gen_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Node_Builder_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Node_Builder_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Iterator_Gen_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Iterator_Gen_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Iterator_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Iterator_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Analysis_Unit_Gen_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Analysis_Unit_Gen_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Analysis_Unit_Trait =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Analysis_Unit_Trait);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Topmost_Invalid_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Topmost_Invalid_Decl);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Nameres_Diagnostics =>
declare
R : Internal_Acc_Solver_Diagnostic_Array :=  new Internal_Rec_Solver_Diagnostic_Array;
begin
R.Value := new Analysis.Solver_Diagnostic_Array'(N.P_Nameres_Diagnostics);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Solve_Enclosing_Context =>
declare
R : Internal_Acc_Solver_Result :=  new Internal_Rec_Solver_Result;
begin
R.Value := N.P_Solve_Enclosing_Context;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Xref_Entry_Point =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N.P_Xref_Entry_Point;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lkt_Node_P_Complete =>
declare
R : Internal_Acc_Complete_Item_Array :=  new Internal_Rec_Complete_Item_Array;
begin
R.Value := new Analysis.Complete_Item_Array'(N.P_Complete);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Lkt_Node (Kind) is
when Lkt_Argument_Range =>
declare
N_Bare_Argument : constant Analysis.Argument := N.As_Argument;
begin
case Member is
when Member_Index_For_Argument_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Argument.F_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Argument_F_Value =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Argument.F_Value);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Lexer_Case_Rule_Cond_Alt_Range =>
declare
N_Bare_Lexer_Case_Rule_Cond_Alt : constant Analysis.Lexer_Case_Rule_Cond_Alt := N.As_Lexer_Case_Rule_Cond_Alt;
begin
case Member is
when Member_Index_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lexer_Case_Rule_Cond_Alt.F_Cond_Exprs);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lexer_Case_Rule_Cond_Alt_F_Send =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lexer_Case_Rule_Cond_Alt.F_Send);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Lexer_Case_Rule_Default_Alt_Range =>
declare
N_Bare_Lexer_Case_Rule_Default_Alt : constant Analysis.Lexer_Case_Rule_Default_Alt := N.As_Lexer_Case_Rule_Default_Alt;
begin
case Member is
when Member_Index_For_Lexer_Case_Rule_Default_Alt_F_Send =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lexer_Case_Rule_Default_Alt.F_Send);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Base_Match_Branch =>
declare
N_Bare_Base_Match_Branch : constant Analysis.Base_Match_Branch := N.As_Base_Match_Branch;
begin
case Member is
when Member_Index_For_Base_Match_Branch_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Match_Branch.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Base_Match_Branch_P_Match_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Match_Branch.P_Match_Part);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Base_Match_Branch (Kind) is
when Lkt_Match_Branch_Range =>
declare
N_Bare_Match_Branch : constant Analysis.Match_Branch := N_Bare_Base_Match_Branch.As_Match_Branch;
begin
case Member is
when Member_Index_For_Match_Branch_F_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Match_Branch.F_Decl);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Pattern_Match_Branch_Range =>
declare
N_Bare_Pattern_Match_Branch : constant Analysis.Pattern_Match_Branch := N_Bare_Base_Match_Branch.As_Pattern_Match_Branch;
begin
case Member is
when Member_Index_For_Pattern_Match_Branch_F_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Pattern_Match_Branch.F_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Lkt_Block_Expr_Clause_Range =>
declare
N_Bare_Block_Expr_Clause : constant Analysis.Block_Expr_Clause := N.As_Block_Expr_Clause;
begin
case Member is
when Member_Index_For_Block_Expr_Clause_F_Clause =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Block_Expr_Clause.F_Clause);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Class_Qualifier =>
declare
N_Bare_Class_Qualifier : constant Analysis.Class_Qualifier := N.As_Class_Qualifier;
begin
case Member is
when Member_Index_For_Class_Qualifier_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Class_Qualifier.P_As_Bool;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Decl =>
declare
N_Bare_Decl : constant Analysis.Decl := N.As_Decl;
begin
case Member is
when Member_Index_For_Decl_F_Syn_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl.F_Syn_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_Custom_Image =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Decl.P_Custom_Image);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_Decl_Type_Name =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Decl.P_Decl_Type_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_Def_Ids =>
declare
R : Internal_Acc_Def_Id_Array :=  new Internal_Rec_Def_Id_Array;
begin
R.Value := new Analysis.Def_Id_Array'(N_Bare_Decl.P_Def_Ids);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_As_Bare_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl.P_As_Bare_Decl);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_Get_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl.P_Get_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_Get_Cast_Type =>
declare
Arg_Cast_To : Analysis.Type_Decl := Get_Node (Internal_Acc_Node (Arguments (1)).all).As_Type_Decl;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl.P_Get_Cast_Type (Arg_Cast_To));
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when Member_Index_For_Decl_P_Get_Keep_Type =>
declare
Arg_Keep_Type : Analysis.Type_Decl := Get_Node (Internal_Acc_Node (Arguments (1)).all).As_Type_Decl;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl.P_Get_Keep_Type (Arg_Keep_Type));
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when Member_Index_For_Decl_P_Get_Suffix_Type =>
declare
Arg_Prefix_Type : Analysis.Type_Decl := Get_Node (Internal_Acc_Node (Arguments (1)).all).As_Type_Decl;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl.P_Get_Suffix_Type (Arg_Prefix_Type));
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when Member_Index_For_Decl_P_Is_Generic =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Decl.P_Is_Generic;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_Return_Type_Is_Instantiated =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Decl.P_Return_Type_Is_Instantiated;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_Is_Instantiated =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Decl.P_Is_Instantiated;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_Name =>
declare
R : Internal_Acc_Symbol :=  new Internal_Rec_Symbol;
begin
R.Value := N_Bare_Decl.P_Name;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_P_Full_Name =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Decl.P_Full_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Decl (Kind) is
when Lkt_Base_Grammar_Rule_Decl =>
declare
N_Bare_Base_Grammar_Rule_Decl : constant Analysis.Base_Grammar_Rule_Decl := N_Bare_Decl.As_Base_Grammar_Rule_Decl;
begin
case Member is
when Member_Index_For_Base_Grammar_Rule_Decl_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Grammar_Rule_Decl.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Explicitly_Typed_Decl =>
declare
N_Bare_Explicitly_Typed_Decl : constant Analysis.Explicitly_Typed_Decl := N_Bare_Decl.As_Explicitly_Typed_Decl;
begin
case Member is
when Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Explicitly_Typed_Decl.F_Decl_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Explicitly_Typed_Decl (Kind) is
when Lkt_Component_Decl =>
declare
N_Bare_Component_Decl : constant Analysis.Component_Decl := N_Bare_Explicitly_Typed_Decl.As_Component_Decl;
begin
case Member is
when Member_Index_For_Component_Decl_F_Default_Val =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Decl.F_Default_Val);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Component_Decl (Kind) is
when Lkt_Field_Decl_Range =>
declare
N_Bare_Field_Decl : constant Analysis.Field_Decl := N_Bare_Component_Decl.As_Field_Decl;
begin
case Member is
when Member_Index_For_Field_Decl_F_Trait_Ref =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Field_Decl.F_Trait_Ref);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Fun_Param_Decl_Range =>
declare
N_Bare_Fun_Param_Decl : constant Analysis.Fun_Param_Decl := N_Bare_Component_Decl.As_Fun_Param_Decl;
begin
case Member is
when Member_Index_For_Fun_Param_Decl_F_Decl_Annotations =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Fun_Param_Decl.F_Decl_Annotations);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Lkt_Val_Decl_Range =>
declare
N_Bare_Val_Decl : constant Analysis.Val_Decl := N_Bare_Explicitly_Typed_Decl.As_Val_Decl;
begin
case Member is
when Member_Index_For_Val_Decl_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Val_Decl.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Lkt_Fun_Decl_Range =>
declare
N_Bare_Fun_Decl : constant Analysis.Fun_Decl := N_Bare_Decl.As_Fun_Decl;
begin
case Member is
when Member_Index_For_Fun_Decl_F_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Fun_Decl.F_Params);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Fun_Decl_F_Return_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Fun_Decl.F_Return_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Fun_Decl_F_Trait_Ref =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Fun_Decl.F_Trait_Ref);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Fun_Decl_F_Body =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Fun_Decl.F_Body);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Fun_Decl_P_Is_Dynamic_Combiner =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Fun_Decl.P_Is_Dynamic_Combiner;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Fun_Decl_P_Find_All_Overrides =>
declare
Arg_Units : Analysis.Analysis_Unit_Array renames Internal_Acc_Analysis_Unit_Array (Arguments (1)).Value.all;
begin
declare
R : Internal_Acc_Fun_Decl_Array :=  new Internal_Rec_Fun_Decl_Array;
begin
R.Value := new Analysis.Fun_Decl_Array'(N_Bare_Fun_Decl.P_Find_All_Overrides (Arg_Units));
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when others => null;
end case;
end;
when Lkt_Env_Spec_Decl_Range =>
declare
N_Bare_Env_Spec_Decl : constant Analysis.Env_Spec_Decl := N_Bare_Decl.As_Env_Spec_Decl;
begin
case Member is
when Member_Index_For_Env_Spec_Decl_F_Actions =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Env_Spec_Decl.F_Actions);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Generic_Decl_Range =>
declare
N_Bare_Generic_Decl : constant Analysis.Generic_Decl := N_Bare_Decl.As_Generic_Decl;
begin
case Member is
when Member_Index_For_Generic_Decl_F_Generic_Param_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Decl.F_Generic_Param_Decls);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Generic_Decl_F_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Decl.F_Decl);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Decl_Range =>
declare
N_Bare_Grammar_Decl : constant Analysis.Grammar_Decl := N_Bare_Decl.As_Grammar_Decl;
begin
case Member is
when Member_Index_For_Grammar_Decl_F_Rules =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Decl.F_Rules);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Lexer_Decl_Range =>
declare
N_Bare_Lexer_Decl : constant Analysis.Lexer_Decl := N_Bare_Decl.As_Lexer_Decl;
begin
case Member is
when Member_Index_For_Lexer_Decl_F_Rules =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lexer_Decl.F_Rules);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Lexer_Family_Decl_Range =>
declare
N_Bare_Lexer_Family_Decl : constant Analysis.Lexer_Family_Decl := N_Bare_Decl.As_Lexer_Family_Decl;
begin
case Member is
when Member_Index_For_Lexer_Family_Decl_F_Rules =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lexer_Family_Decl.F_Rules);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Type_Decl =>
declare
N_Bare_Type_Decl : constant Analysis.Type_Decl := N_Bare_Decl.As_Type_Decl;
begin
case Member is
when Member_Index_For_Type_Decl_F_Traits =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Decl.F_Traits);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Type_Decl_F_Syn_Base_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Decl.F_Syn_Base_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Type_Decl_P_Def_Id =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Decl.P_Def_Id);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Type_Decl_P_Base_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Decl.P_Base_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Type_Decl_P_Base_Type_If_Entity =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Decl.P_Base_Type_If_Entity);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Type_Decl (Kind) is
when Lkt_Generic_Param_Type_Decl_Range =>
declare
N_Bare_Generic_Param_Type_Decl : constant Analysis.Generic_Param_Type_Decl := N_Bare_Type_Decl.As_Generic_Param_Type_Decl;
begin
case Member is
when Member_Index_For_Generic_Param_Type_Decl_F_Has_Class =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Param_Type_Decl.F_Has_Class);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Named_Type_Decl =>
declare
N_Bare_Named_Type_Decl : constant Analysis.Named_Type_Decl := N_Bare_Type_Decl.As_Named_Type_Decl;
begin
case Member is
when Member_Index_For_Named_Type_Decl_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Named_Type_Decl.F_Decls);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Named_Type_Decl (Kind) is
when Lkt_Enum_Class_Decl_Range =>
declare
N_Bare_Enum_Class_Decl : constant Analysis.Enum_Class_Decl := N_Bare_Named_Type_Decl.As_Enum_Class_Decl;
begin
case Member is
when Member_Index_For_Enum_Class_Decl_F_Branches =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Enum_Class_Decl.F_Branches);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Enum_Type_Decl_Range =>
declare
N_Bare_Enum_Type_Decl : constant Analysis.Enum_Type_Decl := N_Bare_Named_Type_Decl.As_Enum_Type_Decl;
begin
case Member is
when Member_Index_For_Enum_Type_Decl_F_Literals =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Enum_Type_Decl.F_Literals);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Lkt_Decl_Annotation_Range =>
declare
N_Bare_Decl_Annotation : constant Analysis.Decl_Annotation := N.As_Decl_Annotation;
begin
case Member is
when Member_Index_For_Decl_Annotation_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl_Annotation.F_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Decl_Annotation_F_Args =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl_Annotation.F_Args);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Decl_Annotation_Args_Range =>
declare
N_Bare_Decl_Annotation_Args : constant Analysis.Decl_Annotation_Args := N.As_Decl_Annotation_Args;
begin
case Member is
when Member_Index_For_Decl_Annotation_Args_F_Args =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl_Annotation_Args.F_Args);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Elsif_Branch_Range =>
declare
N_Bare_Elsif_Branch : constant Analysis.Elsif_Branch := N.As_Elsif_Branch;
begin
case Member is
when Member_Index_For_Elsif_Branch_F_Cond_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Elsif_Branch.F_Cond_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Elsif_Branch_F_Then_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Elsif_Branch.F_Then_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Enum_Class_Case_Range =>
declare
N_Bare_Enum_Class_Case : constant Analysis.Enum_Class_Case := N.As_Enum_Class_Case;
begin
case Member is
when Member_Index_For_Enum_Class_Case_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Enum_Class_Case.F_Decls);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Excludes_Null =>
declare
N_Bare_Excludes_Null : constant Analysis.Excludes_Null := N.As_Excludes_Null;
begin
case Member is
when Member_Index_For_Excludes_Null_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Excludes_Null.P_As_Bool;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Expr =>
declare
N_Bare_Expr : constant Analysis.Expr := N.As_Expr;
begin
case Member is
when Member_Index_For_Expr_P_Get_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Expr.P_Get_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Expr_P_Get_Generic_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Expr.P_Get_Generic_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Expr_P_Get_Expected_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Expr.P_Get_Expected_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Expr_P_Referenced_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Expr.P_Referenced_Decl);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Expr (Kind) is
when Lkt_Any_Of_Range =>
declare
N_Bare_Any_Of : constant Analysis.Any_Of := N_Bare_Expr.As_Any_Of;
begin
case Member is
when Member_Index_For_Any_Of_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Any_Of.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Any_Of_F_Values =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Any_Of.F_Values);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Array_Literal_Range =>
declare
N_Bare_Array_Literal : constant Analysis.Array_Literal := N_Bare_Expr.As_Array_Literal;
begin
case Member is
when Member_Index_For_Array_Literal_F_Exprs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Array_Literal.F_Exprs);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Array_Literal_F_Element_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Array_Literal.F_Element_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Base_Call_Expr =>
declare
N_Bare_Base_Call_Expr : constant Analysis.Base_Call_Expr := N_Bare_Expr.As_Base_Call_Expr;
begin
case Member is
when Member_Index_For_Base_Call_Expr_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Call_Expr.F_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Base_Call_Expr_F_Args =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Call_Expr.F_Args);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Bin_Op_Range =>
declare
N_Bare_Bin_Op : constant Analysis.Bin_Op := N_Bare_Expr.As_Bin_Op;
begin
case Member is
when Member_Index_For_Bin_Op_F_Left =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Bin_Op.F_Left);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Bin_Op_F_Op =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Bin_Op.F_Op);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Bin_Op_F_Right =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Bin_Op.F_Right);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Block_Expr_Range =>
declare
N_Bare_Block_Expr : constant Analysis.Block_Expr := N_Bare_Expr.As_Block_Expr;
begin
case Member is
when Member_Index_For_Block_Expr_F_Clauses =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Block_Expr.F_Clauses);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Cast_Expr_Range =>
declare
N_Bare_Cast_Expr : constant Analysis.Cast_Expr := N_Bare_Expr.As_Cast_Expr;
begin
case Member is
when Member_Index_For_Cast_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Cast_Expr.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Cast_Expr_F_Null_Cond =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Cast_Expr.F_Null_Cond);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Cast_Expr_F_Excludes_Null =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Cast_Expr.F_Excludes_Null);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Cast_Expr_F_Dest_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Cast_Expr.F_Dest_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Dot_Expr_Range =>
declare
N_Bare_Dot_Expr : constant Analysis.Dot_Expr := N_Bare_Expr.As_Dot_Expr;
begin
case Member is
when Member_Index_For_Dot_Expr_F_Prefix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Dot_Expr.F_Prefix);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Dot_Expr_F_Null_Cond =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Dot_Expr.F_Null_Cond);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Dot_Expr_F_Suffix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Dot_Expr.F_Suffix);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Error_On_Null_Range =>
declare
N_Bare_Error_On_Null : constant Analysis.Error_On_Null := N_Bare_Expr.As_Error_On_Null;
begin
case Member is
when Member_Index_For_Error_On_Null_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Error_On_Null.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Generic_Instantiation_Range =>
declare
N_Bare_Generic_Instantiation : constant Analysis.Generic_Instantiation := N_Bare_Expr.As_Generic_Instantiation;
begin
case Member is
when Member_Index_For_Generic_Instantiation_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Instantiation.F_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Generic_Instantiation_F_Args =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Instantiation.F_Args);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Discard_Range =>
declare
N_Bare_Grammar_Discard : constant Analysis.Grammar_Discard := N_Bare_Expr.As_Grammar_Discard;
begin
case Member is
when Member_Index_For_Grammar_Discard_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Discard.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Dont_Skip_Range =>
declare
N_Bare_Grammar_Dont_Skip : constant Analysis.Grammar_Dont_Skip := N_Bare_Expr.As_Grammar_Dont_Skip;
begin
case Member is
when Member_Index_For_Grammar_Dont_Skip_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Dont_Skip.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Grammar_Dont_Skip_F_Dont_Skip =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Dont_Skip.F_Dont_Skip);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_List_Range =>
declare
N_Bare_Grammar_List : constant Analysis.Grammar_List := N_Bare_Expr.As_Grammar_List;
begin
case Member is
when Member_Index_For_Grammar_List_F_List_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_List.F_List_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Grammar_List_F_Kind =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_List.F_Kind);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Grammar_List_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_List.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Grammar_List_F_Sep =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_List.F_Sep);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Null_Range =>
declare
N_Bare_Grammar_Null : constant Analysis.Grammar_Null := N_Bare_Expr.As_Grammar_Null;
begin
case Member is
when Member_Index_For_Grammar_Null_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Null.F_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Opt_Range =>
declare
N_Bare_Grammar_Opt : constant Analysis.Grammar_Opt := N_Bare_Expr.As_Grammar_Opt;
begin
case Member is
when Member_Index_For_Grammar_Opt_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Opt.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Opt_Error_Range =>
declare
N_Bare_Grammar_Opt_Error : constant Analysis.Grammar_Opt_Error := N_Bare_Expr.As_Grammar_Opt_Error;
begin
case Member is
when Member_Index_For_Grammar_Opt_Error_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Opt_Error.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Opt_Error_Group_Range =>
declare
N_Bare_Grammar_Opt_Error_Group : constant Analysis.Grammar_Opt_Error_Group := N_Bare_Expr.As_Grammar_Opt_Error_Group;
begin
case Member is
when Member_Index_For_Grammar_Opt_Error_Group_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Opt_Error_Group.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Opt_Group_Range =>
declare
N_Bare_Grammar_Opt_Group : constant Analysis.Grammar_Opt_Group := N_Bare_Expr.As_Grammar_Opt_Group;
begin
case Member is
when Member_Index_For_Grammar_Opt_Group_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Opt_Group.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Or_Expr_Range =>
declare
N_Bare_Grammar_Or_Expr : constant Analysis.Grammar_Or_Expr := N_Bare_Expr.As_Grammar_Or_Expr;
begin
case Member is
when Member_Index_For_Grammar_Or_Expr_F_Sub_Exprs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Or_Expr.F_Sub_Exprs);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Pick_Range =>
declare
N_Bare_Grammar_Pick : constant Analysis.Grammar_Pick := N_Bare_Expr.As_Grammar_Pick;
begin
case Member is
when Member_Index_For_Grammar_Pick_F_Exprs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Pick.F_Exprs);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Predicate_Range =>
declare
N_Bare_Grammar_Predicate : constant Analysis.Grammar_Predicate := N_Bare_Expr.As_Grammar_Predicate;
begin
case Member is
when Member_Index_For_Grammar_Predicate_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Predicate.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Grammar_Predicate_F_Prop_Ref =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Predicate.F_Prop_Ref);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Rule_Ref_Range =>
declare
N_Bare_Grammar_Rule_Ref : constant Analysis.Grammar_Rule_Ref := N_Bare_Expr.As_Grammar_Rule_Ref;
begin
case Member is
when Member_Index_For_Grammar_Rule_Ref_F_Node_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Rule_Ref.F_Node_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Skip_Range =>
declare
N_Bare_Grammar_Skip : constant Analysis.Grammar_Skip := N_Bare_Expr.As_Grammar_Skip;
begin
case Member is
when Member_Index_For_Grammar_Skip_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Skip.F_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Grammar_Stop_Cut_Range =>
declare
N_Bare_Grammar_Stop_Cut : constant Analysis.Grammar_Stop_Cut := N_Bare_Expr.As_Grammar_Stop_Cut;
begin
case Member is
when Member_Index_For_Grammar_Stop_Cut_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_Stop_Cut.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Parse_Node_Expr_Range =>
declare
N_Bare_Parse_Node_Expr : constant Analysis.Parse_Node_Expr := N_Bare_Expr.As_Parse_Node_Expr;
begin
case Member is
when Member_Index_For_Parse_Node_Expr_F_Node_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Parse_Node_Expr.F_Node_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Parse_Node_Expr_F_Sub_Exprs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Parse_Node_Expr.F_Sub_Exprs);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Token_Lit_Range =>
declare
N_Bare_Token_Lit : constant Analysis.Token_Lit := N_Bare_Expr.As_Token_Lit;
begin
case Member is
when Member_Index_For_Token_Lit_P_Denoted_Value =>
declare
R : Internal_Acc_Decoded_String_Value :=  new Internal_Rec_Decoded_String_Value;
begin
R.Value := N_Bare_Token_Lit.P_Denoted_Value;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Token_No_Case_Lit_Range =>
declare
N_Bare_Token_No_Case_Lit : constant Analysis.Token_No_Case_Lit := N_Bare_Expr.As_Token_No_Case_Lit;
begin
case Member is
when Member_Index_For_Token_No_Case_Lit_F_Lit =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Token_No_Case_Lit.F_Lit);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Token_Pattern_Concat_Range =>
declare
N_Bare_Token_Pattern_Concat : constant Analysis.Token_Pattern_Concat := N_Bare_Expr.As_Token_Pattern_Concat;
begin
case Member is
when Member_Index_For_Token_Pattern_Concat_F_Left =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Token_Pattern_Concat.F_Left);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Token_Pattern_Concat_F_Right =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Token_Pattern_Concat.F_Right);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Token_Pattern_Lit_Range =>
declare
N_Bare_Token_Pattern_Lit : constant Analysis.Token_Pattern_Lit := N_Bare_Expr.As_Token_Pattern_Lit;
begin
case Member is
when Member_Index_For_Token_Pattern_Lit_P_Denoted_Value =>
declare
R : Internal_Acc_Decoded_String_Value :=  new Internal_Rec_Decoded_String_Value;
begin
R.Value := N_Bare_Token_Pattern_Lit.P_Denoted_Value;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Token_Ref_Range =>
declare
N_Bare_Token_Ref : constant Analysis.Token_Ref := N_Bare_Expr.As_Token_Ref;
begin
case Member is
when Member_Index_For_Token_Ref_F_Token_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Token_Ref.F_Token_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Token_Ref_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Token_Ref.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Id_Range =>
declare
N_Bare_Id : constant Analysis.Id := N_Bare_Expr.As_Id;
begin
case Member is
when Member_Index_For_Id_P_Custom_Image =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Id.P_Custom_Image);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Id_Range (Kind) is
when Lkt_Def_Id_Range =>
declare
N_Bare_Def_Id : constant Analysis.Def_Id := N_Bare_Id.As_Def_Id;
begin
case Member is
when Member_Index_For_Def_Id_P_Name =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Def_Id.P_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Def_Id_P_Get_Implementatinons =>
declare
Arg_Units : Analysis.Analysis_Unit_Array renames Internal_Acc_Analysis_Unit_Array (Arguments (1)).Value.all;
begin
declare
R : Internal_Acc_Def_Id_Array :=  new Internal_Rec_Def_Id_Array;
begin
R.Value := new Analysis.Def_Id_Array'(N_Bare_Def_Id.P_Get_Implementatinons (Arg_Units));
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when Member_Index_For_Def_Id_P_Decl_Detail =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Def_Id.P_Decl_Detail);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Def_Id_P_Completion_Item_Kind =>
declare
R : Internal_Acc_Int :=  new Internal_Rec_Int;
begin
R.Value := N_Bare_Def_Id.P_Completion_Item_Kind;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Def_Id_P_Doc =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Def_Id.P_Doc);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Def_Id_P_Find_All_References =>
declare
Arg_Units : Analysis.Analysis_Unit_Array renames Internal_Acc_Analysis_Unit_Array (Arguments (1)).Value.all;
begin
declare
R : Internal_Acc_Ref_Result_Array :=  new Internal_Rec_Ref_Result_Array;
begin
R.Value := new Analysis.Ref_Result_Array'(N_Bare_Def_Id.P_Find_All_References (Arg_Units));
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when others => null;
end case;
end;
when Lkt_Ref_Id_Range =>
declare
N_Bare_Ref_Id : constant Analysis.Ref_Id := N_Bare_Id.As_Ref_Id;
begin
case Member is
when Member_Index_For_Ref_Id_P_Referenced_Defining_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ref_Id.P_Referenced_Defining_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Lkt_If_Expr_Range =>
declare
N_Bare_If_Expr : constant Analysis.If_Expr := N_Bare_Expr.As_If_Expr;
begin
case Member is
when Member_Index_For_If_Expr_F_Cond_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Expr.F_Cond_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_If_Expr_F_Then_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Expr.F_Then_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_If_Expr_F_Alternatives =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Expr.F_Alternatives);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_If_Expr_F_Else_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Expr.F_Else_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Isa_Range =>
declare
N_Bare_Isa : constant Analysis.Isa := N_Bare_Expr.As_Isa;
begin
case Member is
when Member_Index_For_Isa_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Isa.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Isa_F_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Isa.F_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Keep_Expr_Range =>
declare
N_Bare_Keep_Expr : constant Analysis.Keep_Expr := N_Bare_Expr.As_Keep_Expr;
begin
case Member is
when Member_Index_For_Keep_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Keep_Expr.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Keep_Expr_F_Null_Cond =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Keep_Expr.F_Null_Cond);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Keep_Expr_F_Keep_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Keep_Expr.F_Keep_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Lambda_Expr_Range =>
declare
N_Bare_Lambda_Expr : constant Analysis.Lambda_Expr := N_Bare_Expr.As_Lambda_Expr;
begin
case Member is
when Member_Index_For_Lambda_Expr_F_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lambda_Expr.F_Params);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lambda_Expr_F_Return_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lambda_Expr.F_Return_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lambda_Expr_F_Body =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lambda_Expr.F_Body);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Char_Lit_Range =>
declare
N_Bare_Char_Lit : constant Analysis.Char_Lit := N_Bare_Expr.As_Char_Lit;
begin
case Member is
when Member_Index_For_Char_Lit_P_Denoted_Value =>
declare
R : Internal_Acc_Decoded_Char_Value :=  new Internal_Rec_Decoded_Char_Value;
begin
R.Value := N_Bare_Char_Lit.P_Denoted_Value;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Null_Lit_Range =>
declare
N_Bare_Null_Lit : constant Analysis.Null_Lit := N_Bare_Expr.As_Null_Lit;
begin
case Member is
when Member_Index_For_Null_Lit_F_Dest_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Null_Lit.F_Dest_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_String_Lit =>
declare
N_Bare_String_Lit : constant Analysis.String_Lit := N_Bare_Expr.As_String_Lit;
begin
case Member is
when Member_Index_For_String_Lit_P_Denoted_Value =>
declare
R : Internal_Acc_Decoded_String_Value :=  new Internal_Rec_Decoded_String_Value;
begin
R.Value := N_Bare_String_Lit.P_Denoted_Value;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_String_Lit_P_Is_Prefixed_String =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_String_Lit.P_Is_Prefixed_String;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_String_Lit_P_Prefix =>
declare
R : Internal_Acc_Char :=  new Internal_Rec_Char;
begin
R.Value := N_Bare_String_Lit.P_Prefix;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_String_Lit_P_Is_Regexp_Literal =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_String_Lit.P_Is_Regexp_Literal;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_String_Lit (Kind) is
when Lkt_Block_String_Lit_Range =>
declare
N_Bare_Block_String_Lit : constant Analysis.Block_String_Lit := N_Bare_String_Lit.As_Block_String_Lit;
begin
case Member is
when Member_Index_For_Block_String_Lit_F_Lines =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Block_String_Lit.F_Lines);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Lkt_Logic_Assign_Range =>
declare
N_Bare_Logic_Assign : constant Analysis.Logic_Assign := N_Bare_Expr.As_Logic_Assign;
begin
case Member is
when Member_Index_For_Logic_Assign_F_Dest_Var =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Logic_Assign.F_Dest_Var);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Logic_Assign_F_Value =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Logic_Assign.F_Value);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Logic_Expr_Range =>
declare
N_Bare_Logic_Expr : constant Analysis.Logic_Expr := N_Bare_Expr.As_Logic_Expr;
begin
case Member is
when Member_Index_For_Logic_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Logic_Expr.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Logic_Propagate_Range =>
declare
N_Bare_Logic_Propagate : constant Analysis.Logic_Propagate := N_Bare_Expr.As_Logic_Propagate;
begin
case Member is
when Member_Index_For_Logic_Propagate_F_Dest_Var =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Logic_Propagate.F_Dest_Var);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Logic_Propagate_F_Call =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Logic_Propagate.F_Call);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Logic_Unify_Range =>
declare
N_Bare_Logic_Unify : constant Analysis.Logic_Unify := N_Bare_Expr.As_Logic_Unify;
begin
case Member is
when Member_Index_For_Logic_Unify_F_Lhs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Logic_Unify.F_Lhs);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Logic_Unify_F_Rhs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Logic_Unify.F_Rhs);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Match_Expr_Range =>
declare
N_Bare_Match_Expr : constant Analysis.Match_Expr := N_Bare_Expr.As_Match_Expr;
begin
case Member is
when Member_Index_For_Match_Expr_F_Match_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Match_Expr.F_Match_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Match_Expr_F_Branches =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Match_Expr.F_Branches);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Not_Expr_Range =>
declare
N_Bare_Not_Expr : constant Analysis.Not_Expr := N_Bare_Expr.As_Not_Expr;
begin
case Member is
when Member_Index_For_Not_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Not_Expr.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Paren_Expr_Range =>
declare
N_Bare_Paren_Expr : constant Analysis.Paren_Expr := N_Bare_Expr.As_Paren_Expr;
begin
case Member is
when Member_Index_For_Paren_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Paren_Expr.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Raise_Expr_Range =>
declare
N_Bare_Raise_Expr : constant Analysis.Raise_Expr := N_Bare_Expr.As_Raise_Expr;
begin
case Member is
when Member_Index_For_Raise_Expr_F_Dest_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Raise_Expr.F_Dest_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Raise_Expr_F_Except_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Raise_Expr.F_Except_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Subscript_Expr_Range =>
declare
N_Bare_Subscript_Expr : constant Analysis.Subscript_Expr := N_Bare_Expr.As_Subscript_Expr;
begin
case Member is
when Member_Index_For_Subscript_Expr_F_Prefix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subscript_Expr.F_Prefix);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Subscript_Expr_F_Null_Cond =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subscript_Expr.F_Null_Cond);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Subscript_Expr_F_Index =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subscript_Expr.F_Index);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Try_Expr_Range =>
declare
N_Bare_Try_Expr : constant Analysis.Try_Expr := N_Bare_Expr.As_Try_Expr;
begin
case Member is
when Member_Index_For_Try_Expr_F_Try_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Try_Expr.F_Try_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Try_Expr_F_Or_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Try_Expr.F_Or_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Un_Op_Range =>
declare
N_Bare_Un_Op : constant Analysis.Un_Op := N_Bare_Expr.As_Un_Op;
begin
case Member is
when Member_Index_For_Un_Op_F_Op =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Un_Op.F_Op);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Un_Op_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Un_Op.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Lkt_Full_Decl_Range =>
declare
N_Bare_Full_Decl : constant Analysis.Full_Decl := N.As_Full_Decl;
begin
case Member is
when Member_Index_For_Full_Decl_F_Doc =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Full_Decl.F_Doc);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Full_Decl_F_Decl_Annotations =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Full_Decl.F_Decl_Annotations);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Full_Decl_F_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Full_Decl.F_Decl);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Full_Decl_P_Has_Annotation =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Full_Decl.P_Has_Annotation (Arg_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
end;
when others => null;
end case;
end;
when Lkt_Grammar_List_Sep_Range =>
declare
N_Bare_Grammar_List_Sep : constant Analysis.Grammar_List_Sep := N.As_Grammar_List_Sep;
begin
case Member is
when Member_Index_For_Grammar_List_Sep_F_Token =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_List_Sep.F_Token);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Grammar_List_Sep_F_Extra =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Grammar_List_Sep.F_Extra);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Import_Range =>
declare
N_Bare_Import : constant Analysis.Import := N.As_Import;
begin
case Member is
when Member_Index_For_Import_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Import.F_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Import_P_Referenced_Unit =>
declare
R : Internal_Acc_Analysis_Unit :=  new Internal_Rec_Analysis_Unit;
begin
Set_Unit (R, N_Bare_Import.P_Referenced_Unit);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Langkit_Root_Range =>
declare
N_Bare_Langkit_Root : constant Analysis.Langkit_Root := N.As_Langkit_Root;
begin
case Member is
when Member_Index_For_Langkit_Root_F_Imports =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Langkit_Root.F_Imports);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Langkit_Root_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Langkit_Root.F_Decls);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Langkit_Root_P_Fetch_Prelude =>
declare
R : Internal_Acc_Analysis_Unit :=  new Internal_Rec_Analysis_Unit;
begin
Set_Unit (R, N_Bare_Langkit_Root.P_Fetch_Prelude);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Lexer_Case_Rule_Range =>
declare
N_Bare_Lexer_Case_Rule : constant Analysis.Lexer_Case_Rule := N.As_Lexer_Case_Rule;
begin
case Member is
when Member_Index_For_Lexer_Case_Rule_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lexer_Case_Rule.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lexer_Case_Rule_F_Alts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lexer_Case_Rule.F_Alts);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Lexer_Case_Rule_Send_Range =>
declare
N_Bare_Lexer_Case_Rule_Send : constant Analysis.Lexer_Case_Rule_Send := N.As_Lexer_Case_Rule_Send;
begin
case Member is
when Member_Index_For_Lexer_Case_Rule_Send_F_Sent =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lexer_Case_Rule_Send.F_Sent);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Lexer_Case_Rule_Send_F_Match_Size =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Lexer_Case_Rule_Send.F_Match_Size);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Null_Cond_Qualifier =>
declare
N_Bare_Null_Cond_Qualifier : constant Analysis.Null_Cond_Qualifier := N.As_Null_Cond_Qualifier;
begin
case Member is
when Member_Index_For_Null_Cond_Qualifier_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Null_Cond_Qualifier.P_As_Bool;
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Binding_Pattern_Range =>
declare
N_Bare_Binding_Pattern : constant Analysis.Binding_Pattern := N.As_Binding_Pattern;
begin
case Member is
when Member_Index_For_Binding_Pattern_F_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Binding_Pattern.F_Decl);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Binding_Pattern_F_Sub_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Binding_Pattern.F_Sub_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Ellipsis_Pattern_Range =>
declare
N_Bare_Ellipsis_Pattern : constant Analysis.Ellipsis_Pattern := N.As_Ellipsis_Pattern;
begin
case Member is
when Member_Index_For_Ellipsis_Pattern_F_Binding =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ellipsis_Pattern.F_Binding);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Extended_Pattern_Range =>
declare
N_Bare_Extended_Pattern : constant Analysis.Extended_Pattern := N.As_Extended_Pattern;
begin
case Member is
when Member_Index_For_Extended_Pattern_F_Sub_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Extended_Pattern.F_Sub_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Extended_Pattern_F_Details =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Extended_Pattern.F_Details);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Filtered_Pattern_Range =>
declare
N_Bare_Filtered_Pattern : constant Analysis.Filtered_Pattern := N.As_Filtered_Pattern;
begin
case Member is
when Member_Index_For_Filtered_Pattern_F_Sub_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Filtered_Pattern.F_Sub_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Filtered_Pattern_F_Predicate =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Filtered_Pattern.F_Predicate);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_List_Pattern_Range =>
declare
N_Bare_List_Pattern : constant Analysis.List_Pattern := N.As_List_Pattern;
begin
case Member is
when Member_Index_For_List_Pattern_F_Sub_Patterns =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_List_Pattern.F_Sub_Patterns);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Not_Pattern_Range =>
declare
N_Bare_Not_Pattern : constant Analysis.Not_Pattern := N.As_Not_Pattern;
begin
case Member is
when Member_Index_For_Not_Pattern_F_Sub_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Not_Pattern.F_Sub_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Or_Pattern_Range =>
declare
N_Bare_Or_Pattern : constant Analysis.Or_Pattern := N.As_Or_Pattern;
begin
case Member is
when Member_Index_For_Or_Pattern_F_Left_Sub_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Or_Pattern.F_Left_Sub_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Or_Pattern_F_Right_Sub_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Or_Pattern.F_Right_Sub_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Paren_Pattern_Range =>
declare
N_Bare_Paren_Pattern : constant Analysis.Paren_Pattern := N.As_Paren_Pattern;
begin
case Member is
when Member_Index_For_Paren_Pattern_F_Sub_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Paren_Pattern.F_Sub_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Tuple_Pattern_Range =>
declare
N_Bare_Tuple_Pattern : constant Analysis.Tuple_Pattern := N.As_Tuple_Pattern;
begin
case Member is
when Member_Index_For_Tuple_Pattern_F_Sub_Patterns =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Tuple_Pattern.F_Sub_Patterns);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Type_Pattern_Range =>
declare
N_Bare_Type_Pattern : constant Analysis.Type_Pattern := N.As_Type_Pattern;
begin
case Member is
when Member_Index_For_Type_Pattern_F_Type_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Pattern.F_Type_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Field_Pattern_Detail_Range =>
declare
N_Bare_Field_Pattern_Detail : constant Analysis.Field_Pattern_Detail := N.As_Field_Pattern_Detail;
begin
case Member is
when Member_Index_For_Field_Pattern_Detail_F_Id =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Field_Pattern_Detail.F_Id);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Field_Pattern_Detail_F_Expected_Value =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Field_Pattern_Detail.F_Expected_Value);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Property_Pattern_Detail_Range =>
declare
N_Bare_Property_Pattern_Detail : constant Analysis.Property_Pattern_Detail := N.As_Property_Pattern_Detail;
begin
case Member is
when Member_Index_For_Property_Pattern_Detail_F_Call =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Property_Pattern_Detail.F_Call);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Property_Pattern_Detail_F_Expected_Value =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Property_Pattern_Detail.F_Expected_Value);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Selector_Pattern_Detail_Range =>
declare
N_Bare_Selector_Pattern_Detail : constant Analysis.Selector_Pattern_Detail := N.As_Selector_Pattern_Detail;
begin
case Member is
when Member_Index_For_Selector_Pattern_Detail_F_Call =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Selector_Pattern_Detail.F_Call);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Selector_Pattern_Detail_F_Sub_Pattern =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Selector_Pattern_Detail.F_Sub_Pattern);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Selector_Call_Range =>
declare
N_Bare_Selector_Call : constant Analysis.Selector_Call := N.As_Selector_Call;
begin
case Member is
when Member_Index_For_Selector_Call_F_Quantifier =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Selector_Call.F_Quantifier);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Selector_Call_F_Binding =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Selector_Call.F_Binding);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Selector_Call_F_Selector_Call =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Selector_Call.F_Selector_Call);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Type_Ref =>
declare
N_Bare_Type_Ref : constant Analysis.Type_Ref := N.As_Type_Ref;
begin
case Member is
when Member_Index_For_Type_Ref_P_Referenced_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Ref.P_Referenced_Decl);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
case Lkt_Type_Ref (Kind) is
when Lkt_Function_Type_Ref_Range =>
declare
N_Bare_Function_Type_Ref : constant Analysis.Function_Type_Ref := N_Bare_Type_Ref.As_Function_Type_Ref;
begin
case Member is
when Member_Index_For_Function_Type_Ref_F_Param_Types =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Function_Type_Ref.F_Param_Types);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Function_Type_Ref_F_Return_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Function_Type_Ref.F_Return_Type);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Generic_Type_Ref_Range =>
declare
N_Bare_Generic_Type_Ref : constant Analysis.Generic_Type_Ref := N_Bare_Type_Ref.As_Generic_Type_Ref;
begin
case Member is
when Member_Index_For_Generic_Type_Ref_F_Type_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Type_Ref.F_Type_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Generic_Type_Ref_F_Args =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Type_Ref.F_Args);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when Lkt_Simple_Type_Ref_Range =>
declare
N_Bare_Simple_Type_Ref : constant Analysis.Simple_Type_Ref := N_Bare_Type_Ref.As_Simple_Type_Ref;
begin
case Member is
when Member_Index_For_Simple_Type_Ref_F_Type_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Simple_Type_Ref.F_Type_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Lkt_Var_Bind_Range =>
declare
N_Bare_Var_Bind : constant Analysis.Var_Bind := N.As_Var_Bind;
begin
case Member is
when Member_Index_For_Var_Bind_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Var_Bind.F_Name);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when Member_Index_For_Var_Bind_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Var_Bind.F_Expr);
Result := Internal_Value_Access (R);
exception
when Exc : others =>
if Implementation.Properties_May_Raise (Exc) then
Result := Internal_Value_Access (R);
Result.Destroy;
Free (Result);
end if;
raise;
end;
when others => null;
end case;
end;
when others => null;
end case;
      pragma Assert (Result /= null);
      return Result;
   end Eval_Node_Member;

   --------------
   -- Set_Unit --
   --------------

   procedure Set_Unit
     (Intr_Value   : Internal_Acc_Analysis_Unit;
      Actual_Value : Analysis_Unit)
   is
      U : constant Internal_Unit :=
        +Public_Converters.Unwrap_Unit (Actual_Value);
   begin
      Intr_Value.Value :=
        Liblktlang_Support.Internal.Conversions.Wrap_Unit (Self_Id, U);
   end Set_Unit;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Intr_Value : Internal_Rec_Analysis_Unit)
      return Analysis_Unit
   is
      U : constant Implementation.Internal_Unit :=
        +Liblktlang_Support.Internal.Conversions.Unwrap_Unit (Intr_Value.Value);
   begin
      return Public_Converters.Wrap_Unit (U);
   end Get_Unit;

   -----------------
   -- Set_Big_Int --
   -----------------

   procedure Set_Big_Int
     (Intr_Value   : Internal_Acc_Big_Int;
      Actual_Value : Big_Integer) is
   begin
      Intr_Value.Value.Set (Actual_Value);
   end Set_Big_Int;

   -----------------
   -- Get_Big_Int --
   -----------------

   procedure Get_Big_Int
     (Intr_Value   : Internal_Rec_Big_Int;
      Actual_Value : out Big_Integer)
   is
   begin
      Actual_Value.Set (Intr_Value.Value);
   end Get_Big_Int;

   --------------
   -- Set_Node --
   --------------

   procedure Set_Node
     (Intr_Value   : Internal_Acc_Node;
      Actual_Value : Lkt_Node'Class)
   is
      E : constant Internal_Entity := +Unwrap_Entity (Actual_Value);
   begin
      Intr_Value.Value :=
        Liblktlang_Support.Internal.Conversions.Wrap_Node (Self_Id, E);
   end Set_Node;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Intr_Value : Internal_Rec_Node)
      return Lkt_Node
   is
      E : constant Implementation.Internal_Entity :=
        +Liblktlang_Support.Internal.Conversions.Unwrap_Node (Intr_Value.Value);
   begin
      return Public_Converters.Wrap_Node (E.Node, E.Info);
   end Get_Node;

end Liblktlang.Generic_Introspection;
