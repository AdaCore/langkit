


with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with Liblktlang_Support.Generic_API; use Liblktlang_Support.Generic_API;
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;

with Liblktlang.Generic_API; use Liblktlang.Generic_API;






package body Liblktlang.Iterators is

   package Predicate_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Lkt_Node_Predicate,
      "="          => Lkt_Node_Predicate_References."=");

   function To_Array
     (Predicates : Predicate_Vectors.Vector) return Lkt_Node_Predicate_Array;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Predicates : Predicate_Vectors.Vector) return Lkt_Node_Predicate_Array
   is
   begin
      return Result : Lkt_Node_Predicate_Array (1 .. Natural (Predicates.Length)) do
         for I in Result'Range loop
            Result (I) := Predicates.Element (I);
         end loop;
      end return;
   end To_Array;

   --------------
   -- Traverse --
   --------------

   function Traverse (Root : Lkt_Node'Class) return Traverse_Iterator'Class is
   begin
      return Result : Traverse_Iterator do
         Traversal_Iterators.Create_Tree_Iterator (Root.As_Lkt_Node, Result);
      end return;
   end Traverse;

   -----------
   -- "not" --
   -----------

   function "not" (Predicate : Lkt_Node_Predicate) return Lkt_Node_Predicate is
   begin
      return Result : Lkt_Node_Predicate do
         Result.Set
           (Not_Predicate'(Lkt_Node_Predicate_Interface with Predicate => Predicate));
      end return;
   end "not";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Lkt_Node_Predicate) return Lkt_Node_Predicate is
   begin
      return For_All ((Left, Right));
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Lkt_Node_Predicate) return Lkt_Node_Predicate is
   begin
      return For_Some ((Left, Right));
   end "or";

   -------------
   -- For_All --
   -------------

   function For_All (Predicates : Lkt_Node_Predicate_Array) return Lkt_Node_Predicate is
      Preds : Predicate_Vectors.Vector;
   begin
      --  Flatten sub-predicates that are themselves For_All predicates in
      --  Predicates.
      for P of Predicates loop
         if P.Unchecked_Get.all in For_All_Predicate'Class then
            for Sub_P of For_All_Predicate (P.Unchecked_Get.all).Predicates
            loop
               Preds.Append (Sub_P);
            end loop;
         else
            Preds.Append (P);
         end if;
      end loop;

      return Result : Lkt_Node_Predicate do
         Result.Set (For_All_Predicate'
           (Lkt_Node_Predicate_Interface with
            N          => Natural (Preds.Length),
            Predicates => To_Array (Preds)));
      end return;
   end For_All;

   --------------
   -- For_Some --
   --------------

   function For_Some (Predicates : Lkt_Node_Predicate_Array) return Lkt_Node_Predicate is
      Preds : Predicate_Vectors.Vector;
   begin
      --  Flatten sub-predicates that are themselves For_Some predicates in
      --  Predicates.
      for P of Predicates loop
         if P.Unchecked_Get.all in For_Some_Predicate'Class then
            for Sub_P of For_Some_Predicate (P.Unchecked_Get.all).Predicates
            loop
               Preds.Append (Sub_P);
            end loop;
         else
            Preds.Append (P);
         end if;
      end loop;

      return Result : Lkt_Node_Predicate do
         Result.Set (For_Some_Predicate'
           (Lkt_Node_Predicate_Interface with
            N          => Natural (Preds.Length),
            Predicates => To_Array (Preds)));
      end return;
   end For_Some;

   ----------------------
   -- For_All_Children --
   ----------------------

   function For_All_Children
     (Predicate : Lkt_Node_Predicate; Skip_Null : Boolean := True) return Lkt_Node_Predicate
   is
   begin
      return Result : Lkt_Node_Predicate do
         Result.Set (For_All_Children_Predicate'
           (Lkt_Node_Predicate_Interface with
            Predicate => Predicate,
            Skip_Null => Skip_Null));
      end return;
   end For_All_Children;

   -----------------------
   -- For_Some_Children --
   -----------------------

   function For_Some_Children
     (Predicate : Lkt_Node_Predicate; Skip_Null : Boolean := True) return Lkt_Node_Predicate
   is
   begin
      return Result : Lkt_Node_Predicate do
         Result.Set (For_Some_Children_Predicate'
           (Lkt_Node_Predicate_Interface with
            Predicate => Predicate,
            Skip_Null => Skip_Null));
      end return;
   end For_Some_Children;

   ----------------
   -- Child_With --
   ----------------

   function Child_With
     (Field     : Struct_Member_Ref;
      Predicate : Lkt_Node_Predicate) return Lkt_Node_Predicate
   is
      T : constant Liblktlang_Support.Generic_API.Introspection.Type_Ref :=
        Owner (Field);
   begin
      if Language (T) /= Self_Id then
         raise Precondition_Failure with "unexpected language";
      elsif not Is_Node_Type (T) or else Is_Property (Field) then
         raise Precondition_Failure with "node field reference expected";
      end if;

      return Result : Lkt_Node_Predicate do
         Result.Set (Child_With_Predicate'
           (Lkt_Node_Predicate_Interface with
            Field     => Field,
            Predicate => Predicate));
      end return;
   end Child_With;

   -------------
   -- Kind_Is --
   -------------

   function Kind_Is (Kind : Lkt_Node_Kind_Type) return Lkt_Node_Predicate is
   begin
      return Kind_In (Kind, Kind);
   end Kind_Is;

   -------------
   -- Kind_In --
   -------------

   function Kind_In (First, Last : Lkt_Node_Kind_Type) return Lkt_Node_Predicate is
   begin
      return Result : Lkt_Node_Predicate do
         Result.Set (Kind_Predicate'(Lkt_Node_Predicate_Interface with
                                     First => First,
                                     Last  => Last));
      end return;
   end Kind_In;

   -------------
   -- Text_Is --
   -------------

   function Text_Is (Text : Text_Type) return Lkt_Node_Predicate is
   begin
      return Result : Lkt_Node_Predicate do
         Result.Set (Text_Predicate'(Lkt_Node_Predicate_Interface
                     with Text => To_Unbounded_Text (Text)));
      end return;
   end Text_Is;

   ------------------
   -- Node_Is_Null --
   ------------------

   function Node_Is_Null return Lkt_Node_Predicate is
   begin
      return Result : Lkt_Node_Predicate do
         Result.Set (Node_Is_Null_Predicate'(Lkt_Node_Predicate_Interface with null record));
      end return;
   end Node_Is_Null;

   ----------
   -- Next --
   ----------

   function Next
     (It : in out Find_Iterator; Element : out Lkt_Node) return Boolean
   is
      Parent : Traverse_Iterator := Traverse_Iterator (It);
   begin
      while Next (Parent, Element) loop
         if It.Predicate.Unchecked_Get.Evaluate (Element) then
            return True;
         end if;
      end loop;
      return False;
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (It : in out Local_Find_Iterator; Element : out Lkt_Node) return Boolean
   is
      Parent : Traverse_Iterator := Traverse_Iterator (It);
   begin
      while Next (Parent, Element) loop
         if It.Predicate = null or else It.Predicate (Element) then
            return True;
         end if;
      end loop;
      return False;
   end Next;

   ----------
   -- Find --
   ----------

   function Find
     (Root      : Lkt_Node'Class;
      Predicate : access function (N : Lkt_Node) return Boolean := null)
     return Traverse_Iterator'Class is
   begin
      return Ret : Local_Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator (Root.As_Lkt_Node, Ret);

         --  We still want to provide this functionality, even though it is
         --  unsafe. TODO: We might be able to make a safe version of this
         --  using generics. Still would be more verbose though.
         Ret.Predicate := Predicate'Unrestricted_Access.all;
      end return;
   end Find;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (Root      : Lkt_Node'Class;
      Predicate : access function (N : Lkt_Node) return Boolean := null)
      return Lkt_Node
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : Lkt_Node;
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_Lkt_Node;
      end if;
      return Result;
   end Find_First;

   ----------
   -- Find --
   ----------

   function Find
     (Root : Lkt_Node'Class; Predicate : Lkt_Node_Predicate'Class)
      return Traverse_Iterator'Class is
   begin
      return Ret : Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator
           (Root.As_Lkt_Node, Ret);

         --  We still want to provide this functionality, even though it is
         --  unsafe. TODO: We might be able to make a safe version of this
         --  using generics. Still would be more verbose though.
         Ret.Predicate := Lkt_Node_Predicate (Predicate);
      end return;
   end Find;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (Root : Lkt_Node'Class; Predicate : Lkt_Node_Predicate'Class)
      return Lkt_Node
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : Lkt_Node;
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_Lkt_Node;
      end if;
      return Result;
   end Find_First;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (N : Lkt_Node) return Lkt_Node
   is (Parent (N));

   ------------------------------------
   -- First_Child_Index_For_Traverse --
   ------------------------------------

   function First_Child_Index_For_Traverse (N : Lkt_Node) return Natural
   is (First_Child_Index (N));

   -----------------------------------
   -- Last_Child_Index_For_Traverse --
   -----------------------------------

   function Last_Child_Index_For_Traverse (N : Lkt_Node) return Natural
   is (Last_Child_Index (N));

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (N : Lkt_Node; I : Natural) return Lkt_Node
   is (Child (N, I));

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Not_Predicate; N : Lkt_Node) return Boolean is
   begin
      return not P.Predicate.Unchecked_Get.Evaluate (N);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_All_Predicate; N : Lkt_Node) return Boolean is
   begin
      for Predicate of P.Predicates loop
         if not Predicate.Unchecked_Get.Evaluate (N) then
            return False;
         end if;
      end loop;
      return True;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_Some_Predicate; N : Lkt_Node) return Boolean is
   begin
      for Predicate of P.Predicates loop
         if Predicate.Unchecked_Get.Evaluate (N) then
            return True;
         end if;
      end loop;
      return False;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_All_Children_Predicate; N : Lkt_Node) return Boolean
   is
      Child_Pred : Lkt_Node_Predicate_Interface'Class renames P.Predicate.Unchecked_Get.all;
   begin
      for I in 1 .. N.Children_Count loop
         declare
            Child : constant Lkt_Node := N.Child (I);
         begin
            if (not P.Skip_Null or else not Child.Is_Null)
               and then not Child_Pred.Evaluate (Child)
            then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_Some_Children_Predicate; N : Lkt_Node) return Boolean
   is
      Child_Pred : Lkt_Node_Predicate_Interface'Class renames P.Predicate.Unchecked_Get.all;
   begin
      for I in 1 .. N.Children_Count loop
         declare
            Child : constant Lkt_Node := N.Child (I);
         begin
            if (not P.Skip_Null or else not Child.Is_Null)
               and then Child_Pred.Evaluate (Child)
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Child_With_Predicate; N : Lkt_Node) return Boolean
   is
      Lk_N, Field : Lk_Node;
   begin
      if N.Is_Null then
         return False;
      end if;

      --  The predicates accepts ``N`` if both ``N`` has the expected type
      --  (i.e. can own the requested syntax field) and ``N`` does have that
      --  field.

      Lk_N := To_Generic_Node (N);
      if not Is_Derived_From (Type_Of (Lk_N), Owner (P.Field)) then
         return False;
      end if;

      Field := As_Node (Eval_Node_Member (Lk_N, P.Field));
      return P.Predicate.Unchecked_Get.Evaluate (From_Generic_Node (Field));
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Kind_Predicate; N : Lkt_Node) return Boolean is
   begin
      return N.Kind in P.First .. P.Last;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Text_Predicate; N : Lkt_Node) return Boolean
   is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      return (if N.Is_Null
              then P.Text = ""
              else N.Text = P.Text);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Node_Is_Null_Predicate; N : Lkt_Node) return Boolean
   is
      pragma Unreferenced (P);
   begin
      return N.Is_Null;
   end Evaluate;

   


end Liblktlang.Iterators;
