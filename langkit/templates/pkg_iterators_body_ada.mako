## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

<%
   pred_iface = '{}_Predicate_Interface'.format(root_entity.api_name)
   pred_ref = '{}_Predicate'.format(root_entity.api_name)
%>

${exts.with_clauses(with_clauses)}

package body ${ada_lib_name}.Iterators is

   package Predicate_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => ${pred_ref},
      "="          => ${root_entity.api_name}_Predicate_References."=");

   function To_Array
     (Predicates : Predicate_Vectors.Vector) return ${pred_ref}_Array;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Predicates : Predicate_Vectors.Vector) return ${pred_ref}_Array
   is
   begin
      return Result : ${pred_ref}_Array (1 .. Natural (Predicates.Length)) do
         for I in Result'Range loop
            Result (I) := Predicates.Element (I);
         end loop;
      end return;
   end To_Array;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Root : ${root_entity.api_name}'Class) return Traverse_Iterator'Class is
   begin
      return Result : Traverse_Iterator do
         Traversal_Iterators.Create_Tree_Iterator
           (Root.As_${root_entity.api_name}, Result);
      end return;
   end Traverse;

   -----------
   -- "not" --
   -----------

   function "not" (Predicate : ${pred_ref}) return ${pred_ref} is
   begin
      return Result : ${pred_ref} do
         Result.Set
           (Not_Predicate'(${pred_iface} with Predicate => Predicate));
      end return;
   end "not";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : ${pred_ref}) return ${pred_ref} is
   begin
      return For_All ((Left, Right));
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : ${pred_ref}) return ${pred_ref} is
   begin
      return For_Some ((Left, Right));
   end "or";

   -------------
   -- For_All --
   -------------

   function For_All (Predicates : ${pred_ref}_Array) return ${pred_ref} is
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

      return Result : ${pred_ref} do
         Result.Set (For_All_Predicate'
           (${pred_iface} with
            N          => Natural (Preds.Length),
            Predicates => To_Array (Preds)));
      end return;
   end For_All;

   --------------
   -- For_Some --
   --------------

   function For_Some (Predicates : ${pred_ref}_Array) return ${pred_ref} is
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

      return Result : ${pred_ref} do
         Result.Set (For_Some_Predicate'
           (${pred_iface} with
            N          => Natural (Preds.Length),
            Predicates => To_Array (Preds)));
      end return;
   end For_Some;

   -------------
   -- Kind_Is --
   -------------

   function Kind_Is (Kind : ${root_node_kind_name}) return ${pred_ref} is
   begin
      return Result : ${pred_ref} do
         Result.Set (Kind_Predicate'(${pred_iface} with Kind => Kind));
      end return;
   end Kind_Is;

   -------------
   -- Text_Is --
   -------------

   function Text_Is (Text : Text_Type) return ${pred_ref} is
   begin
      return Result : ${pred_ref} do
         Result.Set (Text_Predicate'(${pred_iface}
                     with Text => To_Unbounded_Text (Text)));
      end return;
   end Text_Is;

   ----------
   -- Next --
   ----------

   function Next
     (It      : in out Find_Iterator;
      Element : out ${root_entity.api_name}) return Boolean
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
     (It      : in out Local_Find_Iterator;
      Element : out ${root_entity.api_name}) return Boolean
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
     (Root      : ${root_entity.api_name}'Class;
      Predicate :
        access function (N : ${root_entity.api_name}) return Boolean := null)
     return Traverse_Iterator'Class is
   begin
      return Ret : Local_Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator
           (Root.As_${root_entity.api_name}, Ret);

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
     (Root      : ${root_entity.api_name}'Class;
      Predicate :
        access function (N : ${root_entity.api_name}) return Boolean := null)
      return ${root_entity.api_name}
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : ${root_entity.api_name};
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_${root_entity.api_name};
      end if;
      return Result;
   end Find_First;

   ----------
   -- Find --
   ----------

   function Find
     (Root : ${root_entity.api_name}'Class; Predicate : ${pred_ref}'Class)
      return Traverse_Iterator'Class is
   begin
      return Ret : Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator
           (Root.As_${root_entity.api_name}, Ret);

         --  We still want to provide this functionality, even though it is
         --  unsafe. TODO: We might be able to make a safe version of this
         --  using generics. Still would be more verbose though.
         Ret.Predicate := ${pred_ref} (Predicate);
      end return;
   end Find;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (Root : ${root_entity.api_name}'Class; Predicate : ${pred_ref}'Class)
      return ${root_entity.api_name}
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : ${root_entity.api_name};
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_${root_entity.api_name};
      end if;
      return Result;
   end Find_First;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
     (N : ${root_entity.api_name}) return ${root_entity.api_name}
   is (Parent (N));

   ------------------------------------
   -- First_Child_Index_For_Traverse --
   ------------------------------------

   function First_Child_Index_For_Traverse
     (N : ${root_entity.api_name}) return Natural
   is (First_Child_Index (N));

   -----------------------------------
   -- Last_Child_Index_For_Traverse --
   -----------------------------------

   function Last_Child_Index_For_Traverse
     (N : ${root_entity.api_name}) return Natural
   is (Last_Child_Index (N));

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (N : ${root_entity.api_name}; I : Natural) return ${root_entity.api_name}
   is (Child (N, I));

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Not_Predicate;
      N : ${root_entity.api_name}) return Boolean is
   begin
      return not P.Predicate.Unchecked_Get.Evaluate (N);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_All_Predicate;
      N : ${root_entity.api_name}) return Boolean is
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
     (P : in out For_Some_Predicate;
      N : ${root_entity.api_name}) return Boolean is
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
     (P : in out Kind_Predicate; N : ${root_entity.api_name}) return Boolean is
   begin
      return Kind (N) = P.Kind;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Text_Predicate; N : ${root_entity.api_name}) return Boolean
   is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      return (if N.Is_Null
              then P.Text = ""
              else N.Text = P.Text);
   end Evaluate;

   ${exts.include_extension(ctx.ext('iterators', 'pred_bodies'))}

end ${ada_lib_name}.Iterators;
