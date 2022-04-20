## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

<%
   node = root_entity.api_name
   pred_iface = '{}_Predicate_Interface'.format(node)
   pred_ref = '{}_Predicate'.format(node)
%>

% if ctx.sorted_parse_fields:
with ${ada_lib_name}.Introspection; use ${ada_lib_name}.Introspection;
% endif

${exts.with_clauses(with_clauses)}

package body ${ada_lib_name}.Iterators is

   package Predicate_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => ${pred_ref},
      "="          => ${node}_Predicate_References."=");

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

   function Traverse (Root : ${node}'Class) return Traverse_Iterator'Class is
   begin
      return Result : Traverse_Iterator do
         Traversal_Iterators.Create_Tree_Iterator (Root.As_${node}, Result);
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

   ----------------------
   -- For_All_Children --
   ----------------------

   function For_All_Children
     (Predicate : ${pred_ref}; Skip_Null : Boolean := True) return ${pred_ref}
   is
   begin
      return Result : ${pred_ref} do
         Result.Set (For_All_Children_Predicate'
           (${pred_iface} with
            Predicate => Predicate,
            Skip_Null => Skip_Null));
      end return;
   end For_All_Children;

   -----------------------
   -- For_Some_Children --
   -----------------------

   function For_Some_Children
     (Predicate : ${pred_ref}; Skip_Null : Boolean := True) return ${pred_ref}
   is
   begin
      return Result : ${pred_ref} do
         Result.Set (For_Some_Children_Predicate'
           (${pred_iface} with
            Predicate => Predicate,
            Skip_Null => Skip_Null));
      end return;
   end For_Some_Children;

   ----------------
   -- Child_With --
   ----------------

   function Child_With
     (Field     : Syntax_Field_Reference;
      Predicate : ${pred_ref}) return ${pred_ref} is
   begin
      % if ctx.sorted_parse_fields:
         return Result : ${pred_ref} do
            Result.Set (Child_With_Predicate'
              (${pred_iface} with
               Field     => Field,
               Predicate => Predicate));
         end return;
      % else:
         pragma Unreferenced (Field, Predicate);
         return (raise Program_Error);
      % endif
   end Child_With;

   -------------
   -- Kind_Is --
   -------------

   function Kind_Is (Kind : ${T.node_kind}) return ${pred_ref} is
   begin
      return Kind_In (Kind, Kind);
   end Kind_Is;

   -------------
   -- Kind_In --
   -------------

   function Kind_In (First, Last : ${T.node_kind}) return ${pred_ref} is
   begin
      return Result : ${pred_ref} do
         Result.Set (Kind_Predicate'(${pred_iface} with
                                     First => First,
                                     Last  => Last));
      end return;
   end Kind_In;

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

   ------------------
   -- Node_Is_Null --
   ------------------

   function Node_Is_Null return ${pred_ref} is
   begin
      return Result : ${pred_ref} do
         Result.Set (Node_Is_Null_Predicate'(${pred_iface} with null record));
      end return;
   end Node_Is_Null;

   ----------
   -- Next --
   ----------

   function Next
     (It : in out Find_Iterator; Element : out ${node}) return Boolean
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
     (It : in out Local_Find_Iterator; Element : out ${node}) return Boolean
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
     (Root      : ${node}'Class;
      Predicate : access function (N : ${node}) return Boolean := null)
     return Traverse_Iterator'Class is
   begin
      return Ret : Local_Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator (Root.As_${node}, Ret);

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
     (Root      : ${node}'Class;
      Predicate : access function (N : ${node}) return Boolean := null)
      return ${node}
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : ${node};
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_${node};
      end if;
      return Result;
   end Find_First;

   ----------
   -- Find --
   ----------

   function Find
     (Root : ${node}'Class; Predicate : ${pred_ref}'Class)
      return Traverse_Iterator'Class is
   begin
      return Ret : Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator
           (Root.As_${node}, Ret);

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
     (Root : ${node}'Class; Predicate : ${pred_ref}'Class)
      return ${root_entity.api_name}
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : ${node};
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_${node};
      end if;
      return Result;
   end Find_First;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (N : ${node}) return ${root_entity.api_name}
   is (Parent (N));

   ------------------------------------
   -- First_Child_Index_For_Traverse --
   ------------------------------------

   function First_Child_Index_For_Traverse (N : ${node}) return Natural
   is (First_Child_Index (N));

   -----------------------------------
   -- Last_Child_Index_For_Traverse --
   -----------------------------------

   function Last_Child_Index_For_Traverse (N : ${node}) return Natural
   is (Last_Child_Index (N));

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (N : ${node}; I : Natural) return ${node}
   is (Child (N, I));

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Not_Predicate; N : ${node}) return Boolean is
   begin
      return not P.Predicate.Unchecked_Get.Evaluate (N);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_All_Predicate; N : ${node}) return Boolean is
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
     (P : in out For_Some_Predicate; N : ${node}) return Boolean is
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
     (P : in out For_All_Children_Predicate; N : ${node}) return Boolean
   is
      Child_Pred : ${pred_iface}'Class renames P.Predicate.Unchecked_Get.all;
   begin
      for I in 1 .. N.Children_Count loop
         declare
            Child : constant ${node} := N.Child (I);
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
     (P : in out For_Some_Children_Predicate; N : ${node}) return Boolean
   is
      Child_Pred : ${pred_iface}'Class renames P.Predicate.Unchecked_Get.all;
   begin
      for I in 1 .. N.Children_Count loop
         declare
            Child : constant ${node} := N.Child (I);
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
     (P : in out Child_With_Predicate; N : ${node}) return Boolean is
   begin
      if N.Is_Null then
         return False;
      end if;

      % if ctx.sorted_parse_fields:
         declare
            Field_Index : Positive;
         begin
            --  First check that N has the requested field
            begin
               Field_Index := Index (N.Kind, P.Field);
            exception
               when Bad_Type_Error =>
                  return False;
            end;

            return P.Predicate.Unchecked_Get.Evaluate (N.Child (Field_Index));
         end;
      % else:
         return False;
         pragma Unreferenced (P);
      % endif
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Kind_Predicate; N : ${node}) return Boolean is
   begin
      return N.Kind in P.First .. P.Last;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Text_Predicate; N : ${node}) return Boolean
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
     (P : in out Node_Is_Null_Predicate; N : ${node}) return Boolean
   is
      pragma Unreferenced (P);
   begin
      return N.Is_Null;
   end Evaluate;

   ${exts.include_extension(ctx.ext('iterators', 'pred_bodies'))}

end ${ada_lib_name}.Iterators;
