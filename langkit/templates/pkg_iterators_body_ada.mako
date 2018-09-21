## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

<%
   pred_iface = '{}_Predicate_Interface'.format(root_entity.api_name)
   pred_ref = '{}_Predicate'.format(root_entity.api_name)
%>

with Ada.Strings.Wide_Wide_Unbounded;

${exts.with_clauses(with_clauses)}

package body ${ada_lib_name}.Iterators is

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
