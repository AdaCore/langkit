## vim: filetype=makoada

% if ctx.separate_properties:
   with ${ada_lib_name}.Analysis.Properties;
   use ${ada_lib_name}.Analysis.Properties;
% endif

package body ${ada_lib_name}.Iterators is

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Root : ${root_entity.api_name}'Class) return Traverse_Iterator is
   begin
      return Create (As_${root_entity.el_type.kwless_raw_name} (Root));
   end Traverse;

   ----------
   -- Next --
   ----------

   function Next
     (It      : in out Find_Iterator;
      Element : out ${root_entity.api_name}) return Boolean is
   begin
      while Next (It.Traverse_It, Element) loop
         if It.Predicate.Evaluate (Element) then
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
   begin
      while Next (It.Traverse_It, Element) loop
         if It.Predicate (Element) then
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
      Predicate : access function (N : ${root_entity.api_name}) return Boolean)
     return Local_Find_Iterator
   is
      Dummy  : ${root_entity.api_name};
      Ignore : Boolean;
   begin
      return Ret : Local_Find_Iterator := Local_Find_Iterator'
        (Ada.Finalization.Limited_Controlled with
         Traverse_It => Traverse (Root),

         --  We still want to provide this functionality, even though it is
         --  unsafe. TODO: We might be able to make a safe version of this
         --  using generics. Still would be more verbose though.
         Predicate   => Predicate'Unrestricted_Access.all)
      do
         Ignore := Next (Ret.Traverse_It, Dummy);
      end return;
   end Find;

   ----------
   -- Find --
   ----------

   function Find
     (Root      : ${root_entity.api_name}'Class;
      Predicate : ${root_entity.api_name}_Predicate) return Find_Iterator
   is
      Dummy  : ${root_entity.api_name};
      Ignore : Boolean;
   begin
      return Ret : Find_Iterator :=
        (Ada.Finalization.Limited_Controlled with
         Traverse_It => Traverse (Root),
         Predicate   => Predicate)
      do
         Ignore := Next (Ret.Traverse_It, Dummy);
      end return;
   end Find;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (Root      : ${root_entity.api_name}'Class;
      Predicate : ${root_entity.api_name}_Predicate)
      return ${root_entity.api_name}
   is
      I      : Find_Iterator := Find (Root, Predicate);
      Result : ${root_entity.api_name};
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_${root_entity.api_name};
      end if;
      return Result;
   end Find_First;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (P : access ${root_entity.api_name}_Kind_Filter;
      N : ${root_entity.api_name}) return Boolean is
   begin
      return Kind (N) = P.Kind;
   end Evaluate;

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
   -- Finalize --
   --------------

   overriding procedure Finalize (It : in out Find_Iterator) is
   begin
      Destroy (It.Predicate);
   end Finalize;

end ${ada_lib_name}.Iterators;
