## vim: filetype=makoada

<%def name="public_api_decl(cls)">
   ## We expose arrays of characters as Langkit_Support.Text.Text_Type, so we
   ## should not declare another type for them.
   % if not cls.element_type.is_character_type:
      type ${cls.api_name} is
         array (Positive range <>) of ${cls.element_type.api_name};
   % endif
</%def>

<%def name="public_api_private_decl(cls)">
   % if cls.used_in_public_struct:
      type ${cls.api_access_name} is access all ${cls.api_name};
      procedure Free is new Ada.Unchecked_Deallocation
        (${cls.api_name}, ${cls.api_access_name});
   % endif
</%def>

<%def name="incomplete_decl(cls)">
   type ${cls.pointed};
   type ${cls.name} is access all ${cls.pointed};
</%def>

<%def name="ada_api_converters_decl(cls)">
   % if cls.to_public_converter_required:
      function ${cls.to_public_converter}
         (Value : ${cls.name}) return ${cls.api_name};
   % endif

   % if cls.to_internal_converter_required:
      function ${cls.to_internal_converter}
         (Value : ${cls.api_name}
          % if cls.conversion_requires_context:
            Context : Internal_Context
          % endif
          ) return ${cls.name};
   % endif
</%def>

<%def name="ada_api_converters_body(cls)">
   % if cls.to_public_converter_required:
      function ${cls.to_public_converter}
         (Value : ${cls.name}) return ${cls.api_name} is
      begin
         return Result : ${cls.api_name} (1 .. Value.N) do
            for I in Result'Range loop
               <% to_public_expr = cls.element_type.to_public_expr(
                     'Value.Items (I)') %>
               Result (I - Value.Items'First + Result'First)
                  % if cls.element_type.is_big_int_type:
                     .Set (${to_public_expr})
                  % else:
                     := ${to_public_expr}
                  % endif
               ;
            end loop;
         end return;
      end;
   % endif

   % if cls.to_internal_converter_required:
      function ${cls.to_internal_converter}
         (Value : ${cls.api_name}
          % if cls.conversion_requires_context:
            Context : Internal_Context
          % endif
          ) return ${cls.name}
      is
         Result : constant ${cls.name} :=
            ${cls.constructor_name} (Value'Length);
      begin
         for I in Value'Range loop
            Result.Items (I - Value'First + Result.Items'First) :=
               ${cls.element_type.to_internal_expr('Value (I)', 'Context')};
         end loop;
         return Result;
      end;
   % endif
</%def>

<%def name="decl(cls)">

   <% elt_type = cls.element_type.name %>

   % if not cls.element_type.is_character_type:
   type ${cls.array_type_name} is
      array (Positive range <>) of ${cls.element_type.name};
   % endif

   type ${cls.pointed} (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : ${cls.array_type_name} (1 .. N);
   end record;

   Empty_${cls.api_name}_Record : aliased ${cls.pointed} :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   ${cls.null_constant} : constant ${cls.name} :=
      Empty_${cls.api_name}_Record'Access;

   ## If we are on the entity type, we need a conversion function
   ## to be able to get element arrays starting from 0 and convert them into
   ## arrays starting from 1. We need it to convert from entity arrays,
   ## to our array record type.
   % if cls.element_type == T.root_node.entity:
   function ${cls.constructor_name}
     (Items : AST_Envs.Entity_Array) return ${cls.name};
   % endif

   function ${cls.constructor_name} (Items_Count : Natural) return ${cls.name};
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function ${cls.constructor_name}
     (Items : ${cls.array_type_name}) return ${cls.name};
   --  Create a new array from an existing collection of elements

   ## For arrays of characters, also emit an unbounded string-based constructor
   % if cls.element_type.is_character_type:
      function ${cls.constructor_name}
        (Items : Unbounded_Text_Type) return ${cls.name};
      --  Create a new character array from an unbounded string. This is not
      --  just for convenience: this function avoids using the secondary stack
      --  to load the string itself, avoiding stack overflows for big strings.
   % endif

   ## Helper getter generated for properties code. Used in CollectionGet's code
   function Get
     (T       : ${cls.name};
      Index   : Integer;
      Or_Null : Boolean := False) return ${elt_type};
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   ## Helper getter generated for properties code. Used in CollectionGet's code
   function Concat (L, R : ${cls.name}) return ${cls.name};

   % if cls.element_type.is_string_type:
      function Join_Strings
        (Separator : ${T.String.name};
         Strings   : ${cls.name}) return ${T.String.name};
      --  Return the concatenation of all strings in ``Strings``, separated by
      --  ``Separator``.
   % endif

   ## Helper for properties code
   function Length (T : ${cls.name}) return Natural;

   procedure Inc_Ref (T : ${cls.name});
   procedure Dec_Ref (T : in out ${cls.name});

   function Equivalent (L, R : ${cls.name}) return Boolean;

   % if cls.requires_to_iterator_property:
      function To_Iterator
        (Self    : ${cls.name};
         Context : Internal_Context) return ${cls.element_type.iterator.name};
      --  Return an iterator on values from Self. Context is used to create the
      --  corresponding safety net.
   % endif

   % if ctx.properties_logging:
      function Trace_Image (A : ${cls.name}) return String;
   % endif

   % if cls.requires_unique_function:
      function Make_Unique (A : ${cls.name}) return ${cls.name};
      --  Return a copy of A with duplicated elements removed
   % endif

   % if cls.requires_hash_function:
      function Hash (R : ${cls.name}) return Hash_Type;
   % endif

  procedure Free is new Ada.Unchecked_Deallocation
    (${cls.pointed}, ${cls.name});
</%def>

<%def name="body(cls)">

   <% elt_type = cls.element_type.name %>

   % if cls.requires_vector and cls.element_type != ctx.root_grammar_class:
      package ${cls.pkg_vector} is new Langkit_Support.Vectors (${elt_type});
   % endif

   ---------
   -- Get --
   ---------

   function Get
     (T       : ${cls.name};
      Index   : Integer;
      Or_Null : Boolean := False) return ${elt_type}
   is
      function Absolute_Get
        (T : ${cls.name}; Index : Integer)
         return ${elt_type}
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => ${elt_type},
         Sequence_Type => ${cls.name},
         Length        => Length,
         Get           => Absolute_Get);

      Result : ${elt_type};
   begin
      if Relative_Get (T, Index, Result) then
         % if cls.element_type.is_refcounted:
            Inc_Ref (Result);
         % endif
         return Result;
      elsif Or_Null then
         return ${cls.element_type.nullexpr};
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : ${cls.name}) return ${cls.name} is
      Ret : ${cls.name} := ${cls.constructor_name} (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      % if cls.element_type.is_refcounted:
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      % endif
      return Ret;
   end Concat;

   % if cls.element_type.is_string_type:
      ------------------
      -- Join_Strings --
      ------------------

      function Join_Strings
        (Separator : ${T.String.name};
         Strings   : ${cls.name}) return ${T.String.name}
      is
         Separator_Length : constant Natural := Separator.N;
         Length           : Natural := 0;
         First            : Boolean;
      begin
         --  First, compute the length of the result: the sum of all string
         --  lengths in Strings.
         First := True;
         for S of Strings.Items loop
            if First then
               First := False;
            else
               Length := Length + Separator_Length;
            end if;
            Length := Length + S.N;
         end loop;

         return Result : constant ${T.String.name} :=
            ${T.String.constructor_name} (Length)
         do
            --  Now copy the content of all strings into the result
            declare
               Last : Natural := 0;
            begin
               First := True;
               for S of Strings.Items loop
                  if First then
                     First := False;
                  else
                     Result.Items (Last + 1 .. Last + Separator_Length) :=
                        Separator.Items;
                     Last := Last + Separator_Length;
                  end if;
                  Result.Items (Last + 1 .. Last + S.N) := S.Items;
                  Last := Last + S.N;
               end loop;
            end;
         end return;
      end Join_Strings;
   % endif

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : ${cls.name}) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : ${cls.name}) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out ${cls.name}) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         % if cls.element_type.is_refcounted:
            ## When we destroy the array, owned values will have one less
            ## owner, so decrement their reference count.
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         % endif
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function ${cls.constructor_name} (Items_Count : Natural) return ${cls.name}
   is (if Items_Count = 0
       then ${cls.nullexpr}
       else new ${cls.pointed}'(N => Items_Count, Ref_Count => 1, Items => <>));

   % if cls.element_type == T.root_node.entity:
   function ${cls.constructor_name}
     (Items : AST_Envs.Entity_Array) return ${cls.name}
   is (if Items'Length = 0
       then ${cls.nullexpr}
       else new ${cls.pointed}'
         (N         => Items'Length,
          Items     => Implementation.${cls.array_type_name} (Items),
          Ref_Count => 1));
   % endif

   function ${cls.constructor_name}
     (Items : ${cls.array_type_name}) return ${cls.name} is
   begin
      if Items'Length = 0 then
         return ${cls.nullexpr};
      end if;

      % if cls.element_type.is_refcounted:
         for El of Items loop
            Inc_Ref (El);
         end loop;
      % endif
      return new ${cls.pointed}'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   % if cls.element_type.is_character_type:
      function ${cls.constructor_name}
        (Items : Unbounded_Text_Type) return ${cls.name}
      is
         Result : constant ${cls.name} :=
            ${cls.constructor_name} (Length (Items));
         S      : Big_Wide_Wide_String_Access;
         L      : Natural;
      begin
         Get_Wide_Wide_String (Items, S, L);
         Result.Items (1 .. L) := S.all (1 .. L);
         return Result;
      end;
   % endif

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : ${cls.name}) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
            % if cls.element_type.has_equivalent_function:
               not Equivalent (L.Items (I), R.Items (I))
            % else:
               L.Items (I) /= R.Items (I)
            % endif
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;

   % if cls.requires_to_iterator_property:
      -----------------
      -- To_Iterator --
      -----------------

      function To_Iterator
        (Self    : ${cls.name};
         Context : Internal_Context) return ${cls.element_type.iterator.name}
      is
      begin
         Inc_Ref (Self);
         return new ${cls.element_type.iterator.iterator_type_name}'
           (Ref_Count  => 1,
            Safety_Net => Create_Safety_Net (Context),
            Elements   => Self,
            Index      => 1);
      end To_Iterator;
   % endif

   % if ctx.properties_logging:
      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : ${cls.name}) return String is
         % if cls.is_string_type:
            begin
               return Image (A.Items);
         % else:
               Result : Unbounded_String;
            begin
               Append (Result, "[");
               for I in A.Items'Range loop
                  if I > A.Items'First then
                     Append (Result, ", ");
                  end if;
                  Append (Result, Trace_Image (A.Items (I)));
               end loop;
               Append (Result, "]");
               return To_String (Result);
         % endif
      end Trace_Image;
   % endif

   % if cls.requires_unique_function:
      function Make_Unique (A : ${cls.name}) return ${cls.name} is
         <%
            elt_type = cls.element_type
            elt_equiv_func = ('Equivalent'
                              if elt_type.has_equivalent_function else
                              '"="')
         %>

         --  To keep the order of the result deterministic, this function works
         --  in two steps. First, determine for each item in A whether it
         --  duplicate a previous item. Only then, go through all items in A
         --  and add only non-duplicated ones to the result.

         Duplicates : array (A.Items'Range) of Boolean := (others => False);
         --  For each item in A, indicate whether the value is a duplicate of a
         --  previous one in the array.

         Unique_Elements : Natural;
         --  Number of unique elements in A
      begin
         --  Step 1: compute Duplicates
         declare
            package Sets is new Ada.Containers.Hashed_Sets
              (Element_Type        => ${elt_type.name},
               "="                 => ${elt_equiv_func},
               Equivalent_Elements => ${elt_equiv_func},
               Hash                => Hash);
            Set : Sets.Set;
         begin
            for I in A.Items'Range loop
               declare
                  Item : ${elt_type.name} renames A.Items (I);
               begin
                  if Set.Contains (Item) then
                     Duplicates (I) := True;
                  else
                     Set.Insert (Item);
                  end if;
               end;
            end loop;
            Unique_Elements := Natural (Set.Length);
         end;

         --  Step 2: create the result
         declare
            Next   : Positive := 1;
            Result : constant ${cls.name} :=
               ${cls.constructor_name} (Unique_Elements);
         begin
            for I in A.Items'Range loop
               declare
                  Item : ${elt_type.name} renames A.Items (I);
               begin
                  if not Duplicates (I) then
                     Result.Items (Next) := Item;
                     % if elt_type.is_refcounted:
                        Inc_Ref (Item);
                     % endif
                     Next := Next + 1;
                  end if;
               end;
            end loop;
            pragma Assert (Next = Result.N + 1);

            return Result;
         end;
      end Make_Unique;
   % endif

   % if cls.requires_hash_function:
      ----------
      -- Hash --
      ----------

      function Hash (R : ${cls.name}) return Hash_Type is
         Result : Hash_Type := Initial_Hash;
      begin
         for I in R.Items'Range loop
            Result := Combine (Result, Hash (R.Items (I)));
         end loop;
         return Result;
      end Hash;
   % endif

</%def>
