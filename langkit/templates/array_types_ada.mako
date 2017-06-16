## vim: filetype=makoada

<%def name="public_incomplete_decl(cls)">
   type ${cls.pointed};
   type ${cls.name} is access all ${cls.pointed};
</%def>

<%def name="public_decl(cls)">

   <% elt_type = cls.element_type.name %>

   type ${cls.api_name}
     is array (Positive range <>) of ${cls.element_type.name};
   type ${cls.pointed} (N : Natural) is record
      Ref_Count : Positive;
      Items     : ${cls.api_name} (1 .. N);
   end record;

   ## If we are on the entity type, we need a conversion function
   ## to be able to get element arrays starting from 0 and convert them into
   ## arrays starting from 1. We need it to convert from entity arrays,
   ## to our array record type.
   % if cls.element_type == T.root_node.entity():
   function Create (Items : AST_Envs.Entity_Array) return ${cls.name};
   % endif

   function Create (Items_Count : Natural) return ${cls.name};
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

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

   ## Helper for properties code
   function Length (T : ${cls.name}) return Natural;

   procedure Inc_Ref (T : ${cls.name});
   procedure Dec_Ref (T : in out ${cls.name});

</%def>

<%def name="private_decl(cls)">
  procedure Free is new Ada.Unchecked_Deallocation
    (${cls.pointed}, ${cls.name});
</%def>

<%def name="body(cls)">

   <% elt_type = cls.element_type.name %>

   % if cls.element_type != ctx.root_grammar_class:
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
         % if cls.element_type.is_refcounted():
            Inc_Ref (Result);
         % endif
         return Result;
      elsif Or_Null then
         return ${cls.element_type.nullexpr()};
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : ${cls.name}) return ${cls.name} is
      use ${cls.pkg_vector};
      Ret : ${cls.name} := Create (Length (L) + Length (R));
   begin
      Ret.Items := (L.Items & R.Items);
      return Ret;
   end Concat;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : ${cls.name}) is
   begin
      T.Ref_Count := T.Ref_Count + 1;
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
      if T = null then
         return;
      end if;

      if T.Ref_Count = 1 then
         % if cls.element_type.is_refcounted():
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

   ------------
   -- Create --
   ------------

   function Create (Items_Count : Natural) return ${cls.name} is
     (new ${cls.pointed}'(N => Items_Count, Ref_Count => 1, Items => <>));

   % if cls.element_type == T.root_node.entity():
   function Create (Items : AST_Envs.Entity_Array) return ${cls.name}
   is (new ${cls.pointed}'(N         => Items'Length,
                           Items     => ${cls.api_name} (Items),
                           Ref_Count => 1));
   % endif
</%def>
