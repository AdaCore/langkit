## vim: filetype=makoada

<%def name="public_decl(cls)">

   <% elt_type = cls.element_type().name() %>

   type ${cls.api_name()} is array (Positive range <>) of ${elt_type};
   type ${cls.pointed()} (N : Natural) is record
      Ref_Count : Positive;
      Items     : ${cls.api_name()} (1 .. N);
   end record;

   type ${cls.name()} is access all ${cls.pointed()};

   ## If we are on the env element type, we need a conversion function
   ## to be able to get element arrays starting from 0 and convert them into
   ## arrays starting from 1. We need it to convert from env element arrays,
   ## to our array record type.
   % if cls.element_type() == ctx.env_element:
   function Copy is new AST_Envs.Env_Element_Arrays.Copy
     (Positive, ${cls.api_name()});

   function Create (Items : AST_Envs.Env_Element_Array) return ${cls.name()}
   is (new ${cls.pointed()}'(N         => Items'Length,
                             Items     => Copy (Items),
                             Ref_Count => 1));
   % endif

   package ${cls.pkg_vector()} is new Langkit_Support.Vectors
     (${elt_type});

   function Create (N : Natural) return ${cls.name()} is
     (new ${cls.pointed()}'(N => N, Ref_Count => 1, Items => <>));
   --  Create a new array for N uninitialized elements and give its only
   --  ownership to the caller.

   ## Helper getter generated for properties code. Used in CollectionGet's code
   function Get
     (T       : ${cls.name()};
      Index   : Integer;
      Or_Null : Boolean := False) return ${elt_type};
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   ## Helper for properties code
   function Length (T : ${cls.name()}) return Natural is (T.N);

   procedure Inc_Ref (T : ${cls.name()});
   procedure Dec_Ref (T : in out ${cls.name()});

</%def>

<%def name="private_decl(cls)">
  procedure Free is new Ada.Unchecked_Deallocation
    (${cls.pointed()}, ${cls.name()});
</%def>

<%def name="body(cls)">

   <% elt_type = cls.element_type().name() %>

   ---------
   -- Get --
   ---------

   function Get
     (T       : ${cls.name()};
      Index   : Integer;
      Or_Null : Boolean := False) return ${elt_type}
   is
      function Absolute_Get
        (T : ${cls.name()}; Index : Integer)
         return ${elt_type}
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => ${elt_type},
         Sequence_Type => ${cls.name()},
         Length        => Length,
         Get           => Absolute_Get);

      Result : ${elt_type};
   begin
      if Relative_Get (T, Index, Result) then
         % if cls.element_type().is_refcounted():
            Inc_Ref (Result);
         % endif
         return Result;
      elsif Or_Null then
         return ${cls.element_type().nullexpr()};
      else
         raise Property_Error;
      end if;
   end Get;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : ${cls.name()}) is
   begin
      ## The array value already has one ownership for each item: there is no
      ## no need to add one here.
      T.Ref_Count := T.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out ${cls.name()}) is
   begin
      if T = null then
         return;
      end if;

      ## The array value has only one ownership for each item: remove this
      ## owneship only when destroying the array.

      if T.Ref_Count = 1 then
         % if cls.element_type().is_refcounted():
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

</%def>
