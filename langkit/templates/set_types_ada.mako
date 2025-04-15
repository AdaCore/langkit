## vim: filetype=makoada

<%def name="incomplete_decl(cls)">
   type ${cls.pointed};
   type ${cls.name} is access all ${cls.pointed};
</%def>

<%def name="decl(cls)">

   <%
      elt_type = cls.element_type
      elt_equiv_func = ('Equivalent'
                        if elt_type.has_equivalent_function else
                        '"="')
      pkg_name = cls.hashed_sets_pkg_name
   %>

   package ${pkg_name} is new Ada.Containers.Hashed_Sets
     (Element_Type        => ${elt_type.name},
      "="                 => ${elt_equiv_func},
      Equivalent_Elements => ${elt_equiv_func},
      Hash                => Hash);

   subtype ${cls.set_type_name} is ${pkg_name}.Set;

   type ${cls.pointed} is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : ${cls.set_type_name};
   end record;

   Empty_${cls.pointed} : aliased ${cls.pointed} :=
     (Ref_Count => -1, Items => <>);
   ${cls.null_constant} : constant ${cls.name} :=
      Empty_${cls.pointed}'Access;

   function Contains
     (S : ${cls.name}; E : ${elt_type.name}) return Boolean;
   --  Whether this set contains the given element

   ## Helper for properties code
   function Length (T : ${cls.name}) return Natural;

   procedure Inc_Ref (T : ${cls.name});
   procedure Dec_Ref (T : in out ${cls.name});

   function Equivalent (L, R : ${cls.name}) return Boolean;

   % if ctx.properties_logging:
      function Trace_Image (S : ${cls.name}) return String;
   % endif

  procedure Free is new Ada.Unchecked_Deallocation
    (${cls.pointed}, ${cls.name});
</%def>

<%def name="body(cls)">

   <%
      elt_type = cls.element_type
      pkg_name = cls.hashed_sets_pkg_name
   %>

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

   function Length (T : ${cls.name}) return Natural
   is (Natural (T.Items.Length));

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
            ## When we destroy the set, owned values will have one less
            ## owner, so decrement their reference count.
            for Item of T.Items loop
               declare
                  Ref : ${cls.element_type.name} := Item;
               begin
                  Dec_Ref (Ref);
               end;
            end loop;
         % endif
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   --------------
   -- Contains --
   --------------

   function Contains
     (S : ${cls.name}; E : ${elt_type.name}) return Boolean is
   begin
      return S.Items.Contains (E);
   end Contains;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : ${cls.name}) return Boolean is
   begin
      return L.Items.Equivalent_Sets (R.Items);
   end Equivalent;

   % if ctx.properties_logging:
      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (S : ${cls.name}) return String is
         Result : Unbounded_String;
         First  : Boolean := True;
      begin
         Append (Result, "{");
         for Item of S.Items loop
            if First then
               First := False;
            else
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (Item));
         end loop;
         Append (Result, "}");
         return To_String (Result);
      end Trace_Image;
   % endif

</%def>
