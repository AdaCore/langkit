## vim: filetype=makoada

<%def name="public_api_decl(cls)">
  type ${cls.api_name} is tagged private;
  ${ada_doc('langkit.iterator_type')}

  function Next
    (I : ${cls.api_name};
     E : out ${"{}{}".format(
         cls.element_type.api_name,
         "'Class" if cls.element_type.is_entity_type else ''
     )}) return Boolean;
  ${ada_doc('langkit.iterator_next')}
</%def>

<%def name="public_api_private_decl(cls)">
  type ${cls.api_name} is new Ada.Finalization.Controlled with record
     Internal_Iterator : Implementation.${cls.name};
  end record;

  overriding procedure Initialize (T : in out ${cls.api_name});
  overriding procedure Adjust     (T : in out ${cls.api_name});
  overriding procedure Finalize   (T : in out ${cls.api_name});
</%def>

<%def name="incomplete_decl(cls)">
   type ${cls.iterator_type_name};
   type ${cls.name} is access all ${cls.iterator_type_name};
</%def>

<%def name="ada_api_converters_decl(cls)">
   % if cls.to_public_converter_required:
      function ${cls.to_public_converter}
         (Value : ${cls.name}) return ${cls.api_name};
   % endif
</%def>

<%def name="public_api_body(cls)">
   % if cls.to_public_converter_required:
      function ${cls.to_public_converter}
         (Value : ${cls.name}) return ${cls.api_name} is
      begin
         Inc_Ref (Value);
         return (Ada.Finalization.Controlled with Internal_Iterator => Value);
      end;
   % endif

  ----------
  -- Next --
  ----------

  function Next
    (I : ${cls.api_name};
     E : out ${"{}{}".format(
         cls.element_type.api_name,
         "'Class" if cls.element_type.is_entity_type else ''
     )}) return Boolean
  is
     Internal_Element : ${cls.element_type.name};
     Result           : Boolean :=
        Next (I.Internal_Iterator, Internal_Element);
  begin
     if Result then
     % if cls.element_type.is_entity_type:
        E := ${cls.element_type.api_name}'Class
          (${cls.element_type.to_public_expr('Internal_Element')});
     % else:
        E := ${cls.element_type.to_public_expr('Internal_Element')};
     % endif
     end if;
     return Result;
  end Next;

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize (T : in out ${cls.api_name}) is
  begin
     T.Internal_Iterator := null;
  end Initialize;

  ------------
  -- Adjust --
  ------------

  overriding procedure Adjust (T : in out ${cls.api_name}) is
  begin
     if T.Internal_Iterator /= null then
        Inc_Ref (T.Internal_Iterator);
     end if;
  end Adjust;

  --------------
  -- Finalize --
  --------------

  procedure Finalize (T : in out ${cls.api_name}) is
  begin
     if T.Internal_Iterator /= null then
        Dec_Ref (T.Internal_Iterator);
        T.Internal_Iterator := null;
     end if;
  end Finalize;
</%def>

<%def name="decl(cls)">

   <% elt_type = cls.element_type.name %>

   type ${cls.iterator_type_name} is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero

      Elements : ${cls.element_type.array.name};
      Index    : Positive;
   end record;

   Empty_${cls.iterator_type_name} : aliased ${cls.iterator_type_name} :=
     (Ref_Count => -1,
      Elements => ${cls.element_type.array.null_constant},
      Index => 1);
   ${cls.null_constant} : constant ${cls.name} :=
      Empty_${cls.iterator_type_name}'Access;

   function Next
     (T       : ${cls.name};
      Element : out ${elt_type}) return Boolean;

   procedure Inc_Ref (T : ${cls.name});
   procedure Dec_Ref (T : in out ${cls.name});

   % if ctx.properties_logging:
      function Trace_Image (A : ${cls.name}) return String;
   % endif

  procedure Free is new Ada.Unchecked_Deallocation
    (${cls.iterator_type_name}, ${cls.name});
</%def>

<%def name="body(cls)">

   <% elt_type = cls.element_type.name %>

   ----------
   -- Next --
   ----------

   function Next
     (T       : ${cls.name};
      Element : out ${elt_type}) return Boolean is
   begin
      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : ${cls.name}) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out ${cls.name}) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   % if ctx.properties_logging:
      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : ${cls.name}) return String is
      begin
         return "<Iterator with index =>" & A.Index'Image & ">";
      end Trace_Image;
   % endif

</%def>
