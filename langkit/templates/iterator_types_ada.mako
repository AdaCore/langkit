## vim: filetype=makoada

<%def name="public_api_decl(cls)">
   type ${cls.api_name} is private;
   ${ada_doc('langkit.iterator_type', 3)}

   <% class_suffix = "'Class" if cls.element_type.is_entity_type else "" %>

   function Next
     (Self : ${cls.api_name};
      Item : out ${cls.element_type.api_name}${class_suffix}) return Boolean;
   ${ada_doc('langkit.iterator_next')}
</%def>

<%def name="public_api_private_decl(cls)">
   ## Since the Next function potentially has a class-wide argument, the public
   ## type (cls.api_name) cannot be tagged itself ("operation can be
   ## dispatching in only one type"), so it cannot be controlled.
   ##
   ## Still, to properly handle resources, we still need a controlled record
   ## type, so define an intermediate one.

   type ${cls.api_name}_Controlled is new Ada.Finalization.Controlled with
   record
      Internal_Iterator : Implementation.${cls.name};
   end record;

   type ${cls.api_name} is record
      Controlled : ${cls.api_name}_Controlled;
   end record;

   overriding procedure Initialize (Self : in out ${cls.api_name}_Controlled);
   overriding procedure Adjust     (Self : in out ${cls.api_name}_Controlled);
   overriding procedure Finalize   (Self : in out ${cls.api_name}_Controlled);
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
   % if cls.to_internal_converter_required:
      function ${cls.to_internal_converter}
        (Value : ${cls.api_name}) return ${cls.name};
   % endif
</%def>

<%def name="public_api_body(cls)">
   <% class_suffix = "'Class" if cls.element_type.is_entity_type else "" %>

   % if cls.to_public_converter_required:
      function ${cls.to_public_converter}
         (Value : ${cls.name}) return ${cls.api_name} is
      begin
         Inc_Ref (Value);
         return (Controlled => (Ada.Finalization.Controlled with
                                Internal_Iterator => Value));
      end;
   % endif

   % if cls.to_internal_converter_required:
      function ${cls.to_internal_converter}
        (Value : ${cls.api_name}) return ${cls.name} is
      begin
         return Result : constant ${cls.name} :=
            Value.Controlled.Internal_Iterator
         do
            if Result /= null then
               Implementation.Check_Safety_Net (Result.Safety_Net);
               Inc_Ref (Result);
            end if;
         end return;
      end;
   % endif

   ----------
   -- Next --
   ----------

   function Next
     (Self : ${cls.api_name};
      Item : out ${cls.element_type.api_name}${class_suffix}) return Boolean
   is
      Internal_Iter    : constant ${cls.name} :=
         Self.Controlled.Internal_Iterator;
      Internal_Element : ${cls.element_type.name};
      Result           : Boolean;
   begin
      if Internal_Iter = null then
         raise Precondition_Failure with "null iterator argument";
      end if;

      Result := Next (Internal_Iter, Internal_Element);
      if Result then
      % if cls.element_type.is_entity_type:
         Item := ${cls.element_type.api_name}'Class
           (${cls.element_type.to_public_expr('Internal_Element')});
      % else:
         Item := ${cls.element_type.to_public_expr('Internal_Element')};
      % endif
      end if;
      return Result;
   end Next;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out ${cls.api_name}_Controlled) is
   begin
      Self.Internal_Iterator := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out ${cls.api_name}_Controlled) is
   begin
      Inc_Ref (Self.Internal_Iterator);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out ${cls.api_name}_Controlled) is
   begin
      Dec_Ref (Self.Internal_Iterator);
   end Finalize;
</%def>

<%def name="decl(cls)">

   <% elt_type = cls.element_type.name %>

   type ${cls.iterator_type_name} is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : ${cls.element_type.array.name};
      Index    : Positive;
   end record;

   Empty_${cls.iterator_type_name} : aliased ${cls.iterator_type_name} :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => ${cls.element_type.array.null_constant},
      Index      => 1);
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
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

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
      if T /= null and then T.Ref_Count >= 0 then
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
         return "<Iterator of ${cls.element_type.dsl_name}, index="
                & A.Index'Image & ">";
      end Trace_Image;
   % endif

</%def>
