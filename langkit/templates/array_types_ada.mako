## vim: filetype=makoada

<%def name="public_decl(cls)">

   <% elt_type = cls.element_type().name() %>

   type ${cls.api_name()} is array (Positive range <>) of ${elt_type};
   type ${cls.pointed()} (N : Natural) is record
      Items : ${cls.api_name()} (1 .. N);
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
   is (new ${cls.pointed()}'(Items => Copy (Items), N => Items'Length));
   % endif

   package ${cls.pkg_vector()} is new Langkit_Support.Vectors
     (${elt_type});

   ## Helper getter generated for properties code. Used in CollectionGet's code
   function Get
     (T       : ${cls.name()};
      Index   : Natural;
      Or_Null : Boolean := False) return ${elt_type}
   is
     (if Index < T.Items'Length
      then T.Items (Index + 1)
      else (if Or_Null
            then ${cls.element_type().nullexpr()}
            else raise Property_Error));

   function Length (T : ${cls.name()}) return Natural is (T.N);
   ## Helper for properties code

   procedure Destroy is new Ada.Unchecked_Deallocation
     (${cls.pointed()}, ${cls.name()});
</%def>
