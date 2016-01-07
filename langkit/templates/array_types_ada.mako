## vim: filetype=makoada

<%def name="public_decl(cls)">

   <% type = decl_type(cls.element_type) %>

   type ${type}_Array is array (Positive range <>) of ${type};
   type ${type}_Array_Record (N : Natural) is record
      Items : ${type}_Array (1 .. N);
   end record;

   type ${type}_Array_Access is access all ${type}_Array_Record;

   ## If we are on the root grammar class type, we need a conversion function
   ## to be able to get element arrays starting from 0 and convert them into
   ## arrays starting from 1. We need it to convert from env element arrays,
   ## that are arrays of instances of the root grammar class, to our array
   ## record type.
   % if element_type == ctx.root_grammar_class:
   function Copy is new AST_Envs.Element_Arrays.Copy (Positive, ${type}_Array);

   function Create (Items : AST_Envs.Element_Array) return ${type}_Array_Access
   is (new ${type}_Array_Record'(Items => Copy (Items), N => Items'Length));
   % endif

   package ${type}_Vectors is new Ada.Containers.Vectors
     (Index_Type  => Positive,
      Element_Type => ${type});

</%def>
