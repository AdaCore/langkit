## vim: filetype=makoada

<% type = decl_type(element_type) %>

% if not private_part:

   type ${type}_Array is array (Positive range <>) of ${type};
   type ${type}_Array_Record (N : Natural) is record
      Items : ${type}_Array (1 .. N);
   end record;

   type ${type}_Array_Access is access all ${type}_Array_Record;

   package ${type}_Vectors is new Ada.Containers.Vectors
     (Index_Type  => Positive,
      Element_Type => ${type});

% endif
