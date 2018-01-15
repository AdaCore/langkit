## vim: filetype=makoada

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;

package ${ada_lib_name}.Rewriting is

   type Rewriting_Handle is private;
   --  Handle for an analysis context rewriting session

   No_Rewriting_Handle : constant Rewriting_Handle;

   function Handle (Context : Analysis_Context) return Rewriting_Handle;
   --  Return the rewriting handle associated to Context, or
   --  No_Rewriting_Handle if Context is not being rewritten.

   function Context (Handle : Rewriting_Handle) return Analysis_Context
      with Pre => Handle /= No_Rewriting_Handle;
   --  Return the analysis context associated to Handle

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle
     with Pre  => Handle (Context) = No_Rewriting_Handle,
          Post => Handle (Context) /= No_Rewriting_Handle
                  and then Start_Rewriting'Result = Handle (Context)
                  and then ${ada_lib_name}.Rewriting.Context
                             (Start_Rewriting'Result) = Context;
   --  Start a rewriting session for Context.
   --
   --  This handle will keep track of all changes to do on Context's analysis
   --  units. Once the set of changes is complete, call the Apply procedure to
   --  actually update Context. This makes it possible to inspect the "old"
   --  Context state while creating the list of changes.
   --
   --  There can be only one rewriting session per analysis context, so this
   --  will raise an Existing_Rewriting_Handle_Error exception if Context
   --  already has a living rewriting session.

   procedure Apply (Handle : in out Rewriting_Handle)
      with Pre  => Handle /= No_Rewriting_Handle,
           Post => Handle = No_Rewriting_Handle;
   --  Apply all modifications to Handle's analysis context and close Handle

private

   type Rewriting_Handle_Type is record
      Context : Analysis_Context;
   end record;

   type Rewriting_Handle is access Rewriting_Handle_Type;
   No_Rewriting_Handle : constant Rewriting_Handle := null;

end ${ada_lib_name}.Rewriting;
