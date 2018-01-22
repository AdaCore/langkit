## vim: filetype=makoada

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

package body ${ada_lib_name}.Rewriting is

   function Handle (Context : Analysis_Context) return Rewriting_Handle is
     (Get_Rewriting_Handle (Context));

   function Context (Handle : Rewriting_Handle) return Analysis_Context is
     (Handle.Context);

   function Convert is new Ada.Unchecked_Conversion
     (AST_Node_Pointer, ${root_node_type_name});
   function Convert is new Ada.Unchecked_Conversion
     (${root_node_type_name}, AST_Node_Pointer);

   function Hash (Node : AST_Node_Pointer) return Ada.Containers.Hash_Type is
     (Hash (Convert (Node)));

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle
   is
      Result : constant Rewriting_Handle := new Rewriting_Handle_Type'
        (Context => Context,
         Units   => <>);
   begin
      Set_Rewriting_Handle (Context, Result);
      return Result;
   end Start_Rewriting;

   -----------
   -- Apply --
   -----------

   procedure Apply (Handle : in out Rewriting_Handle) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Rewriting_Handle_Type, Rewriting_Handle);
      procedure Free is new Ada.Unchecked_Deallocation
        (Unit_Rewriting_Handle_Type, Unit_Rewriting_Handle);
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Rewriting_Handle_Type, Node_Rewriting_Handle);

      Ctx : constant Analysis_Context := Context (Handle);
   begin
      --  Free all resources tied to Handle
      for Unit of Handle.Units loop
         for Node of Unit.Nodes loop
            Free (Node);
         end loop;
         Free (Unit);
      end loop;
      Free (Handle);

      --  Release the rewriting handle singleton for its context
      Set_Rewriting_Handle (Ctx, Handle);
   end Apply;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle is
      use Unit_Maps;

      Context        : constant Analysis_Context := Analysis.Context (Unit);
      Context_Handle : constant Rewriting_Handle := Handle (Context);
      Filename       : constant Unbounded_String :=
         To_Unbounded_String (Get_Filename (Unit));
      Cur            : constant Cursor := Context_Handle.Units.Find (Filename);
   begin
      if Cur /= No_Element then
         return Element (Cur);
      end if;

      declare
         Result : constant Unit_Rewriting_Handle :=
            new Unit_Rewriting_Handle_Type'(Context_Handle => Context_Handle,
                                            Unit           => Unit,
                                            Nodes          => <>);
      begin
         Context_Handle.Units.Insert (Filename, Result);
         return Result;
      end;
   end Handle;

   ------------
   -- Handle --
   ------------

   function Handle
     (Node : ${root_entity.api_name}'Class) return Node_Rewriting_Handle
   is
      use Node_Maps;

      Bare_Node   : constant ${root_node_type_name} :=
         Analysis.Implementation.Bare_Node (Node);
      N           : constant AST_Node_Pointer := Convert (Bare_Node);
      Unit_Handle : constant Unit_Rewriting_Handle := Handle (Node.Get_Unit);
      Cur         : constant Cursor := Unit_Handle.Nodes.Find (N);
   begin
      if Cur /= No_Element then
         return Element (Cur);
      end if;

      declare
         Result : constant Node_Rewriting_Handle :=
            new Node_Rewriting_Handle_Type'
              (Context_Handle => Unit_Handle.Context_Handle,
               Node           => N);
      begin
         Unit_Handle.Nodes.Insert (N, Result);
         return Result;
      end;
   end Handle;

end ${ada_lib_name}.Rewriting;
