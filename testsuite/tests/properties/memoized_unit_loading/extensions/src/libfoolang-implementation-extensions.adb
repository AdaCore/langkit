package body Libfoolang.Implementation.Extensions is

   --------------------------------------
   -- Example_P_Ext_Fetch_Example_Unit --
   --------------------------------------

   function Example_P_Ext_Fetch_Example_Unit
     (Node : Bare_Example) return Internal_Unit is
   begin
      return Get_From_File
        (Context  => Node.Unit.Context,
         Filename => "example.txt",
         Charset  => Default_Charset,
         Reparse  => False,
         Rule     => Default_Grammar_Rule);
   end Example_P_Ext_Fetch_Example_Unit;

   ------------------------------
   -- Example_P_Ext_Unit_Count --
   ------------------------------

   function Example_P_Ext_Unit_Count
     (Node : Bare_Example) return Integer is
   begin
      return Integer (Node.Unit.Context.Units.Length);
   end Example_P_Ext_Unit_Count;

end Libfoolang.Implementation.Extensions;
