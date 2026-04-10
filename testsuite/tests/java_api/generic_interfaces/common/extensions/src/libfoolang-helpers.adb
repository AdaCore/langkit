with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;          use Libfoolang.Analysis;
with Libfoolang.Common;            use Libfoolang.Common;
with Libfoolang.Public_Converters; use Libfoolang.Public_Converters;

package body Libfoolang.Helpers is

   type My_Unit_Provider is new Unit_Provider_Interface with null record;

   overriding function Get_Unit_Filename
     (Provider : My_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;
   overriding function Get_Unit
     (Provider : My_Unit_Provider;
      Context  : Analysis_Context'Class;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return Analysis_Unit'Class;
   overriding procedure Release (Provider : in out My_Unit_Provider) is null;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : My_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is
   begin
      return
        Image (Name)
        & "-"
        & (case Kind is
           when Unit_Specification => "spec",
           when Unit_Body          => "body")
        & ".txt";
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider : My_Unit_Provider;
      Context  : Analysis_Context'Class;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return Analysis_Unit'Class
   is
   begin
      return Analysis_Unit'Class
        (Context.Get_From_File
           (Provider.Get_Unit_Filename (Name, Kind), Charset, Reparse));
   end Get_Unit;

   --------------------------
   -- Create_Unit_Provider --
   --------------------------

   function Create_Unit_Provider return Internal_Unit_Provider_Access is
   begin
      return Wrap_Public_Provider
        (Create_Unit_Provider_Reference (My_Unit_Provider'(null record)));
   end Create_Unit_Provider;

end Libfoolang.Helpers;
