with Libfoolang.Rewriting; use Libfoolang.Rewriting;

procedure Process_Apply
  (Handle         : in out Rewriting_Handle;
   Abort_On_Error : Boolean := True);
--  Call Apply on Handle. If it fails, and display error messages, and if
--  Abort_On_Error, raise a Program_Error.
