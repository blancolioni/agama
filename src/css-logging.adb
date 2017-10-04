with Ada.Text_IO;

package body Css.Logging is

   Log_File_Name : constant String := "agama.log";
   Log_File      : Ada.Text_IO.File_Type;
   Logging       : Boolean := False;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      if not Logging then
         Ada.Text_IO.Create (Log_File, Ada.Text_IO.Out_File, Log_File_Name);
         Logging := True;
      end if;
      Ada.Text_IO.Put_Line (Log_File, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Element : Css_Element_Interface'Class;
      Message : String)
   is
      Header : constant String :=
                 Element.Tag
                 & (if Element.Classes /= ""
                    then " ." & Element.Classes
                    else "")
                 & (if Element.Id /= ""
                    then " #" & Element.Id
                    else "");
   begin
      if Header /= "" then
         Log (Header & ": " & Message);
      end if;
   end Log;

end Css.Logging;
