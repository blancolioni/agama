with Ada.Text_IO;

package body Css.Logging is

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Message);
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
