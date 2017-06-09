with Ada.Text_IO;

package body Css.Writer is

   procedure Write_Rule (File : Ada.Text_IO.File_Type;
                         Name : String;
                         Rule : Css_Rule);

   -----------
   -- Write --
   -----------

   procedure Write
     (Css : Css_Rule;
      Path : String)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      for Parent of Css.Parents loop
         Write_Rule (File, Parent.Name.all, Parent);
      end loop;
      for State in Css.States.Iterate loop
         Write_Rule (File, Rule_Maps.Key (State),
                     Rule_Maps.Element (State));
      end loop;
      Close (File);
   end Write;

   ----------------
   -- Write_Rule --
   ----------------

   procedure Write_Rule (File : Ada.Text_IO.File_Type;
                         Name : String;
                         Rule : Css_Rule)
   is
      use Ada.Text_IO;
      pragma Unreferenced (Name);
   begin
      Put_Line (File, Rule.Name.all & " {");
      for Position in Rule.Selector_Map.Iterate loop
         Put_Line (File,
                   Selector_Maps.Key (Position) & ": "
                   & To_String (Selector_Maps.Element (Position)));
      end loop;
      Put_Line (File, "}");
   end Write_Rule;

end Css.Writer;
