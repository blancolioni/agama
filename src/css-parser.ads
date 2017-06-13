package Css.Parser is

   procedure Load_Css_File
     (Path : String);

   function Parse_Inline_Style
     (Inline_Style : String)
      return Css_Rule;

end Css.Parser;
