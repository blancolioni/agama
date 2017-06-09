with Cairo;

package Css.Images is

   function Get_Image_Surface
     (Path : String)
      return Cairo.Cairo_Surface;

   procedure Get_Image_Size
     (Path          : String;
      Width, Height : out Float);

end Css.Images;
