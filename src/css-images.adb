with Glib;
with Cairo.Image_Surface;
with Cairo.Png;

with Xi.String_Maps;

package body Css.Images is

   package Image_Surface_Maps is
     new Xi.String_Maps (Cairo.Cairo_Surface, Cairo."=");

   Image_Cache : Image_Surface_Maps.Map;

   --------------------
   -- Get_Image_Size --
   --------------------

   procedure Get_Image_Size
     (Path          : String;
      Width, Height : out Float)
   is
      Surface    : constant Cairo.Cairo_Surface := Get_Image_Surface (Path);
      Img_Height : constant Glib.Gint :=
                     Cairo.Image_Surface.Get_Height (Surface);
      Img_Width  : constant Glib.Gint :=
                     Cairo.Image_Surface.Get_Width (Surface);
   begin
      Width := Float (Img_Width);
      Height := Float (Img_Height);
   end Get_Image_Size;

   -----------------------
   -- Get_Image_Surface --
   -----------------------

   function Get_Image_Surface
     (Path : String)
      return Cairo.Cairo_Surface
   is
   begin
      if not Image_Cache.Contains (Path) then
         declare
            Image : constant Cairo.Cairo_Surface :=
                      Cairo.Png.Create_From_Png (Path);
         begin
            Image_Cache.Insert (Path, Image);
         end;
      end if;
      return Image_Cache.Element (Path);
   end Get_Image_Surface;

end Css.Images;
