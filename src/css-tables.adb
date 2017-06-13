with Ada.Containers.Vectors;

package body Css.Tables is

   type Table_Grid_Element is
      record
         Element   : Css_Element;
         Width     : Float;
         Height    : Float;
         Table_Row : Positive;
         Row_Cell  : Positive;
         Grid_Row  : Positive;
         Grid_Col  : Positive;
         Row_Span  : Natural;
         Col_Span  : Natural;
      end record;

   package Grid_Element_Vectors is
     new Ada.Containers.Vectors (Positive, Table_Grid_Element);

   type Table_Grid_Row is
      record
         Element   : Css_Element;
         Width     : Float;
         Height    : Float;
         Table_Row : Positive;
         Grid_Row  : Positive;
         Cells     : Grid_Element_Vectors.Vector;
      end record;

   package Grid_Row_Vectors is
     new Ada.Containers.Vectors (Positive, Table_Grid_Row);

   type Table_Grid is
      record
         Element : Css_Element;
         Size    : Layout_Size;
         Rows    : Grid_Row_Vectors.Vector;
      end record;

   procedure Create_Grid
     (Table : Css_Element;
      Grid  : in out Table_Grid);

   ------------------------
   -- Apply_Table_Layout --
   ------------------------

   procedure Apply_Table_Layout
     (Table : Css_Element)
   is
      --  Top_Left   : constant Layout_Position := Table.Get_Layout_Position;
      Current       : Layout_Position := (0.0, 0.0);
      Grid          : Table_Grid;
   begin
      Create_Grid (Table, Grid);
      for Row of Grid.Rows loop
         Row.Element.Set_Layout_Position (Current);

         for Cell of Row.Cells loop
            if Cell.Col_Span > 0 and then Cell.Row_Span > 0 then
               Cell.Element.Set_Layout_Position ((Current.X, 0.0));
               Cell.Element.Set_Layout_Width (Cell.Width);
               Cell.Element.Set_Layout_Height (Row.Height);
               Cell.Element.Log
                 ("set final position: "
                  & Css.Image (Cell.Element.Get_Layout_Position));
            end if;
            Current.X := Current.X + Cell.Width;
         end loop;

         Current.X := 0.0;
         Current.Y := Current.Y + Row.Height;
      end loop;
   end Apply_Table_Layout;

   -----------------
   -- Create_Grid --
   -----------------

   procedure Create_Grid
     (Table : Css_Element;
      Grid  : in out Table_Grid)
   is
      Table_Body  : constant Array_Of_Elements :=
                      Table.Child_Elements_With_Tag ("tbody");
      Row_Parent  : constant Css_Element :=
                      (if Table_Body'Length > 0
                       then Table_Body (Table_Body'First)
                       else Table);
      Rows        : constant Array_Of_Elements :=
                      Row_Parent.Child_Elements_With_Tag ("tr");
      Row_Index   : Natural := 0;
      Col_Count   : Natural := 0;
   begin
      Grid.Element := Table;
      for Row of Rows loop
         Row_Index := Row_Index + 1;
         declare
            Grid_Row   : Table_Grid_Row :=
                           Table_Grid_Row'
                             (Element   => Row,
                              Height    => 0.0,
                              Width     => 0.0,
                              Table_Row => Row_Index,
                              Grid_Row  => Row_Index,
                              Cells     => <>);
            Cells      : constant Array_Of_Elements :=
                           Row.Child_Elements;
            Cell_Index : Natural := 0;
         begin
            Row.Log ("row: cell count =" & Natural'Image (Cells'Length));
            Col_Count := Natural'Max (Col_Count, Cells'Length);
            for Cell of Cells loop
               Cell.Log ("cell");
               Cell_Index := Cell_Index + 1;
               declare
                  Grid_Cell : Table_Grid_Element :=
                                Table_Grid_Element'
                                  (Element   => Cell,
                                   Width     => 0.0,
                                   Height    => 0.0,
                                   Table_Row => Row_Index,
                                   Row_Cell  => Cell_Index,
                                   Grid_Row  => Row_Index,
                                   Grid_Col  => Cell_Index,
                                   Row_Span  => 1,
                                   Col_Span  => 1);
               begin
                  Cell.Apply_Layout;
                  Cell.Set_Layout_Size (Cell.Contents_Layout_Size);
                  Grid_Cell.Width := Cell.Layout_Width;
                  Grid_Cell.Height := Cell.Layout_Height;
                  if Grid_Cell.Height > Grid_Row.Height then
                     Grid_Row.Height := Grid_Cell.Height;
                  end if;
                  Grid_Row.Cells.Append (Grid_Cell);
               end;
            end loop;
            Grid.Rows.Append (Grid_Row);
         end;
      end loop;

      declare
         Col_Widths  : array (1 .. Col_Count) of Float := (others => 0.0);
         Grid_Width  : Float := 0.0;
         Grid_Height : Float := 0.0;
      begin
         for Row of Grid.Rows loop
            declare
               Cell_Index : Positive := 1;
            begin
               Grid_Height := Grid_Height + Row.Height;
               for Cell of Row.Cells loop
                  if Cell.Col_Span = 1 then
                     declare
                        Width : Float renames Col_Widths (Cell_Index);
                     begin
                        Width := Float'Max (Width, Cell.Width);
                     end;
                  end if;
                  Cell_Index := Cell_Index + Cell.Col_Span;
               end loop;
            end;
         end loop;

         for Width of Col_Widths loop
            Grid_Width := Grid_Width + Width;
         end loop;

         for Row of Grid.Rows loop
            declare
               Cell_Index : Positive := 1;
            begin
               Row.Width := Grid_Width;
               Row.Element.Set_Layout_Width (Grid_Width);
               Row.Element.Set_Layout_Height (Row.Height);
               Row.Element.Log (Css.Image (Row.Element.Get_Layout_Size));
               for Cell of Row.Cells loop
                  Cell.Width := 0.0;
                  for I in 1 .. Cell.Col_Span loop
                     Cell.Width :=
                       Cell.Width + Col_Widths (Cell_Index);
                     Cell_Index := Cell_Index + 1;
                  end loop;
               end loop;
            end;
         end loop;

         Table.Set_Layout_Width (Grid_Width);
         Table.Set_Layout_Height (Grid_Height);
         Table.Log (Css.Image (Table.Get_Layout_Size));
      end;

   end Create_Grid;

end Css.Tables;
