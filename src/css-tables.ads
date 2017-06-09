package Css.Tables is

   procedure Apply_Table_Layout
     (Table : Css_Element)
     with Pre => Table.Is_Table;

end Css.Tables;
