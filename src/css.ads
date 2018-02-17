private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with WL.String_Maps;

package Css is

   subtype Css_Float is Float range Float'Range;

   type Css_Color_Intensity is mod 256;

   type Css_Color is
      record
         Red   : Css_Color_Intensity := 0;
         Green : Css_Color_Intensity := 0;
         Blue  : Css_Color_Intensity := 0;
         Alpha : Css_Color_Intensity := Css_Color_Intensity'Last;
      end record;

   type Css_Direction is (Across, Down);

   type Css_Side is (Left, Top, Right, Bottom);

   type Css_Corner is (Top_Left, Top_Right, Bottom_Right, Bottom_Left);

   type Css_Position is (Static, Relative, Fixed, Absolute);

   type Css_Display is (None, Block, Inline);

   subtype Css_Active_Position is Css_Position range Relative .. Absolute;

   subtype Css_Active_Display is Css_Display range Block .. Inline;

   type Layout_Position is
      record
         X, Y : Css_Float := 0.0;
      end record;

   function Image (Position : Layout_Position) return String;

   type Layout_Size is
      record
         Constrained_Width  : Boolean := False;
         Constrained_Height : Boolean := False;
         Width, Height      : Css_Float := 0.0;
      end record;

   function Image (Size : Layout_Size) return String;

   function Merge (Left, Right : Layout_Size) return Layout_Size;

   function Bottom_Right_Position
     (Top_Left : Layout_Position;
      Size     : Layout_Size)
      return Layout_Position;

   function Bottom_Right_Size
     (Top_Left : Layout_Position;
      Size     : Layout_Size)
      return Layout_Size;

   type Css_Element_Value is private;
   Null_Element_Value : constant Css_Element_Value;

   function Is_Function (Value : Css_Element_Value) return Boolean;
   function Is_String (Value : Css_Element_Value) return Boolean;
   function Is_Tuple (Value : Css_Element_Value) return Boolean;

   function To_String (Value : Css_Element_Value) return String;
   function To_Color (Value : Css_Element_Value) return Css_Color;

   function Function_Name (Value : Css_Element_Value) return String
   with Pre => Is_Function (Value);
   function Argument_Count (Value : Css_Element_Value) return Natural
     with Post => not Is_String (Value) or else Argument_Count'Result = 1;

   function Argument
     (Value : Css_Element_Value;
      Index : Positive) return Css_Element_Value
     with Pre => (Is_Function (Value) and then Index <= Argument_Count (Value))
     or else (Is_String (Value) and then Index = 1);

   function Pixels (Px : Integer) return Css_Element_Value;

   type Layout_Interface is interface;

   function Get_Layout_Position
     (Layout : Layout_Interface)
      return Layout_Position
      is abstract;

   procedure Set_Layout_Position
     (Layout   : in out Layout_Interface;
      Position : Layout_Position)
   is abstract;

   function Get_Layout_Size
     (Layout : Layout_Interface)
      return Layout_Size
      is abstract;

   procedure Set_Layout_Size
     (Layout : in out Layout_Interface;
      Size   : Layout_Size)
   is abstract;

   function Get_Layout_Bottom_Right_Position
     (Layout : Layout_Interface'Class)
      return Layout_Position
   is (Bottom_Right_Position (Layout.Get_Layout_Position,
                              Layout.Get_Layout_Size));

   function Get_Layout_Bottom_Right_Size
     (Layout : Layout_Interface'Class)
      return Layout_Size
   is (Bottom_Right_Size (Layout.Get_Layout_Position,
                          Layout.Get_Layout_Size));

   function Layout_Height
     (Layout : Layout_Interface'Class)
      return Css_Float
   is (Layout.Get_Layout_Size.Height);

   function Layout_Width
     (Layout : Layout_Interface'Class)
      return Css_Float
   is (Layout.Get_Layout_Size.Width);

   procedure Set_Layout_Width
     (Layout : in out Layout_Interface'Class;
      Width  : Css_Float);

   procedure Set_Layout_Height
     (Layout : in out Layout_Interface'Class;
      Height : Css_Float);

   type Css_Styled_Interface is interface;

   procedure Create_Style
     (Styles : in out Css_Styled_Interface;
      Name   : String)
   is abstract;

   procedure Set_Style
     (Element : in out Css_Styled_Interface;
      Name    : String;
      State   : String;
      Value   : Css_Element_Value)
   is abstract;

   procedure Set_Style
     (Element : in out Css_Styled_Interface'Class;
      Name    : String;
      Value   : Css_Element_Value);

   function Style
     (Element : Css_Styled_Interface;
      Name    : String)
      return Css_Element_Value
      is abstract;

   type Css_Element_Interface is interface
     and Layout_Interface
     and Css_Styled_Interface;

   type Css_Element is access all Css_Element_Interface'Class;

   function Tag
     (Element : Css_Element_Interface)
      return String
      is abstract;

   function Id
     (Element : Css_Element_Interface)
      return String
      is abstract;

   function Classes
     (Element : Css_Element_Interface)
      return String
      is abstract;

   function Minimum_Size
     (Element    : Css_Element_Interface;
      Constraint : Layout_Size)
      return Layout_Size
      is abstract;

   function Required_Parent_Tag
     (Element : Css_Element_Interface)
      return String
      is abstract;

   function Parent_Element
     (Element : Css_Element_Interface)
      return access Css_Element_Interface'Class
      is abstract;

   function Child_Index
     (Element : Css_Element_Interface)
      return Natural
      is abstract;
   --  return the index of this element in its parent
   --  i.e. 1 if element is the first child, etc
   --  return 0 if this element does not have a parent

   function Contents_Layout_Size
     (Element : Css_Element_Interface)
      return Layout_Size
      is abstract;

   procedure Set_Contents_Layout_Size
     (Element : in out Css_Element_Interface;
      Size    : Layout_Size)
   is abstract;

   procedure Log
     (Element : Css_Element_Interface'Class;
      Message : String);

   type Array_Of_Elements is
     array (Positive range <>) of Css_Element;

   function No_Elements return Array_Of_Elements;

   function Child_Elements
     (Element : Css_Element_Interface)
      return Array_Of_Elements
      is abstract;

   function Child_Elements_With_Tag
     (Element : Css_Element_Interface'Class;
      Tag     : String)
      return Array_Of_Elements;

   function Get_Children_With_Selector
     (Element  : Css_Element_Interface'Class;
      Selector : String)
      return Array_Of_Elements;

   function Get_Child_With_Id
     (Element : Css_Element_Interface'Class;
      Id      : String)
      return Css_Element;

   function Default_Style_Value
     (Element : Css_Element_Interface;
      Name    : String)
      return Css_Element_Value
      is abstract;

   function Style_To_String
     (Element    : Css_Element_Interface'Class;
      Style_Name : String)
      return String;

   function Display
     (Element : Css_Element_Interface'Class)
      return Css_Display
   is (Css_Display'Value (Element.Style_To_String ("display")));

   function Margin_Pixels
     (Element : Css_Element_Interface'Class;
      Side    : Css_Side)
      return Css_Float;

   function Border_Pixels
     (Element  : Css_Element_Interface'Class;
      Side     : Css_Side)
      return Css_Float;

   function Border_Radius_Pixels
     (Element  : Css_Element_Interface'Class;
      Corner   : Css_Corner)
      return Css_Float;

   function Padding_Pixels
     (Element  : Css_Element_Interface'Class;
      Side     : Css_Side)
      return Css_Float;

   function Top_Element
     (Element : Css_Element_Interface'Class)
      return Css_Element;

   function Position
     (Element : Css_Element_Interface'Class)
      return Css_Position;

   function Positioned
     (Element : Css_Element_Interface'Class)
      return Boolean
   is (Element.Position /= Static);

   function Positioned_Parent
     (Element : Css_Element_Interface'Class)
      return Css_Element;

   function Has_Style
     (Element : Css_Element_Interface'Class;
      Name    : String)
      return Boolean;

   function Has_Width_Style
     (Element : Css_Element_Interface'Class)
      return Boolean
   is (Element.Has_Style ("width"));

   function Has_Height_Style
     (Element : Css_Element_Interface'Class)
      return Boolean
   is (Element.Has_Style ("height"));

   function Width_Style
     (Element : Css_Element_Interface'Class)
      return Css_Element_Value
   is (Element.Style ("width"));

   function Height_Style
     (Element : Css_Element_Interface'Class)
      return Css_Element_Value
   is (Element.Style ("height"));

   function Has_Left_Style
     (Element : Css_Element_Interface'Class)
      return Boolean
   is (Element.Has_Style ("left"));

   function Has_Top_Style
     (Element : Css_Element_Interface'Class)
      return Boolean
   is (Element.Has_Style ("top"));

   function Has_Right_Style
     (Element : Css_Element_Interface'Class)
      return Boolean
   is (Element.Has_Style ("right"));

   function Has_Bottom_Style
     (Element : Css_Element_Interface'Class)
      return Boolean
   is (Element.Has_Style ("bottom"));

   function Left_Style
     (Element : Css_Element_Interface'Class)
      return Css_Element_Value
   is (Element.Style ("left"));

   function Top_Style
     (Element : Css_Element_Interface'Class)
      return Css_Element_Value
   is (Element.Style ("top"));

   function Right_Style
     (Element : Css_Element_Interface'Class)
      return Css_Element_Value
   is (Element.Style ("right"));

   function Bottom_Style
     (Element : Css_Element_Interface'Class)
      return Css_Element_Value
   is (Element.Style ("bottom"));

   function Measure
     (Element : Css_Element_Interface'Class;
      Value   : Css_Element_Value;
      Side    : Css_Side)
      return Css_Float;

   function Measure
     (Element   : Css_Element_Interface'Class;
      Value     : Css_Element_Value;
      Available : Css_Float)
      return Css_Float;

   function Measure_Position
     (Parent  : Css_Element_Interface'Class;
      Element : not null access Css_Element_Interface'Class;
      Side    : Css_Side)
      return Css_Float;

   procedure Apply_Layout
     (Top_Element : not null access Css_Element_Interface'Class);

   procedure Apply_Table_Layout
     (Top_Element : not null access Css_Element_Interface'Class)
     with Pre => Top_Element.Is_Table;

   procedure Set_Style
     (Element : in out Css_Element_Interface'Class;
      Name    : String;
      Value   : String);

   function Match_Class
     (Element   : Css_Element_Interface'Class;
      Css_Class : String)
      return Boolean;

   function Match_Tag
     (Element  : Css_Element_Interface'Class;
      Tag_Name : String)
      return Boolean;

   function Is_Table (Element : Css_Element_Interface) return Boolean
                      is abstract;

   function Is_Table_Row (Element : Css_Element_Interface'Class) return Boolean
   is (Element.Match_Tag ("tr"));

   function Is_Table_Cell
     (Element : Css_Element_Interface'Class)
      return Boolean
   is (Element.Match_Tag ("td") or else Element.Match_Tag ("th"));

   type Array_Of_Element_Values is
     array (Positive range <>) of Css_Element_Value;

   type Evaluator is access
     function (Context : Layout_Interface'Class;
               Args    : Array_Of_Element_Values)
               return Css_Element_Value;

   procedure Add_Evaluator
     (Name : String;
      Eval : Evaluator);

   function Evaluate
     (Context : Layout_Interface'Class;
      Value   : Css_Element_Value)
      return Css_Element_Value;

   function Null_Context return Layout_Interface'Class'Class;

   type Css_Selector_Record is tagged private;

   type Css_Selector is access all Css_Selector_Record'Class;

   type Css_Rule_Record is tagged private;

   type Css_Rule is access all Css_Rule_Record'Class;

   function Inline_Style_Rules
     (Element : Css_Element_Interface)
      return Css_Rule
      is abstract;

   type Style_Sheet_Record is tagged private;

   type Style_Sheet is access all Style_Sheet_Record'Class;

   procedure Append_Rule
     (Sheet     : in out Style_Sheet_Record'Class;
      Selector  : Css_Selector;
      Rule      : Css_Rule);

   procedure Load_Style_Rules
     (Style_Sheet : Style_Sheet_Record'Class;
      Element     : in out Css_Element_Interface'Class);

   function Current_Style_Sheet
     return Style_Sheet;

   type Css_Style_Map is
     new Css_Styled_Interface with private;

   overriding procedure Create_Style
     (Map    : in out Css_Style_Map;
      Name   : String);

   overriding procedure Set_Style
     (Map    : in out Css_Style_Map;
      Name   : String;
      State  : String;
      Value  : Css_Element_Value);

   overriding function Style
     (Map  : Css_Style_Map;
      Name : String)
      return Css_Element_Value;

   function Default_Style_Value
     (Style_Name : String)
      return Css_Element_Value;

   function Is_Inherited
     (Style_Name : String)
      return Boolean;

private

   procedure Apply_Layout
     (Element : not null access Css_Element_Interface'Class;
      Flow    : in out Layout_Position);

   type Css_Element_Value_Type is
     (String_Value,
      Function_Value,
      Reference_Value);

   type Css_Element_Value_Record (Value_Type : Css_Element_Value_Type);

   type Css_Element_Value is access all Css_Element_Value_Record;

   Null_Element_Value : constant Css_Element_Value := null;

   function New_Value
     (From_String : String)
      return Css_Element_Value;

   function New_Value
     (Function_Name : String;
      Arguments     : Array_Of_Element_Values)
      return Css_Element_Value;

   function Reference_Value (Name : String) return Css_Element_Value;

   procedure Define_Value (Name : String;
                           Value : Css_Element_Value);

   package Element_Value_Vectors is
     new Ada.Containers.Vectors (Positive, Css_Element_Value);

   type Css_Element_Value_Record (Value_Type : Css_Element_Value_Type) is
      record
         case Value_Type is
            when String_Value =>
               Val_String : Ada.Strings.Unbounded.Unbounded_String;
            when Function_Value =>
               Val_Fn_Name : Ada.Strings.Unbounded.Unbounded_String;
               Val_Args    : Element_Value_Vectors.Vector;
            when Reference_Value =>
               Val_Ref_Name : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   type Selector_Element_Class is
     (Inline_Style,
      Identity,
      Css_Class,
      Attribute,
      Element_Tag);

   function Selector_Class_Specificity
     (Selector_Class : Selector_Element_Class)
      return Natural
   is ((case Selector_Class is
           when Inline_Style          => 1_000,
           when Identity              => 100,
           when Css_Class | Attribute => 10,
           when Element_Tag           => 1));

   type Selector_Element is
      record
         Class      : Selector_Element_Class := Css_Class;
         Text       : Ada.Strings.Unbounded.Unbounded_String;
         State      : Ada.Strings.Unbounded.Unbounded_String;
         State_Args : Element_Value_Vectors.Vector;
      end record;

   function Selector_Specificity
     (Element : Selector_Element)
      return Natural;

   package List_Of_Selector_Elements is
     new Ada.Containers.Doubly_Linked_Lists (Selector_Element);

   type Css_Selector_Record is tagged
      record
         Selectors : List_Of_Selector_Elements.List;
      end record;

   type Style_Entry is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Value : Css_Element_Value;
      end record;

   package Style_Entry_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Style_Entry);

   type Css_Rule_Record is tagged
      record
         Styles : Style_Entry_Lists.List;
      end record;

   function To_Hex_String (Value : Natural) return String;

   type Rule_Entry is
      record
         Selector : Css_Selector;
         Styles   : Css_Rule;
      end record;

   package List_Of_Css_Rules is
     new Ada.Containers.Doubly_Linked_Lists (Rule_Entry);

   type Style_Sheet_Record is tagged
      record
         Rules : List_Of_Css_Rules.List;
      end record;

   package Style_Maps is new WL.String_Maps (Css_Element_Value);

   type Css_Style_Map is
     new Css_Styled_Interface with
      record
         Map : Style_Maps.Map;
      end record;

   function Bottom_Right_Position
     (Top_Left : Layout_Position;
      Size     : Layout_Size)
      return Layout_Position
   is ((Top_Left.X + (if Size.Constrained_Width then Size.Width else 0.0),
        Top_Left.Y + (if Size.Constrained_Height then Size.Height else 0.0)));

end Css;
