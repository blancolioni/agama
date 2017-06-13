with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Text_IO;

with Css.Paths;

with Css.Logging;

with Css.Tables;

package body Css is

   function To_Lower (S : String) return String
                      renames Ada.Characters.Handling.To_Lower;

   package Style_Value_Maps is
     new WL.String_Maps (Css_Element_Value);

   Default_Style_Value_Map : Style_Value_Maps.Map;

   package Inherited_Style_Sets is
     new Ada.Containers.Indefinite_Hashed_Sets
       (String, Ada.Strings.Fixed.Hash_Case_Insensitive,
        Ada.Strings.Fixed.Equal_Case_Insensitive);

   Inherited_Style : Inherited_Style_Sets.Set;

   type Null_Evaluation_Context is
     new Layout_Interface with null record;

   overriding function Get_Layout_Position
     (Layout : Null_Evaluation_Context)
      return Layout_Position
   is ((0.0, 0.0));

   overriding procedure Set_Layout_Position
     (Layout : in out Null_Evaluation_Context;
      Position : Layout_Position)
   is null;

   overriding function Get_Layout_Size
     (Layout : Null_Evaluation_Context)
      return Layout_Size
   is ((False, False, 0.0, 0.0));

   overriding procedure Set_Layout_Size
     (Layout : in out Null_Evaluation_Context;
      Size   : Layout_Size)
   is null;

   function Parse_Html_Color
     (Text : String)
      return Css_Color;

   function To_Css_Value
     (Color : Css_Color)
      return Css_Element_Value;

   package Evaluator_Maps is
     new WL.String_Maps (Evaluator);

   Evaluator_Map : Evaluator_Maps.Map;

   package Definition_Maps is
     new WL.String_Maps (Css_Element_Value);

   Definitions : Definition_Maps.Map;

   Have_Initial_Evaluators : Boolean := False;

   Local_Current_Style_Sheet : Style_Sheet;

   type Applied_Style is
      record
         Specificity : Natural;
         Style       : Style_Entry;
      end record;

   package Applied_Style_Map is
     new WL.String_Maps (Applied_Style);

   package Named_Color_Maps is
     new WL.String_Maps (Css_Color);

   Named_Colors : Named_Color_Maps.Map;

   procedure Read_Colors;

   procedure Create_Initial_Evaluators;

   procedure Load_Inherited_Style_Set;

   procedure Check_Argument_Count
     (Function_Name  : String;
      Arguments      : Array_Of_Element_Values;
      Required_Count : Natural);

   function Evaluate_Alpha
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value;

   function Evaluate_Rgb
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value;

   function Evaluate_Rgba
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value;

   function Evaluate_Shade
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value;

   function Evaluate_Url
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value;

   function Allocate (Value : Css_Element_Value_Record)
                      return Css_Element_Value;

   procedure Check_Rule
     (Element : in out Css_Element_Interface'Class;
      Current : in out Applied_Style_Map.Map'Class;
      Rule    : Rule_Entry);

   procedure Apply_Rule
     (Current          : in out Applied_Style_Map.Map'Class;
      Rule_Specificity : Natural;
      Rule             : Css_Rule);

   function Match
     (Element  : Css_Element_Interface'Class;
      Selector : Selector_Element)
      return Boolean;

   function Match_State
     (Element    : Css_Element_Interface'Class;
      State_Name : String;
      State_Args : Element_Value_Vectors.Vector)
      return Boolean;

   function Match_String
     (Space_Separated_String : String;
      Search_Text            : String)
      return Boolean
     with Unreferenced;

   function Measure
     (Context_Size : Float;
      Measurement  : String)
      return Float;

   -------------------
   -- Add_Evaluator --
   -------------------

   procedure Add_Evaluator
     (Name : String;
      Eval : Evaluator)
   is
   begin
      Evaluator_Map.Insert (Name, Eval);
   end Add_Evaluator;

   --------------
   -- Allocate --
   --------------

   function Allocate (Value : Css_Element_Value_Record)
                      return Css_Element_Value
   is
   begin
      return new Css_Element_Value_Record'(Value);
   end Allocate;

   -----------------
   -- Append_Rule --
   -----------------

   procedure Append_Rule
     (Sheet     : in out Style_Sheet_Record'Class;
      Selector  : Css_Selector;
      Rule      : Css_Rule)
   is
   begin
      Sheet.Rules.Append ((Selector, Rule));
   end Append_Rule;

   ------------------
   -- Apply_Layout --
   ------------------

   procedure Apply_Layout
     (Top_Element : not null access Css_Element_Interface'Class)
   is
      Child_Elements : constant Array_Of_Elements :=
                         Top_Element.Child_Elements;
      Flow_Position  : Layout_Position := (0.0, 0.0);
   begin
      Top_Element.Log
        ("child count:" & Natural'Image (Child_Elements'Length));
      for Child of Child_Elements loop
         Child.Log ("apply layout (flow position is "
                    & Image (Flow_Position) & ")");
         Child.Apply_Layout (Flow_Position);
         if Child.Is_Table then
            Child.Apply_Table_Layout;
         else
            Child.Apply_Layout;
         end if;
         Top_Element.Set_Contents_Layout_Size
           (Merge
              (Top_Element.Contents_Layout_Size,
               Child.Get_Layout_Bottom_Right_Size));
         Flow_Position.Y :=
           Bottom_Right_Position
             (Flow_Position, Child.Get_Layout_Size).Y;
         Flow_Position.X := 0.0;
      end loop;
      Top_Element.Log
        ("contents size: " & Image (Top_Element.Contents_Layout_Size));
   end Apply_Layout;

   ------------------
   -- Apply_Layout --
   ------------------

   procedure Apply_Layout
     (Element : not null access Css_Element_Interface'Class;
      Flow    : in out Layout_Position)
   is
      Location : Layout_Position;
      Size     : Layout_Size;
      Parent   : constant Css_Element :=
                   (if Element.Position = Absolute
                    then Element.Positioned_Parent
                    else Element.Parent_Element);
   begin

      if Element.Display = None then
         return;
      end if;

      Element.Log ("apply_layout: position is "
                   & Css_Position'Image (Element.Position));

      case Element.Position is
         when Static =>
            Location := Flow;
         when Relative =>
            Flow.X := Flow.X + Parent.Measure_Position (Element, Left);
            Flow.Y := Flow.Y + Parent.Measure_Position (Element, Top);
            Location := Flow;
         when Fixed =>
            Location :=
              (Parent.Measure_Position (Element, Left),
               Parent.Measure_Position (Element, Top));
         when Absolute =>
            Location := Parent.Get_Layout_Position;
            Location.X :=
              Location.X + Parent.Measure_Position (Element, Left);
            Location.Y :=
              Location.Y + Parent.Measure_Position (Element, Top);
      end case;

      Element.Log ("set position: " & Image (Location));
      Element.Set_Layout_Position (Location);

      if Element.Has_Width_Style then
         Size.Constrained_Width := True;
         Size.Width := Parent.Measure (Element.Width_Style, Left);
      elsif Element.Has_Left_Style
        and then Element.Has_Right_Style
        and then Element.Position /= Static
      then
         Size.Constrained_Width := True;
         declare
            Max_X : Float := 0.0;
            Right_Value : constant Float :=
                            Parent.Measure (Element.Right_Style, Right);
         begin
            case Css_Active_Position (Element.Position) is
               when Relative =>
                  Max_X := Flow.X
                    + Parent.Get_Layout_Size.Width
                    - Right_Value;
               when Fixed =>
                  Max_X :=
                    Parent.Top_Element.Get_Layout_Size.Width - Right_Value;
               when Absolute =>
                  Max_X := Right_Value;
            end case;
            Size.Width := Max_X - Element.Get_Layout_Position.X;
         end;
      end if;

      if Element.Has_Height_Style then
         Size.Constrained_Height := True;
         Size.Height := Parent.Measure (Element.Height_Style, Top);
      elsif Element.Has_Style ("top")
        and then Element.Has_Style ("bottom")
        and then Element.Position /= Static
      then
         Size.Constrained_Height := True;
         declare
            Max_Y  : Float := 0.0;
            Offset : constant Float :=
                       Parent.Measure (Element.Bottom_Style, Bottom);

         begin
            case Css_Active_Position (Element.Position) is
               when Relative =>
                  Max_Y := Flow.Y
                    + Parent.Get_Layout_Size.Height - Offset;
               when Fixed =>
                  Max_Y :=
                    Parent.Top_Element.Get_Layout_Size.Height - Offset;
               when Absolute =>
                  Max_Y := Offset;
            end case;
            Size.Height := Max_Y - Element.Get_Layout_Position.Y;
            Location.Y := Parent.Measure (Element.Top_Style, Top);
         end;
      end if;

      declare
         Min_Size : constant Layout_Size :=
                      Element.Minimum_Size (Size);
      begin
         if Min_Size.Constrained_Width and then
           (not Size.Constrained_Width or else Min_Size.Width > Size.Width)
         then
            Size.Constrained_Width := True;
            Size.Width := Min_Size.Width;
         end if;
         if Min_Size.Constrained_Height and then
           (not Size.Constrained_Height or else Min_Size.Height > Size.Height)
         then
            Size.Constrained_Height := True;
            Size.Height := Min_Size.Height;
         end if;
      end;

      Element.Set_Layout_Size (Size);

      if Element.Has_Right_Style
        and then not Element.Has_Left_Style
        and then Element.Positioned
        and then Element.Get_Layout_Size.Constrained_Width
      then
         declare
            Offset : constant Float :=
                       Parent.Measure (Element.Right_Style, Right);
         begin
            case Css_Active_Position (Element.Position) is
               when Relative =>
                  Location.X := Flow.X
                    - Size.Width - Offset;
               when Fixed =>
                  Location.X :=
                    Parent.Top_Element.Get_Layout_Size.Width
                      - Size.Width - Offset;
               when Absolute =>
                  Location.X := Offset - Size.Width;
            end case;
         end;
      end if;

      if Element.Has_Bottom_Style
        and then not Element.Has_Top_Style
        and then Element.Positioned
        and then Element.Get_Layout_Size.Constrained_Height
      then
         declare
            Offset : constant Float :=
                       Parent.Measure (Element.Bottom_Style, Bottom);
         begin
            case Css_Active_Position (Element.Position) is
               when Relative =>
                  Location.Y := Flow.Y
                    - Size.Height - Offset;
               when Fixed =>
                  Location.Y :=
                    Parent.Top_Element.Get_Layout_Size.Height
                      - Size.Height - Offset;
               when Absolute =>
                  Location.Y := Offset - Size.Height;
            end case;
         end;
      end if;

      Element.Set_Layout_Position (Location);

   end Apply_Layout;

   ----------------
   -- Apply_Rule --
   ----------------

   procedure Apply_Rule
     (Current          : in out Applied_Style_Map.Map'Class;
      Rule_Specificity : Natural;
      Rule             : Css_Rule)
   is
      use Ada.Strings.Unbounded;
   begin
      for Style of Rule.Styles loop
         declare
            Name : constant String := To_String (Style.Name);
         begin
            if not Current.Contains (Name) then
               Current.Insert (Name, (Rule_Specificity, Style));
            else
               declare
                  Existing : constant Applied_Style := Current.Element (Name);
               begin
                  if Rule_Specificity >= Existing.Specificity then
                     Current.Replace (Name, (Rule_Specificity, Style));
                  end if;
               end;
            end if;
         end;
      end loop;
   end Apply_Rule;

   ------------------------
   -- Apply_Table_Layout --
   ------------------------

   procedure Apply_Table_Layout
     (Top_Element : not null access Css_Element_Interface'Class)
   is
   begin
      Css.Tables.Apply_Table_Layout (Top_Element);
   end Apply_Table_Layout;

   --------------
   -- Argument --
   --------------

   function Argument (Value : Css_Element_Value;
                      Index : Positive) return Css_Element_Value
   is
   begin
      return Value.Val_Args (Index);
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count (Value : Css_Element_Value) return Natural is
   begin
      return Value.Val_Args.Last_Index;
   end Argument_Count;

   -------------------
   -- Border_Pixels --
   -------------------

   function Border_Pixels
     (Element  : Css_Element_Interface'Class;
      Side     : Css_Side)
      return Float
   is
      Border_Width_Side : constant String :=
                            "border-width-"
                            & To_Lower (Css_Side'Image (Side));
      Border_Side       : constant String :=
                            "border-"
                            & To_Lower (Css_Side'Image (Side));
      Border_Width      : constant String :=
                            "border-width";
      Border            : constant String := "border";
   begin
      if Element.Has_Style (Border_Width_Side) then
         return Element.Measure (Element.Style (Border_Width_Side), Side);
      elsif Element.Has_Style (Border_Side) then
         return Element.Measure
           (Argument (Element.Style (Border_Side), 1), Side);
      elsif Element.Has_Style (Border_Width) then
         return Element.Measure (Element.Style (Border_Width), Side);
      elsif Element.Has_Style (Border) then
         return Element.Measure
           (Argument (Element.Style (Border), 1), Side);
      else
         return 0.0;
      end if;
   end Border_Pixels;

   --------------------------
   -- Border_Radius_Pixels --
   --------------------------

   function Border_Radius_Pixels
     (Element  : Css_Element_Interface'Class;
      Corner   : Css_Corner)
      return Float
   is
      pragma Unreferenced (Corner);
      Value : constant Css_Element_Value := Element.Style ("border-radius");
   begin
      if Value = null then
         return 0.0;
      else
         return Element.Measure (Value, Left);
      end if;
   end Border_Radius_Pixels;

   -----------------------
   -- Bottom_Right_Size --
   -----------------------

   function Bottom_Right_Size
     (Top_Left : Layout_Position;
      Size     : Layout_Size)
      return Layout_Size
   is
      Pos : constant Layout_Position := Bottom_Right_Position (Top_Left, Size);
   begin
      return (True, True, Pos.X, Pos.Y);
   end Bottom_Right_Size;

   --------------------------
   -- Check_Argument_Count --
   --------------------------

   procedure Check_Argument_Count
     (Function_Name  : String;
      Arguments      : Array_Of_Element_Values;
      Required_Count : Natural)
   is
   begin
      if Arguments'Length /= Required_Count then
         raise Constraint_Error with
           "Xi: warning: function " & Function_Name
           & ": require" & Natural'Image (Required_Count)
           & " argument" & (if Required_Count = 1 then "" else "s")
           & " but received" & Natural'Image (Arguments'Length);
      end if;
   end Check_Argument_Count;

   ----------------
   -- Check_Rule --
   ----------------

   procedure Check_Rule
     (Element : in out Css_Element_Interface'Class;
      Current : in out Applied_Style_Map.Map'Class;
      Rule    : Rule_Entry)
   is
   begin
      for Selector of Rule.Selector.Selectors loop
         if Match (Element, Selector) then
            Apply_Rule (Current, Selector_Specificity (Selector),
                        Rule.Styles);
         end if;
      end loop;
   end Check_Rule;

   -----------------------------
   -- Child_Elements_With_Tag --
   -----------------------------

   function Child_Elements_With_Tag
     (Element : Css_Element_Interface'Class;
      Tag     : String)
      return Array_Of_Elements
   is
      Result : Array_Of_Elements := Element.Child_Elements;
      Count  : Natural := Result'First - 1;
   begin
      for I in Result'Range loop
         if Result (I).Tag = Tag then
            Count := Count + 1;
            Result (Count) := Result (I);
         end if;
      end loop;
      return Result (Result'First .. Count);
   end Child_Elements_With_Tag;

   -------------------------------
   -- Create_Initial_Evaluators --
   -------------------------------

   procedure Create_Initial_Evaluators is
   begin
      Evaluator_Map.Insert ("()", null);
      Evaluator_Map.Insert ("alpha", Evaluate_Alpha'Access);
      Evaluator_Map.Insert ("rgb", Evaluate_Rgb'Access);
      Evaluator_Map.Insert ("rgba", Evaluate_Rgba'Access);
      Evaluator_Map.Insert ("shade", Evaluate_Shade'Access);
      Evaluator_Map.Insert ("url", Evaluate_Url'Access);
      Have_Initial_Evaluators := True;
   end Create_Initial_Evaluators;

   ------------------
   -- Create_Style --
   ------------------

   overriding procedure Create_Style
     (Map    : in out Css_Style_Map;
      Name   : String)
   is
   begin
      Map.Map.Insert (Name, null);
   end Create_Style;

   -------------------------
   -- Current_Style_Sheet --
   -------------------------

   function Current_Style_Sheet
     return Style_Sheet
   is
   begin
      if Local_Current_Style_Sheet = null then
         Local_Current_Style_Sheet := new Style_Sheet_Record;
      end if;
      return Local_Current_Style_Sheet;
   end Current_Style_Sheet;

   -------------------------
   -- Default_Style_Value --
   -------------------------

   function Default_Style_Value
     (Style_Name : String)
      return Css_Element_Value
   is
   begin
      if Default_Style_Value_Map.Is_Empty then
         Default_Style_Value_Map.Insert
           ("display", New_Value ("inline"));
      end if;

      if Default_Style_Value_Map.Contains (Style_Name) then
         return Default_Style_Value_Map.Element (Style_Name);
      else
         return null;
      end if;
   end Default_Style_Value;

   ------------------
   -- Define_Value --
   ------------------

   procedure Define_Value (Name : String;
                           Value : Css_Element_Value)
   is
   begin
      Definitions.Insert (Name, Value);
   end Define_Value;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Context : Layout_Interface'Class;
      Value   : Css_Element_Value)
      return Css_Element_Value
   is
   begin
      if not Have_Initial_Evaluators then
         Create_Initial_Evaluators;
      end if;

      if Value = null then
         return null;
      end if;

      case Value.Value_Type is
         when String_Value =>
            return Value;
         when Reference_Value =>
            declare
               Name : constant String :=
                        Ada.Strings.Unbounded.To_String
                          (Value.Val_Ref_Name);
            begin
               if Definitions.Contains (Name) then
                  return Context.Evaluate
                    (Definitions.Element (Name));
               else
                  raise Constraint_Error with "undefined: " & Name;
                  return New_Value (Name);
               end if;
            end;

         when Function_Value =>
            declare
               use Evaluator_Maps;
               Position : constant Cursor :=
                            Evaluator_Map.Find
                              (Ada.Strings.Unbounded.To_String
                                 (Value.Val_Fn_Name));
            begin
               if Has_Element (Position) then
                  declare
                     Eval_Fn   : constant Evaluator := Element (Position);
                     Eval_Args : Array_Of_Element_Values
                       (1 .. Value.Val_Args.Last_Index);
                  begin
                     if Eval_Fn = null then
                        return Value;
                     else
                        for I in Eval_Args'Range loop
                           Eval_Args (I) :=
                             Context.Evaluate (Value.Val_Args (I));
                        end loop;
                        return Eval_Fn (Context, Eval_Args);
                     end if;
                  end;
               else
                  return Value;
               end if;
            end;
      end case;
   end Evaluate;

   --------------------
   -- Evaluate_Alpha --
   --------------------

   function Evaluate_Alpha
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value
   is
   begin
      Check_Argument_Count ("alpha", Args, 2);

      declare
         Color : Css_Color :=
                   To_Color (Context.Evaluate (Args (Args'First)));
         Alpha : constant Float :=
                   Float'Value
                     (To_String (Context.Evaluate (Args (Args'First + 1))));
      begin
         Color.Alpha := Css_Color_Intensity (Alpha * 255.0);
         return To_Css_Value (Color);
      end;
   end Evaluate_Alpha;

   ------------------
   -- Evaluate_Rgb --
   ------------------

   function Evaluate_Rgb
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value
   is
      pragma Unreferenced (Context);
   begin
      Check_Argument_Count ("rgb", Args, 3);

      return To_Css_Value
        ((Css_Color_Intensity'Value (To_String (Args (1))),
         Css_Color_Intensity'Value (To_String (Args (2))),
         Css_Color_Intensity'Value (To_String (Args (3))),
         255));
   end Evaluate_Rgb;

   -------------------
   -- Evaluate_Rgba --
   -------------------

   function Evaluate_Rgba
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value
   is
      pragma Unreferenced (Context);
   begin
      Check_Argument_Count ("rgb", Args, 4);

      return To_Css_Value
        ((Css_Color_Intensity'Value (To_String (Args (1))),
         Css_Color_Intensity'Value (To_String (Args (2))),
         Css_Color_Intensity'Value (To_String (Args (3))),
         Css_Color_Intensity'Value (To_String (Args (4)))));
   end Evaluate_Rgba;

   --------------------
   -- Evaluate_Shade --
   --------------------

   function Evaluate_Shade
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value
   is
      pragma Unreferenced (Context);

      procedure Shade
        (X : in out Css_Color_Intensity;
         Factor : Float);

      -----------
      -- Shade --
      -----------

      procedure Shade
        (X      : in out Css_Color_Intensity;
         Factor : Float)
      is
         New_X : constant Float := Float (X) * Factor;
      begin
         if New_X < 0.0 then
            X := 0;
         elsif New_X > 255.0 then
            X := 255;
         else
            X := Css_Color_Intensity (New_X);
         end if;
      end Shade;

   begin
      Check_Argument_Count ("shade", Args, 2);

      declare
         Color : Css_Color :=
                   To_Color (Args (Args'First));
         Factor : constant Float :=
                    Float'Value
                      (To_String (Args (Args'First + 1)));
      begin
         Shade (Color.Red, Factor);
         Shade (Color.Green, Factor);
         Shade (Color.Blue, Factor);

         return To_Css_Value (Color);
      end;
   end Evaluate_Shade;

   ------------------
   -- Evaluate_Url --
   ------------------

   function Evaluate_Url
     (Context : Layout_Interface'Class;
      Args    : Array_Of_Element_Values)
      return Css_Element_Value
   is
      pragma Unreferenced (Context);
   begin
      Check_Argument_Count ("url", Args, 1);
      return Args (Args'First);
   end Evaluate_Url;

   -------------------
   -- Function_Name --
   -------------------

   function Function_Name (Value : Css_Element_Value) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Value.Val_Fn_Name);
   end Function_Name;

   ---------------
   -- Has_Style --
   ---------------

   function Has_Style
     (Element : Css_Element_Interface'Class;
      Name    : String)
      return Boolean
   is
   begin
      return Element.Style (Name) /= null
        or else (Is_Inherited (Name)
                 and then Element.Parent_Element /= null
                 and then Element.Parent_Element.Has_Style (Name));
   end Has_Style;

   -----------
   -- Image --
   -----------

   function Image (Position : Layout_Position) return String is
   begin
      return "left:" & Integer'Image (Integer (Position.X))
        & "; top:" & Integer'Image (Integer (Position.Y))
        & ";";
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Size : Layout_Size) return String is
   begin
      return (if Size.Constrained_Width
              then "width:" & Integer'Image (Integer (Size.Width)) & ";"
              else "")
        & (if Size.Constrained_Height
           then "height:" & Integer'Image (Integer (Size.Height)) & ";"
           else "");
   end Image;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function (Value : Css_Element_Value) return Boolean is
   begin
      return Value.Value_Type = Function_Value;
   end Is_Function;

   ------------------
   -- Is_Inherited --
   ------------------

   function Is_Inherited
     (Style_Name : String)
      return Boolean
   is
   begin
      if Inherited_Style.Is_Empty then
         Load_Inherited_Style_Set;
      end if;
      return Inherited_Style.Contains (Style_Name);
   end Is_Inherited;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (Value : Css_Element_Value) return Boolean is
   begin
      return Value.Value_Type = String_Value;
   end Is_String;

   --------------
   -- Is_Tuple --
   --------------

   function Is_Tuple (Value : Css_Element_Value) return Boolean is
   begin
      return Is_Function (Value) and then Function_Name (Value) = "()";
   end Is_Tuple;

   ------------------------------
   -- Load_Inherited_Style_Set --
   ------------------------------

   procedure Load_Inherited_Style_Set is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, Css.Paths.Config_File ("inherited-styles.txt"));
      while not End_Of_File (File) loop
         declare
            Style_Name : constant String := Get_Line (File);
         begin
            Inherited_Style.Insert (Style_Name);
         end;
      end loop;
      Close (File);
   end Load_Inherited_Style_Set;

   ----------------------
   -- Load_Style_Rules --
   ----------------------

   procedure Load_Style_Rules
     (Style_Sheet : Style_Sheet_Record'Class;
      Element     : in out Css_Element_Interface'Class)
   is
      Styles : Applied_Style_Map.Map;
   begin
      for Rule of Style_Sheet.Rules loop
         Check_Rule (Element, Styles, Rule);
      end loop;

      if Element.Inline_Style_Rules /= null then
         Apply_Rule
           (Current          => Styles,
            Rule_Specificity =>
              Selector_Class_Specificity (Inline_Style),
            Rule             => Element.Inline_Style_Rules);
      end if;

      for Style of Styles loop
         declare
            Name  : constant String :=
                      Ada.Strings.Unbounded.To_String (Style.Style.Name);
            Value : constant Css_Element_Value :=
                      Element.Evaluate (Style.Style.Value);
         begin
            Element.Set_Style (Name, "", Value);
         end;
      end loop;

   end Load_Style_Rules;

   ---------
   -- Log --
   ---------

   procedure Log
     (Element : Css_Element_Interface'Class;
      Message : String)
   is
   begin
      Css.Logging.Log
        (Element, Message);
   end Log;

   -------------------
   -- Margin_Pixels --
   -------------------

   function Margin_Pixels
     (Element : Css_Element_Interface'Class;
      Side    : Css_Side)
      return Float
   is
      Specific_Name : constant String :=
                        "margin-" & To_Lower (Css_Side'Image (Side));
   begin
      if Element.Has_Style (Specific_Name) then
         return Element.Measure (Element.Style (Specific_Name), Side);
      elsif Element.Has_Style ("margin") then
         return Element.Measure (Element.Style ("margin"), Side);
      else
         return 0.0;
      end if;
   end Margin_Pixels;

   -----------
   -- Match --
   -----------

   function Match
     (Element  : Css_Element_Interface'Class;
      Selector : Selector_Element)
      return Boolean
   is
      Result : Boolean;
      Text   : constant String :=
                 Ada.Strings.Unbounded.To_String (Selector.Text);

   begin
      case Selector.Class is
         when Inline_Style =>
            Result := True;
         when Identity =>
            Result := Text = "*" or else Element.Id = Text;
         when Css_Class =>
            Result := Text = "*" or else Element.Match_Class (Text);
         when Attribute =>
            Result := True;
         when Element_Tag =>
            Result := Text = "*" or else Element.Match_Tag (Text);
      end case;

      return Result and then
        Match_State (Element,
                     Ada.Strings.Unbounded.To_String (Selector.State),
                     Selector.State_Args);

   end Match;

   -----------------
   -- Match_Class --
   -----------------

   function Match_Class
     (Element   : Css_Element_Interface'Class;
      Css_Class : String)
      return Boolean
   is
      use Ada.Characters.Handling;
      Match : Boolean := False;

      procedure Check (Class_Name : String);

      -----------
      -- Check --
      -----------

      procedure Check (Class_Name : String) is
      begin
         if To_Lower (Class_Name) = Css_Class then
            Match := True;
         end if;
      end Check;

      Classes : constant String := Element.Classes & " ";
      Start   : Positive := Classes'First;
      Space   : Natural;
   begin
      loop
         Space := Ada.Strings.Fixed.Index (Classes, " ", Start);
         exit when Space = 0;
         if Space > Start then
            Check (Classes (Start .. Space - 1));
            exit when Match;
         end if;
         Start := Space + 1;
      end loop;
      return Match;
   end Match_Class;

   -----------------
   -- Match_State --
   -----------------

   function Match_State
     (Element    : Css_Element_Interface'Class;
      State_Name : String;
      State_Args : Element_Value_Vectors.Vector)
      return Boolean
   is
   begin
      if State_Name = "" then
         return True;
      elsif State_Name = "nth-child" then
         declare
            Argument : constant String :=
                         To_String (State_Args.First_Element);
            Index    : constant Natural := Element.Child_Index;
         begin
            if Index = 0 then
               return False;
            elsif Argument = "even" then
               return Index mod 2 = 0;
            elsif Argument = "odd" then
               return Index mod 2 /= 0;
            elsif (for all Ch of Argument => Ch in '0' .. '9') then
               return Index = Natural'Value (Argument);
            else
               return False;
            end if;
         end;
      else
         return False;
      end if;
   end Match_State;

   ------------------
   -- Match_String --
   ------------------

   function Match_String
     (Space_Separated_String : String;
      Search_Text            : String)
      return Boolean
   is
      Start : Positive := Space_Separated_String'First;
   begin
      for I in Space_Separated_String'Range loop
         if Space_Separated_String (I) = ' ' then
            if Space_Separated_String (Start .. I - 1) = Search_Text then
               return True;
            end if;
            Start := I + 1;
         end if;
      end loop;
      return False;
   end Match_String;

   ---------------
   -- Match_Tag --
   ---------------

   function Match_Tag
     (Element  : Css_Element_Interface'Class;
      Tag_Name : String)
      return Boolean
   is
      use Ada.Characters.Handling;
   begin
      return To_Lower (Element.Tag) = Tag_Name;
   end Match_Tag;

   -------------
   -- Measure --
   -------------

   function Measure
     (Element : Css_Element_Interface'Class;
      Value   : Css_Element_Value;
      Side    : Css_Side)
      return Float
   is
      Text : constant String :=
               (if Value = null then "0" else To_String (Value));
   begin
      case Side is
         when Left =>
            return Measure (Element.Layout_Width, Text);
         when Top =>
            return Measure (Element.Layout_Height, Text);
         when Right =>
            return Element.Layout_Width
              - Measure (Element.Layout_Width, Text);
         when Bottom =>
            return Element.Layout_Height
              - Measure (Element.Layout_Height, Text);
      end case;
   end Measure;

   -------------
   -- Measure --
   -------------

   function Measure (Context_Size : Float;
                     Measurement  : String)
                     return Float
   is
      use Ada.Characters.Handling;
      Index : Positive := Measurement'First;
   begin
      while Index <= Measurement'Length
        and then Is_Digit (Measurement (Index))
      loop
         Index := Index + 1;
      end loop;
      if Index = Measurement'First then
         return 0.0;
      elsif Index > Measurement'Last then
         return Float'Value (Measurement);
      elsif To_Lower (Measurement (Index .. Measurement'Last)) = "px" then
         return Float'Value (Measurement (Measurement'First .. Index - 1));
      elsif Measurement (Index .. Measurement'Last) = "%" then
         return Float'Value (Measurement (Measurement'First .. Index - 1))
           * Context_Size / 100.0;
      else
         return Float'Value (Measurement);
      end if;
   end Measure;

   -------------
   -- Measure --
   -------------

   function Measure
     (Element   : Css_Element_Interface'Class;
      Value     : Css_Element_Value;
      Available : Float)
      return Float
   is
   begin
      if Value = null then
         return 0.0;
      else
         return Measure (Available, To_String (Element.Evaluate (Value)));
      end if;
   end Measure;

   ----------------------
   -- Measure_Position --
   ----------------------

   function Measure_Position
     (Parent  : Css_Element_Interface'Class;
      Element : not null access Css_Element_Interface'Class;
      Side    : Css_Side)
      return Float
   is
      Style_Name : constant String :=
                     Ada.Characters.Handling.To_Lower
                       (Css_Side'Image (Side));
      Value      : constant Css_Element_Value :=
                     Element.Style (Style_Name);
   begin
      return Parent.Measure (Element.Evaluate (Value), Side);
   end Measure_Position;

   -----------
   -- Merge --
   -----------

   function Merge (Left, Right : Layout_Size) return Layout_Size is
   begin
      return (Left.Constrained_Width or else Right.Constrained_Width,
              Left.Constrained_Height or else Right.Constrained_Height,
              (if Left.Constrained_Width
               then (if Right.Constrained_Width
                 then Float'Max (Left.Width, Right.Width)
                 else Left.Width)
               elsif Right.Constrained_Width
               then Right.Width
               else 0.0),
              (if Left.Constrained_Height
               then (if Right.Constrained_Height
                 then Float'Max (Left.Height, Right.Height)
                 else Left.Height)
               elsif Right.Constrained_Height
               then Right.Height
               else 0.0));
   end Merge;

   ---------------
   -- New_Value --
   ---------------

   function New_Value
     (From_String : String)
      return Css_Element_Value
   is
   begin
      return Allocate
        ((String_Value,
         Ada.Strings.Unbounded.To_Unbounded_String (From_String)));
   end New_Value;

   ---------------
   -- New_Value --
   ---------------

   function New_Value
     (Function_Name : String;
      Arguments     : Array_Of_Element_Values)
      return Css_Element_Value
   is
   begin
      return Value : constant Css_Element_Value :=
        Allocate ((Function_Value,
                  Ada.Strings.Unbounded.To_Unbounded_String
                    (Function_Name),
                  Element_Value_Vectors.Empty_Vector))
      do
         for Argument of Arguments loop
            Value.Val_Args.Append (Argument);
         end loop;
      end return;
   end New_Value;

   -----------------
   -- No_Elements --
   -----------------

   function No_Elements return Array_Of_Elements is
   begin
      return Result : Array_Of_Elements (1 .. 0) do
         null;
      end return;
   end No_Elements;

   ------------------
   -- Null_Context --
   ------------------

   function Null_Context return Layout_Interface'Class is
      Result : Null_Evaluation_Context;
   begin
      return Result;
   end Null_Context;

   --------------------
   -- Padding_Pixels --
   --------------------

   function Padding_Pixels
     (Element : Css_Element_Interface'Class;
      Side    : Css_Side)
      return Float
   is
      Specific_Name : constant String :=
                        "padding-" & To_Lower (Css_Side'Image (Side));
   begin
      if Element.Has_Style (Specific_Name) then
         return Element.Measure (Element.Style (Specific_Name), Side);
      elsif Element.Has_Style ("padding") then
         return Element.Measure (Element.Style ("padding"), Side);
      else
         return 0.0;
      end if;
   end Padding_Pixels;

   ----------------------
   -- Parse_Html_Color --
   ----------------------

   function Parse_Html_Color
     (Text : String)
      return Css_Color
   is
      function To_Color_Intensity (X : String) return Css_Color_Intensity
      is (Css_Color_Intensity'Value ("16#" & X & "#"));

   begin
      if Text (Text'First) = '#' then
         if Text'Length = 4 then
            return Color : Css_Color do
               Color.Red :=
                 16 * To_Color_Intensity
                   (Text (Text'First + 1 .. Text'First + 1));
               Color.Green :=
                 16 * To_Color_Intensity
                   (Text (Text'First + 2 .. Text'First + 2));
               Color.Blue :=
                 16 * To_Color_Intensity
                   (Text (Text'First + 3 .. Text'First + 3));
            end return;
         else
            return Color : Css_Color do
               Color.Red :=
                 To_Color_Intensity
                   (Text (Text'First + 1 .. Text'First + 2));
               Color.Green :=
                 To_Color_Intensity
                   (Text (Text'First + 3 .. Text'First + 4));
               Color.Blue :=
                 To_Color_Intensity
                   (Text (Text'First + 5 .. Text'First + 6));
               Color.Alpha :=
                 (if Text'Length < 9
                  then 255
                  else To_Color_Intensity
                    (Text (Text'First + 7 .. Text'First + 8)));
            end return;
         end if;
      else
         if Named_Colors.Is_Empty then
            Read_Colors;
         end if;

         if Named_Colors.Contains (Text) then
            return Named_Colors.Element (Text);
         else
            return (0, 0, 0, 255);
         end if;
      end if;

   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "cannot parse color '" & Text & "'";
   end Parse_Html_Color;

   --------------
   -- Position --
   --------------

   function Position
     (Element : Css_Element_Interface'Class)
      return Css_Position
   is
   begin
      if not Element.Has_Style ("position") then
         return Static;
      else
         return Css_Position'Value
           (To_String (Element.Style ("position")));
      end if;
   end Position;

   -----------------------
   -- Positioned_Parent --
   -----------------------

   function Positioned_Parent
     (Element : Css_Element_Interface'Class)
      return Css_Element
   is
   begin
      if Element.Parent_Element.Positioned
        or else Element.Parent_Element.Parent_Element = null
      then
         return Element.Parent_Element;
      else
         return Element.Parent_Element.Positioned_Parent;
      end if;
   end Positioned_Parent;

   -----------------
   -- Read_Colors --
   -----------------

   procedure Read_Colors is
      use Ada.Text_IO;
      File : File_Type;

      function Is_Space (Ch : Character) return Boolean
      is (Ch = ' ' or else Character'Pos (Ch) in 9 | 10 | 13);

   begin

      Named_Colors.Insert ("transparent", (255, 255, 255, 0));

      Open (File, In_File, Css.Paths.Config_Path & "/rgb.txt");
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            if Line (Line'First) = '!' then
               null;
            else
               declare
                  use Ada.Characters.Handling;
                  State     : Natural := 0;
                  R, G, B   : Css_Color_Intensity := 0;
                  X         : Css_Color_Intensity;
                  Start     : Positive := Line'First;
                  At_Number : Boolean := False;
               begin
                  for I in Line'Range loop
                     if not Is_Space (Line (I))
                       and then State >= 3
                     then
                        Named_Colors.Insert (Line (I .. Line'Last),
                                             (R, G, B, 255));
                        exit;
                     end if;

                     if Is_Digit (Line (I)) then
                        if not At_Number then
                           At_Number := True;
                           Start := I;
                        end if;
                     elsif Is_Space (Line (I)) then
                        if At_Number then
                           X :=
                             Css_Color_Intensity'Value
                               (Line (Start .. I - 1));
                           if State = 0 then
                              R := X;
                              State := 1;
                           elsif State = 1 then
                              G := X;
                              State := 2;
                           elsif State = 2 then
                              B := X;
                              State := 3;
                           end if;
                           At_Number := False;
                        end if;
                     end if;
                  end loop;
               end;
            end if;
         end;
      end loop;
      Close (File);

   end Read_Colors;

   ---------------------
   -- Reference_Value --
   ---------------------

   function Reference_Value (Name : String) return Css_Element_Value is
   begin
      return Allocate ((Reference_Value,
                       Ada.Strings.Unbounded.To_Unbounded_String
                         (Name)));
   end Reference_Value;

   --------------------------
   -- Selector_Specificity --
   --------------------------

   function Selector_Specificity
     (Element : Selector_Element)
      return Natural
   is
      use Ada.Strings.Unbounded;
   begin
      if Element.State /= Null_Unbounded_String then
         return Selector_Class_Specificity (Element.Class) + 10;
      else
         return Selector_Class_Specificity (Element.Class);
      end if;
   end Selector_Specificity;

   -----------------------
   -- Set_Layout_Height --
   -----------------------

   procedure Set_Layout_Height
     (Layout : in out Layout_Interface'Class;
      Height : Float)
   is
      Size : Layout_Size := Layout.Get_Layout_Size;
   begin
      Size.Constrained_Height := True;
      Size.Height := Height;
      Layout.Set_Layout_Size (Size);
   end Set_Layout_Height;

   ----------------------
   -- Set_Layout_Width --
   ----------------------

   procedure Set_Layout_Width
     (Layout : in out Layout_Interface'Class;
      Width  : Float)
   is
      Size : Layout_Size := Layout.Get_Layout_Size;
   begin
      Size.Constrained_Width := True;
      Size.Width := Width;
      Layout.Set_Layout_Size (Size);
   end Set_Layout_Width;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Element : in out Css_Element_Interface'Class;
      Name    : String;
      Value   : String)
   is
      Css_Value : constant Css_Element_Value :=
                    New_Value (Value);
   begin
      Element.Set_Style (Name, "", Css_Value);
   end Set_Style;

   ---------------
   -- Set_Style --
   ---------------

   overriding procedure Set_Style
     (Map    : in out Css_Style_Map;
      Name   : String;
      State  : String;
      Value  : Css_Element_Value)
   is
      pragma Unreferenced (State);
   begin
      if Value = null then
         if Map.Map.Contains (Name) then
            Map.Map.Delete (Name);
         end if;
      else
         if Map.Map.Contains (Name) then
            Map.Map.Replace (Name, Value);
         else
            Map.Map.Insert (Name, Value);
         end if;
      end if;
   end Set_Style;

   -----------
   -- Style --
   -----------

   overriding function Style
     (Map  : Css_Style_Map;
      Name : String)
      return Css_Element_Value
   is
   begin
      if Map.Map.Contains (Name) then
         return Map.Map.Element (Name);
      else
         return null;
      end if;
   end Style;

   ---------------------
   -- Style_To_String --
   ---------------------

   function Style_To_String
     (Element    : Css_Element_Interface'Class;
      Style_Name : String)
      return String
   is
      Value : constant Css_Element_Value := Element.Style (Style_Name);
   begin
      if Value = null then
         return To_String (Element.Default_Style_Value (Style_Name));
      elsif Is_String (Value) then
         return To_String (Value);
      else
         return To_String (Element.Evaluate (Value));
      end if;
   end Style_To_String;

   --------------
   -- To_Color --
   --------------

   function To_Color (Value : Css_Element_Value) return Css_Color is
   begin
      return Parse_Html_Color
        (To_String (Value));
   end To_Color;

   ------------------
   -- To_Css_Value --
   ------------------

   function To_Css_Value
     (Color : Css_Color)
      return Css_Element_Value
   is
   begin
      return New_Value
        ('#'
         & To_Hex_String (Natural (Color.Red))
         & To_Hex_String (Natural (Color.Green))
         & To_Hex_String (Natural (Color.Blue))
         & (if Color.Alpha /= 255
           then To_Hex_String (Natural (Color.Alpha))
           else ""));
   end To_Css_Value;

   -------------------
   -- To_Hex_String --
   -------------------

   function To_Hex_String (Value : Natural) return String is

      function To_Hex_Digit (Value : Natural) return Character;

      ------------------
      -- To_Hex_Digit --
      ------------------

      function To_Hex_Digit (Value : Natural) return Character is
      begin
         if Value < 10 then
            return Character'Val (Value + Character'Pos ('0'));
         else
            return Character'Val (Value + Character'Pos ('a') - 10);
         end if;
      end To_Hex_Digit;

   begin
      if Value < 256 then
         return (To_Hex_Digit (Value / 16), To_Hex_Digit (Value mod 16));
      else
         return To_Hex_String (Value / 256) & To_Hex_String (Value mod 256);
      end if;
   end To_Hex_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Css_Element_Value) return String is
   begin
      if Value = null then
         return "null";
      end if;
      case Value.Value_Type is
         when String_Value =>
            return Ada.Strings.Unbounded.To_String (Value.Val_String);
         when Function_Value =>
            if Function_Name (Value) = "()" then
               declare
                  use Ada.Strings.Unbounded;
                  Result : Unbounded_String := Null_Unbounded_String;
               begin
                  for I in 1 .. Value.Val_Args.Last_Index loop
                     Result := Result &
                     (if I = 1 then "" else " ")
                       & To_String (Value.Val_Args (I));
                  end loop;
                  return To_String (Result);
               end;
            else
               declare
                  use Ada.Strings.Unbounded;
                  Result : Unbounded_String := Value.Val_Fn_Name;
               begin
                  for I in 1 .. Value.Val_Args.Last_Index loop
                     Result := Result &
                     (if I = 1 then "(" else ",")
                       & To_String (Value.Val_Args (I));
                  end loop;
                  return To_String (Result) & ")";
               end;
            end if;
         when Reference_Value =>
            return "@" & Ada.Strings.Unbounded.To_String (Value.Val_Ref_Name);
      end case;
   end To_String;

   ---------------
   -- To_String --
   ---------------

--     function To_String (Measurement : Css_Measurement) return String is
--        Value : constant String := Xi.Float_Images.Image (Measurement.Value);
--     begin
--        case Measurement.Unit is
--           when No_Measurement =>
--              return "";
--           when Percentage =>
--              return Value & "%";
--           when Pixels =>
--              return Value & "px";
--        end case;
--     end To_String;

   -----------------
   -- Top_Element --
   -----------------

   function Top_Element
     (Element : Css_Element_Interface'Class)
      return Css_Element
   is
      It : Css_Element := Element.Parent_Element;
   begin
      while It.Parent_Element /= null loop
         It := It.Parent_Element;
      end loop;
      return It;
   end Top_Element;

end Css;
