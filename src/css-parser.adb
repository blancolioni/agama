with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;

with Css.Parser.Tokens;     use Css.Parser.Tokens;
with Css.Parser.Lexical;    use Css.Parser.Lexical;

package body Css.Parser is

   function "+" (Value : String) return Unbounded_String
                 renames To_Unbounded_String;

   function At_Property_Value return Boolean;
   function Parse_Property_Value return Css_Element_Value;
   function Parse_Atomic_Property_Value return Css_Element_Value;

   function Parse_Function_Arguments
      return Array_Of_Element_Values;

   procedure Parse_Rule;
   function Parse_Selector_Rule return Css_Rule;

   procedure Parse_Meta_Rule
   with Pre => Tok = Tok_Identifier;

   procedure Parse_Error (Message : String);

   -----------------------
   -- At_Property_Value --
   -----------------------

   function At_Property_Value return Boolean is
   begin
      return Tok = Tok_Identifier or else Tok = Tok_String_Constant
        or else Tok = Tok_At or else Tok = Tok_Hash;
   end At_Property_Value;

   -----------------------
   -- Evaluate_Function --
   -----------------------

--     function Evaluate_Function
--       (Name : Unbounded_String)
--        return Unbounded_String
--     is
--        function Parse_Arguments return Array_Of_Element_Values;
--
--        function Evaluate_Rgb (Args : Arguments) return Unbounded_String;
--        function Evaluate_Rgba (Args : Arguments) return Unbounded_String;
--        function Evaluate_Shade (Args : Arguments) return Unbounded_String;
--        function Evaluate_Url (Args : Arguments) return Unbounded_String;
--
--        ------------------
--        -- Evaluate_Rgb --
--        ------------------
--
--        function Evaluate_Rgb (Args : Arguments) return Unbounded_String is
--        begin
--           if Args'Length /= 3 then
--              Parse_Error ("rgb requires 3 arguments");
--              return Null_Unbounded_String;
--           end if;
--
--           return '#' & To_Hex_String (Args);
--        end Evaluate_Rgb;
--
--        -------------------
--        -- Evaluate_Rgba --
--        -------------------
--
--        function Evaluate_Rgba (Args : Arguments) return Unbounded_String is
--        begin
--           if Args'Length /= 4 then
--              Parse_Error ("rgba requires 3 arguments");
--              return Null_Unbounded_String;
--           end if;
--
--           return '#' & To_Hex_String (Args);
--        end Evaluate_Rgba;
--
--        --------------------
--        -- Evaluate_Shade --
--        --------------------
--
--        function Evaluate_Shade (Args : Arguments) return Unbounded_String is
--        begin
--           if Args'Length /= 2 then
--              Parse_Error ("shade requires 2 arguments");
--              return Null_Unbounded_String;
--           end if;
--
--           declare
--              use type Xi.Xi_Float;
--              Color : Xi.Color.Xi_Color :=
--                         Xi.Color.Parser.Parse_Html_Color
--                           (To_String (Args (Args'First)));
--              Factor : constant Xi.Xi_Float :=
--                         Xi.Xi_Float'Value
--                           (To_String (Args (Args'First + 1)));
--           begin
--              Color := Xi.Color.Shade (Color, Factor);
--
--              return '#'
--                & To_Hex_String (Natural (Color.Red * 255.0))
--                & To_Hex_String (Natural (Color.Green * 255.0))
--                & To_Hex_String (Natural (Color.Blue * 255.0))
--                & To_Unbounded_String
--                (((if Color.Alpha /= 1.0
--                 then To_Hex_String (Natural (Color.Alpha * 255.0))
--                 else "")));
--           end;
--        end Evaluate_Shade;
--
--        ------------------
--        -- Evaluate_Url --
--        ------------------
--
--        function Evaluate_Url (Args : Arguments) return Unbounded_String is
--        begin
--           if Args'Length /= 1 then
--              Parse_Error ("url requires 1 argument");
--              return Null_Unbounded_String;
--           end if;
--           return Args (Args'First);
--        end Evaluate_Url;
--
--        ---------------------
--        -- Parse_Arguments --
--        ---------------------
--
--        function Parse_Arguments return Arguments is
--        begin
--           if At_Property_Value then
--              declare
--                 Result : constant Css_Element_Value := Parse_Property_Value;
--              begin
--                 if Tok = Tok_Comma then
--                    Scan;
--                    return To_Unbounded_String (Result) & Parse_Arguments;
--                 elsif Tok = Tok_Right_Paren then
--                    Scan;
--                    return (1 => To_Unbounded_String (Result));
--                 else
--                    Parse_Error ("expected ',' or ')'");
--                    return (1 => To_Unbounded_String (Result));
--                 end if;
--              end;
--           else
--              declare
--                 Result : Arguments (1 .. 0);
--              begin
--                 if Tok = Tok_Right_Paren then
--                    Scan;
--                 else
--                    Parse_Error ("missing argument");
--                 end if;
--                 return Result;
--              end;
--           end if;
--        end Parse_Arguments;
--
--        -------------------
--        -- To_Hex_String --
--        -------------------
--
--        function To_Hex_String (Args : Arguments) return Unbounded_String is
--           X      : Integer;
--           Result : Unbounded_String;
--        begin
--           for I in Args'Range loop
--              declare
--                 Arg : constant String := To_String (Args (I));
--              begin
--                 X := Integer'Value (Arg);
--                 if X not in 0 .. 255 then
--                    Parse_Error ("out of range: " & Arg);
--                 end if;
--                 Result := Result & To_Unbounded_String (To_Hex_String (X));
--              exception
--                 when Constraint_Error =>
--                    Parse_Error (Arg & ": bad rgb value");
--              end;
--           end loop;
--
--           return Result;
--        end To_Hex_String;
--
--     begin
--        Scan; --  '('
--
--        declare
--           Args : constant Arguments := Parse_Arguments;
--        begin
--           if Name = "rgb" then
--              return Evaluate_Rgb (Args);
--           elsif Name = "rgba" then
--              return Evaluate_Rgba (Args);
--           elsif Name = "shade" then
--              return Evaluate_Shade (Args);
--           elsif Name = "url" then
--              return Evaluate_Url (Args);
--           else
--              Warning ("unknown function: " & To_String (Name));
--              declare
--                 Result : Unbounded_String := Name;
--              begin
--                 for I in Args'Range loop
--                    Result := Result & (if I = Args'First then "(" else ",")
--                      & Args (I);
--                 end loop;
--                 Result := Result & ")";
--                 return Result;
--              end;
--           end if;
--        end;
--
--     end Evaluate_Function;

   -------------------
   -- Load_Css_File --
   -------------------

   procedure Load_Css_File
     (Path : String)
   is
   begin
      Open (Path);
      while Tok /= Tok_End_Of_File loop
         Parse_Rule;
      end loop;
      Close;
   end Load_Css_File;

   ---------------------------------
   -- Parse_Atomic_Property_Value --
   ---------------------------------

   function Parse_Atomic_Property_Value return Css_Element_Value is
      Result : Css_Element_Value;
   begin
      if Tok = Tok_Hash then
         Scan;
         if Tok = Tok_Identifier then
            Result :=
              New_Value ("#" & Tok_Text);
            Scan;
         else
            Parse_Error ("missing color");
         end if;
      elsif Tok = Tok_At then
         Scan;
         if Tok = Tok_Identifier then
            Result := Reference_Value (Tok_Text);
            Scan;
         else
            Parse_Error ("missing name");
         end if;
      elsif Tok = Tok_String_Constant then
         Result := New_Value (Tok_Text);
         Scan;
      elsif Tok = Tok_Identifier then
         if Next_Tok = Tok_Left_Paren then
            declare
               Fn_Name : constant String := Tok_Text;
            begin
               Scan;
               Scan;
               declare
                  Args : constant Array_Of_Element_Values :=
                           Parse_Function_Arguments;
               begin
                  Result := New_Value (Fn_Name, Args);
               end;
            end;
         else
            Result := New_Value (Tok_Text);
            Scan;
         end if;
      else
         Parse_Error ("missing property value");
      end if;
      return Result;
   end Parse_Atomic_Property_Value;

   -----------------
   -- Parse_Error --
   -----------------

   procedure Parse_Error (Message : String) is
   begin
      Error (Message);
      raise Constraint_Error with Message;
   end Parse_Error;

   ------------------------------
   -- Parse_Function_Arguments --
   ------------------------------

   function Parse_Function_Arguments
     return Array_Of_Element_Values
   is
   begin
      if At_Property_Value then
         declare
            Item : constant Css_Element_Value := Parse_Property_Value;
         begin
            if Tok = Tok_Comma then
               Scan;
               return Item & Parse_Function_Arguments;
            else
               if Tok = Tok_Right_Paren then
                  Scan;
               else
                  Parse_Error ("missing ')'");
               end if;
               return (1 => Item);
            end if;
         end;
      else
         declare
            Result : Array_Of_Element_Values (1 .. 0);
         begin
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Parse_Error ("missing ')'");
            end if;
            return Result;
         end;
      end if;
   end Parse_Function_Arguments;

   ------------------------
   -- Parse_Inline_Style --
   ------------------------

   function Parse_Inline_Style
     (Inline_Style : String)
      return Css_Rule
   is
   begin
      Open_String (Inline_Style);
      return Rule : constant Css_Rule := Parse_Selector_Rule do
         Close;
      end return;
   end Parse_Inline_Style;

   ---------------------
   -- Parse_Meta_Rule --
   ---------------------

   procedure Parse_Meta_Rule is
   begin
      if Tok_Text = "define-color" then
         Scan;
         if Tok = Tok_Identifier then
            declare
               Name : constant String := Tok_Text;
            begin
               Scan;
               Define_Value (Name, Parse_Property_Value);
            end;
         else
            Parse_Error ("missing identifier");
         end if;
      elsif Tok_Text = "import" then
         Scan;
         declare
            Value : constant Css_Element_Value :=
                      Parse_Property_Value;
            Source : constant String :=
                       To_String
                         (Null_Context.Evaluate (Value));
         begin
            Load_Css_File (Source);
         end;
      else
         Parse_Error ("unknown meta rule: " & Tok_Text);
      end if;
      if Tok = Tok_Semicolon then
         Scan;
      end if;
   end Parse_Meta_Rule;

   --------------------------
   -- Parse_Property_Value --
   --------------------------

   function Parse_Property_Value return Css_Element_Value is
      Result : Css_Element_Value := Parse_Atomic_Property_Value;
   begin
      if At_Property_Value then
         declare
            Fn_Name : constant String := "()";
            Args    : Array_Of_Element_Values (1 .. 10);
            Index   : Positive := 1;
            Item    : Css_Element_Value := Result;
         begin
            loop
               Args (Index) := Item;
               exit when not At_Property_Value;
               Index := Index + 1;
               Item := Parse_Atomic_Property_Value;
            end loop;

            Result := New_Value (Fn_Name, Args (1 .. Index));
         end;
      end if;
      return Result;
   end Parse_Property_Value;

   ----------------
   -- Parse_Rule --
   ----------------

   procedure Parse_Rule is
      Selector : Css_Selector;
   begin
      if Tok = Tok_At then
         Scan;
         Parse_Meta_Rule;
      elsif Tok = Tok_Identifier
        or else Tok = Tok_Asterisk
        or else Tok = Tok_Dot
        or else Tok = Tok_Hash
      then

         Selector := new Css_Selector_Record;

         while Tok /= Tok_End_Of_File and then Tok /= Tok_Left_Brace loop
            declare
               Element : Selector_Element;
            begin
               if Tok = Tok_Identifier
                 or else Tok = Tok_Asterisk
               then
                  Element.Class := Element_Tag;
                  Element.Text := +Tok_Text;
                  Scan;
               elsif Tok = Tok_Dot then
                  Scan;
                  if Tok = Tok_Identifier then
                     Element.Class := Css_Class;
                     Element.Text := +Tok_Text;
                     Scan;
                  else
                     Parse_Error ("missing identifier");
                  end if;
               elsif Tok = Tok_Hash then
                  Scan;
                  if Tok = Tok_Identifier then
                     Element.Class := Identity;
                     Element.Text := +Tok_Text;
                     Scan;
                  else
                     Parse_Error ("missing identifier");
                  end if;
               else
                  Parse_Error ("expected an identifier");
               end if;
               if Tok = Tok_Colon then
                  Scan;
                  if Tok = Tok_Identifier then
                     Element.State := +Tok_Text;
                     Scan;
                     if Tok = Tok_Left_Paren then
                        Scan;
                        declare
                           Args : constant Array_Of_Element_Values :=
                                    Parse_Function_Arguments;
                        begin
                           for Arg of Args loop
                              Element.State_Args.Append (Arg);
                           end loop;
                        end;
                     end if;
                  end if;
               end if;

               Selector.Selectors.Append (Element);
            end;
            if Tok = Tok_Comma then
               Scan;
               if Tok = Tok_End_Of_File or else Tok = Tok_Left_Brace then
                  Parse_Error ("missing selector");
               end if;
            elsif Tok /= Tok_Left_Brace then
               Parse_Error ("missing '{'");
            end if;
         end loop;

         if Tok /= Tok_Left_Brace then
            Parse_Error ("missing rule");
         end if;

         Scan;

         declare
            Rule : constant Css_Rule := Parse_Selector_Rule;
         begin

            if Tok /= Tok_Right_Brace then
               Parse_Error ("missing '}'");
            else
               Scan;
            end if;

            Current_Style_Sheet.Append_Rule (Selector, Rule);

         end;
      else
         Parse_Error ("missing rule");
      end if;
   end Parse_Rule;

   -------------------------
   -- Parse_Selector_Rule --
   -------------------------

   function Parse_Selector_Rule return Css_Rule is
      Rule : constant Css_Rule := new Css_Rule_Record;
   begin

      while Tok = Tok_Identifier loop
         declare
            Property_Name : constant String := Tok_Text;
         begin
            Scan;
            if Tok = Tok_Colon then
               Scan;
            else
               Parse_Error ("missing ':'");
            end if;

            declare
               Property_Value : constant Css_Element_Value :=
                                  Parse_Property_Value;
            begin
               Rule.Styles.Append
                 ((+Property_Name, Property_Value));
               if Tok = Tok_Semicolon then
                  Scan;
               end if;
            end;
         end;
      end loop;

      return Rule;
   end Parse_Selector_Rule;

end Css.Parser;
