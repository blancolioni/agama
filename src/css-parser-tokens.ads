private package Css.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_String_Constant,

       Tok_No_Reserved_Identifiers,

       Tok_Left_Brace, Tok_Right_Brace,
       Tok_Left_Paren, Tok_Right_Paren,
       Tok_Comma, Tok_Colon, Tok_Semicolon,
       Tok_Dot, Tok_Ampersand, Tok_Forward_Slash,
       Tok_Asterisk, Tok_Hash, Tok_At
      );

end Css.Parser.Tokens;
