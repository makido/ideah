package haskell.lexer;

import com.intellij.psi.tree.TokenSet;

public interface HaskellTokenTypes {

    HaskellTokenType ML_COMMENT = new HaskellTokenType("block comment");
    HaskellTokenType COMMENT = new HaskellTokenType("line comment");
    HaskellTokenType KEYWORD = new HaskellTokenType("keyword");
    HaskellTokenType KEY_OP = new HaskellTokenType("key operator");
    HaskellTokenType L_PAREN = new HaskellTokenType("left paren");
    HaskellTokenType R_PAREN = new HaskellTokenType("right paren");
    HaskellTokenType L_SQUARE = new HaskellTokenType("left bracket");
    HaskellTokenType R_SQUARE = new HaskellTokenType("right bracket");
    HaskellTokenType L_CURLY = new HaskellTokenType("left brace");
    HaskellTokenType R_CURLY = new HaskellTokenType("right brace");
    HaskellTokenType COMMA = new HaskellTokenType("comma");
    HaskellTokenType VAR_ID = new HaskellTokenType("var");
    HaskellTokenType CON_ID = new HaskellTokenType("Constructor");
    HaskellTokenType VAR_SYM = new HaskellTokenType("operator");
    HaskellTokenType CON_SYM = new HaskellTokenType(":operator");
    HaskellTokenType INTEGER = new HaskellTokenType("integer");
    HaskellTokenType FLOAT = new HaskellTokenType("float");
    HaskellTokenType OCTAL = new HaskellTokenType("octal");
    HaskellTokenType HEX = new HaskellTokenType("hex");
    HaskellTokenType WHITESPACE = new HaskellTokenType("white space");
    HaskellTokenType ERROR_STRING = new HaskellTokenType("wrong string");
    HaskellTokenType ERROR_NUMBER = new HaskellTokenType("wrong number");
    HaskellTokenType ERROR_UNDEFINED = new HaskellTokenType("wrong token");
    HaskellTokenType SPECIAL = new HaskellTokenType("special");
    HaskellTokenType STRING = new HaskellTokenType("string");
    HaskellTokenType CHAR = new HaskellTokenType("char");

    TokenSet COMMENTS = TokenSet.create(COMMENT, ML_COMMENT);
    TokenSet WHITESPACES = TokenSet.create(WHITESPACE);
    TokenSet STRINGS = TokenSet.create(STRING, ERROR_STRING);
}
