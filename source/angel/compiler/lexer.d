module angel.compiler.lexer;

import std.ascii;

public enum TokenType {
    Identifier,

    /** PLEASE DON'T USE 'assert' and 'print' IN REAL APPLICATIONS!
    they are specifically to test VM features in DEBUG MODE, not for real world applications.
    the implementation of this keyword is ****NOT**** present in PUBLIC releases of 
    AngelJS, and it will result in terrible exceptions.
    */
    Keyword_Assert,
    Keyword_Print,

    Keyword_Let,
    Keyword_True,
    Keyword_False,
    Keyword_Null,
    Keyword_If,
    Keyword_Else,
    Keyword_While,
    Keyword_For,

    String,
    Float, Integer,
    
    Plus, Minus, Divide, Multiply, Not,

    Equal, EqualEqual, NotEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    And, Or,

    DoubleDot, Dot, Semicolon,

    LeftParen, RightParen,
    LeftBrace, RightBrace,

    EndOfFile,
}

public struct Token {
    int line, position;
    string text;
    TokenType type;
}

public class Lexer {

    public string original;
    public char current;
    public int position;
    public int line;

    public this(string charArray) {
        this.position = 0;
        this.line = 1;
        this.original =  charArray;

        this.Next();
    }

    public char Next() {
        char last = current;

        if (position >= this.original.length) {
            current = '\0';
        } else {
            current = this.original[this.position++];
            if (current == '\n') {
                this.line++;
            }
        }

        return last;
    }

    public Token NextToken() {
        while ((isWhite(current) || current == '#') && current != '\0') {
            if (current == '#') {
                while (current != '\n' && current != '\0') {
                    Next();
                }
            }
            Next();
        }

        if (isDigit(current)) {
            string acc = "";
            TokenType literalType = TokenType.Integer;

            while ((isDigit(current) || current == '.' || current == 'f') && current != '\0') {
                if (current == '.') {
                    literalType = TokenType.Float;
                } else if (current == 'f'){
                    Next();
                    break;
                }

                acc ~= Next();
            }

            return Token(line, position, acc, literalType);
        } else if (isAlpha(current) || current == '_') {
            string acc = "";

            while ((isAlpha(current) || current == '_') && current != '\0') {
                acc ~= Next();
            }

            return Token(line,position,acc,GetTypeForStr(acc));
        } else if (current == '"') {
            string acc = "";
            // skip '"'
            Next();

            while (current != '\0' && current != '\"') {
                acc ~= Next();
            }

            Next();

            return Token(line, position, acc, TokenType.String);
        }
    
        mixin(SingleOperator!(':', "DoubleDot"));
        mixin(SingleOperator!(';', "Semicolon"));
        mixin(SingleOperator!('.', "Dot"));

        mixin(SingleOperator!('+', "Plus"));
        mixin(SingleOperator!('-', "Minus"));
        mixin(SingleOperator!('*', "Multiply"));
        mixin(SingleOperator!('/', "Divide"));
        mixin(SingleOperator!('(', "LeftParen"));
        mixin(SingleOperator!(')', "RightParen"));
        mixin(SingleOperator!('{', "LeftBrace"));
        mixin(SingleOperator!('}', "RightBrace"));

        mixin(OperatorEqual!('=', "Equal", "EqualEqual"));
        mixin(OperatorEqual!('!', "Not", "NotEqual"));
        mixin(OperatorEqual!('>', "Greater", "GreaterEqual"));
        mixin(OperatorEqual!('<', "Less", "LessEqual"));

        mixin(DoubleOperator!('&', "And"));
        mixin(DoubleOperator!('|', "Or"));

        return Token(line, position, "\0", TokenType.EndOfFile);
    }
}

auto GetTypeForStr(string acc) {
    debug {
        if (acc == "assert") return TokenType.Keyword_Assert;
        if (acc == "print") return TokenType.Keyword_Print;
    }
    if (acc == "let" || acc == "var") return TokenType.Keyword_Let;
    if (acc == "true") return TokenType.Keyword_True;
    if (acc == "false") return TokenType.Keyword_False;
    if (acc == "null") return TokenType.Keyword_Null;
    if (acc == "if") return TokenType.Keyword_If;
    if (acc == "while") return TokenType.Keyword_While;
    if (acc == "for") return TokenType.Keyword_For;
    if (acc == "else") return TokenType.Keyword_Else;
    return TokenType.Identifier;
}

template SingleOperator(char op, string type)
{
    const char[] SingleOperator = "
        if (current == '" ~ op~"') {
            return Token(line, position, Next() ~ \"\", TokenType." ~type ~");
        }
    ";
}

template OperatorEqual(char op, string type, string multiType)
{
    const char[] OperatorEqual = "
        if (current == '" ~ op~"') {
            char tk = Next();
            if (current == '=') {
                return Token(line, position, tk ~ \"\" ~ Next(), TokenType."~multiType~");
            } else {
                return Token(line, position, tk ~ \"\", TokenType." ~type ~");
            }
        }
    ";
}

template DoubleOperator(char op, string type)
{
    const char[] DoubleOperator = "
        if (current == '" ~ op ~ "') {
            char tk = Next();
            if (current == '" ~ op ~ "') {
                return Token(line, position, tk ~ \"\" ~ Next(), TokenType."~type~");
            }
        }
    ";
}

int UnaryOperatorPrecedence(TokenType type) {
    switch(type) {
        case TokenType.Not:
        case TokenType.Minus:
            return 1;
        default:
            return 0;
    }
} 

int BinaryOperatorPrecedence(TokenType type) {
    switch(type) {
        case TokenType.Multiply:
        case TokenType.Divide:
            return 4;
        case TokenType.Plus:
        case TokenType.Minus:
            return 3;
        case TokenType.Greater:
        case TokenType.GreaterEqual:
        case TokenType.Less:
        case TokenType.LessEqual:
        case TokenType.EqualEqual:
        case TokenType.NotEqual:
            return 2;
        case TokenType.Or:
        case TokenType.And:
            return 1;
        default:
            return 0;
    }
}
