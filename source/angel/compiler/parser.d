module angel.compiler.parser;

import std.stdio;
import std.format;

import angel.compiler.ast;
import angel.compiler.lexer;

public class Parser {

    public Token current;
    public Lexer lexer;
    public bool hadErrors;

    public this(string source) {
        this.lexer = new Lexer(source);
        this.Next();
    }

    public SyntaxTree Parse() {
        SyntaxTree t = new SyntaxTree();

        while (current.type != TokenType.EndOfFile) {
            Node nd = ParseStatment();
            
            if (nd is null) continue;
            
            t.tree ~= nd;
        }

        t.eof = current;

        return t;
    }

    public Node ParseStatment() {
        if (current.type == TokenType.LeftBrace) {
            return ParseBlock();
        } else if (current.type == TokenType.Keyword_Let) {
            // leeeetttt!
            return ParseLet();
        } else if (current.type == TokenType.Keyword_If) {
            return ParseIf();
        } else if (current.type == TokenType.Keyword_While) {
            return ParseWhile();
        } else if (current.type == TokenType.Keyword_For) {
            return ParseFor();
        } else {
            debug {
                if (current.type == TokenType.Keyword_Assert) {
                    Next();
                    Node assertExpr = ParseExpressionSemicolon();
                    return new AssertNode(assertExpr, current);
                }
                if (current.type == TokenType.Keyword_Print) {
                    Next();
                    Node assertExpr = ParseExpressionSemicolon();
                    return new PrintNode(assertExpr, current);
                }
            }
            return ParseExpressionSemicolon();
        }
    }

    public Node ParseBlock() {
        Token start = Match([TokenType.LeftBrace]);
    
        Node[] things;
        while (current.type != TokenType.RightBrace && current.type != TokenType.EndOfFile) {
            things ~= ParseStatment();
        }  

        Token end = Match([TokenType.RightBrace]);
        BlockNode nd = new BlockNode(start,end, things);
        return nd;
    }

    public Node ParseFor() {
        ForNode fn = new ForNode(current.line);
        Next();
        Match([TokenType.LeftParen]);
        
        if (Is(TokenType.Semicolon)) {
            Next(); // advance;
        } else if (Is(TokenType.Keyword_Let)) {
            fn.initializer = ParseLet();
        } else {
            fn.initializer = ParseExpression();
        }

        if (!Is(TokenType.Semicolon)) {
            fn.condition = ParseExpression();
            Match([TokenType.Semicolon]);
        } else {
            Match([TokenType.Semicolon]);
        }

        if (!Is(TokenType.RightParen)) {
            fn.increment = ParseExpression();
            Match([TokenType.RightParen]);
        } else {
            Match([TokenType.RightParen]);
        }

        fn.block = ParseStatment();

        return fn;
    }

    public Node ParseWhile() {
        int ln = current.line;
        Next();

        Match([TokenType.LeftParen]);
        Node condition = ParseExpression();
        Match([TokenType.RightParen]);

        Node block = ParseStatment();

        return new WhileNode(ln, condition, block);
    }

    public Node ParseIf() {
        int ln = current.line;
        Next();

        Match([TokenType.LeftParen]);
        Node condition = ParseExpression();
        Match([TokenType.RightParen]);

        Node block = ParseStatment();

        IfNode nd = new IfNode(ln);
        nd.condition = condition;
        nd.block = block;
        
        if (Is(TokenType.Keyword_Else)) {
            nd.elseLine = Next().line;
            nd.elseClause = ParseStatment();
        }
        return nd;
    }

    public Node ParseLet() {
        Next();
        Token name = CurrentMatches(TokenType.Identifier);
        
        LetNode nd;
        bool hasType = current.type == TokenType.DoubleDot;
        if (hasType) {
            // skip ':'
            Next();
            // instianciate the node and check if the type is an identifier.
            
            nd = new LetNode(name, Match([TokenType.Identifier]));    
        } else { // no type == instiantiate it without a type.
        
            //TODO: add support for dynamic typing.
            writeln("Dynamic Typing is not avaliable in this version of AngelJS, please define a type on variable declaration");
            writeln("using default type 'Integer'");

            nd = new LetNode(name, Token(current.line, current.position, "int", TokenType.Identifier));
        }

        if (current.type == TokenType.Equal) {
            Next();
            // we can just parse it as an expression
            nd.assign = ParseExpression();
            Match([TokenType.Semicolon]);
        } else if (current.type == TokenType.Semicolon) {
            // just to not waste time
            if (!hasType) {
                Error("a let declaration without a type definition must be initialized.");
            }
            Next();
        } else {
            writeln("Error at line ", current.line,": Expected ';' not  ", current.type);
            hadErrors = true;
            return null;
        }
        return nd;
    }

    public Token Next() {
        Token last = current;
        this.current = this.lexer.NextToken();
        return last;
    }
    
    public Node ParseExpressionSemicolon(int parentPrecedence = 0) {
        Node n = ParseExpression(parentPrecedence);
        Match([TokenType.Semicolon]);
        return n;
    }

    public Node ParseExpression(int parentPrecedence = 0) {
        Node left;

        int unaryOperatorPrecedence = UnaryOperatorPrecedence(current.type);

        if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence) {
            Token operatorType = Next();
            Node operand = ParseExpression(unaryOperatorPrecedence);
            left = new UnaryNode(operatorType, operand);
        } else {
            writeln(current.type);
            left = ParsePrimary();
        }

        while (true) {
            int precedence = BinaryOperatorPrecedence(current.type);

            if (precedence == 0 || precedence <= parentPrecedence)
                break;
            
            Token operator = Next();
            Node right = ParseExpression(precedence);
            left = new BinaryOpNode(left, operator, right);
        }

        return left;
    }
    
    public Node ParseAssignment(Token varName) {
        Next();
        Node expr = ParseExpression();
        return new LetAssignNode(varName, expr);
    }

    private Node ParsePrimary() {
        if (Is(TokenType.Identifier)) {
            auto varAccess = Next();
            if (current.type == TokenType.Equal) {
                return ParseAssignment(varAccess);
            }
            return new AccessNode(varAccess);
        }
        if (Is(TokenType.LeftParen)) {
            Next();
            Node expr = ParseExpression();
            Next();

            return expr;
        }
        return new LiteralNode(Match([TokenType.Float, TokenType.Integer, TokenType.Keyword_True, TokenType.Keyword_False, TokenType.String, TokenType.Keyword_Null]));
    }

    public bool Is(TokenType t) {
        return current.type == t;
    }

    public Token CurrentMatches(TokenType t) {
        if (current.type == t) {
            return Next();
        } else {
            // same reason as in Match()
            writeln("Error at line ", current.line,": Expected '",t,"' not  ", current.type);
            hadErrors = true;
            Token tk = Token(current.line, current.position, "badtoken", t);
            Next();
            return tk;
        }
    }

    public Token Match(TokenType[] types) {
        foreach(TokenType t ; types) {
            if (current.type == t) {
                return Next();
            }
        }
        // i'm not using Error() here because i have no idea how to print out an element of an enum
        // as something like "Semicolon" or "Identifier"
        writeln("Error at line ", current.line,": Expected '",types[0],"' not  ", current.type);
        hadErrors = true;
        Next();
        return Token(current.line, current.position, "0", types[0]);
    }

    public void Error(Char, Args...)(in Char[] fmt, Args args) {
        writeln("Error at line ", current.line,": ", format(fmt, args));
        hadErrors = true;
    }
}