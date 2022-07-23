module angel.compiler.ast;

import std.stdio;

import angel.compiler.lexer;

public enum NodeType {
    Literal, BinaryOP, Unary, Let, Access, LetAssign,
    If, While, For, Block,

    /** only avaliable in debug mode, check lexer.d in line 8 - 14*/
    Assert, Print
}

public abstract class Node {
    public NodeType type;
    public this(NodeType t) {this.type = t;}
    debug {
        public abstract void PrintNode(string indent);
    }
}

public class BlockNode : Node {
    public Token start,end;
    public Node[] code;

    public this(Token start, Token end, Node[] code) {
        super(NodeType.Block);
        this.start = start;
        this.end = end;
        this.code = code;
    }

    debug {
        public override void PrintNode(string indent) {
            writeln(indent, "Block");
            foreach(Node n ; code) {
                n.PrintNode("   " ~ indent);
            }
        }
    }
}

public class ForNode : Node {
    public Node block, initializer, condition, increment;
    public int line;

    public this(int line) {
        super(NodeType.For);
        this.line = line;
    }

    debug {
        public override void PrintNode(string indent) {
            writeln(indent, "For");
            write(indent, "   Block:\n");
            block.PrintNode(indent ~ "      ");

            if (this.initializer !is null) {
                write(indent, "   Initializer:\n");
                initializer.PrintNode(indent ~ "      ");
            }
            if (this.condition !is null) {
                write(indent, "   Condition:\n");
                condition.PrintNode(indent ~ "      ");
            }
            if (this.increment !is null) {
                write(indent, "   Increment:\n");
                increment.PrintNode(indent ~ "      ");
            }
        }
    }
}

public class SyntaxTree {
    public Node[] tree;
    public Token eof;

    public void Print() {
        for (int i = 0; i < tree.length; i++) {
            tree[i].PrintNode("");
        }
    }
}

debug {
    public class AssertNode : Node {

        public Node expr;

        // the only reason i capture the last token of this assert is just so that i can
        // throw an error specifically in the line of the assert.
        public Token current;

        public this(Node expr, Token current) {
            super(NodeType.Assert);
            this.current = current;
            this.expr = expr;
        }

        public override void PrintNode(string indent) {
            writeln(indent, "Assert");
            expr.PrintNode(indent ~ "   ");
        }
    }
    public class PrintNode : Node {

        public Node expr;
        public Token current;

        public this(Node expr, Token current) {
            super(NodeType.Print);
            this.current = current;
            this.expr = expr;
        }

        public override void PrintNode(string indent) {
            writeln(indent, "Print");
            expr.PrintNode(indent ~ "   ");
        }
    }
}

public class WhileNode : Node {
    public Node condition;
    public Node block;
    public int line;

    public this(int line, Node condition, Node block) {
        super(NodeType.While);
        this.line = line;
        this.condition = condition;
        this.block = block;
    }    

    debug {
        public override void PrintNode(string indent) {
            writeln(indent ~"While");
            write(indent, "   Condition:\n");
            condition.PrintNode(indent ~ "      ");

            write(indent, "   Block:\n");
            block.PrintNode(indent ~ "      ");
        }
    }
}

public class IfNode : Node {

    public Node condition;
    public Node block;

    public Node elseClause;    

    public int line;
    public int elseLine = -1;

    public this(int line) {
        super(NodeType.If);
        this.line = line;
    }

    debug {
        public override void PrintNode(string indent) {
            writeln(indent, "If");
            write(indent, "   Condition:\n");
            condition.PrintNode(indent ~ "      ");

            write(indent, "   Block:\n");
            block.PrintNode(indent ~ "      ");

            if (elseClause !is null) {
                write(indent, "   Else:\n");
                elseClause.PrintNode(indent ~ "      ");
            }
        }
    }
}

public class LetAssignNode : Node {
    public Token varName;
    public Node expr;

    public this(Token varName, Node expr) {
        super(NodeType.LetAssign);
        this.varName = varName;
        this.expr = expr;
    }

    debug {
        public override void PrintNode(string indent) {
            writeln(indent, "   LetAssignNode '", varName.text, "'");
            expr.PrintNode(indent ~"   ");
        }
    }
}

public class LetNode : Node {
    public Token type;
    public Token name;
    public Node assign;

                            // if this parameter isn't set, the compiler will try to find out its type.
    public this(Token name, Token type = Token(-1, -1, "auto", TokenType.EndOfFile)) {
        super(NodeType.Let);
        this.type = type;
        this.name = name;

        this.assign = null;
    }

    debug {
        public override void PrintNode(string indent) {
            writeln(indent,"Let '", name.text, "' : ", type.text);
            assign.PrintNode(indent ~"   ");
        }
    }
}

public class AccessNode : Node {
    public Token thing;

    public this(Token thing) {
        super(NodeType.Access);
        this.thing = thing;
    }   

    debug {
        public override void PrintNode(string indent) {
            writeln(indent, "Access '", thing.text, "'");
        }
    }
}

public class UnaryNode : Node {

    public Token type;
    public Node operand;

    public this(Token op, Node operand) {
        super(NodeType.Unary);
        this.type = type;
        this.operand = operand;
    }

    debug {
        public override void PrintNode(string indent) {
            writeln(indent, "Unary '", type.type,"'");
            operand.PrintNode(indent ~"   ");
        }
    }
}

public class BinaryOpNode : Node {
    public Node left;
    public Token operator;
    public Node right;

    public this(Node left, Token t, Node right) {
        super(NodeType.BinaryOP);
        this.left = left;
        this.operator = t;
        this.right = right;
    }

    debug {
        public override void PrintNode(string indent) {
            writeln(indent, "BinaryOp '", operator.type, "'");
            left.PrintNode(indent ~"   ");
            right.PrintNode(indent ~"   ");
        }
    }
}

public class LiteralNode : Node {
    
    public Token token;

    public this(Token t) {
        super(NodeType.Literal);
        this.token = t;
    }

    debug {
        public override void PrintNode(string indent) {
            writeln(indent, token);
        }
    }
}