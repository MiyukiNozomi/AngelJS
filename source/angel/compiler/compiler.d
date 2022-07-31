module angel.compiler.compiler;

import std.conv;
import std.stdio;
import std.format;
import std.stdint;
import std.algorithm : remove;

import angel.bytes;

import angel.compiler.ast;
import angel.compiler.lexer;
import angel.compiler.parser;

public class ObjectAlloc {
    ObjectType type;
    int depth;

    public this(ObjectType type, int depth) {
        this.type = type;
        this.depth = depth;
    }
}

public class Local {
    string name;
    ValueType type;
    int position;
    int depth;

    int objId;

    public this(string name, ValueType type) {
        this.name = name;
        this.type = type;
    }
}

public class ModuleCompiler {

    public AngelModule[] modules;
    public int currModuleCompIndex = 0;

    public AngelModule currentModule() {return modules[currModuleCompIndex];}

    public SyntaxTree[] syntaxTrees;

    public void AddTree(SyntaxTree t) {
        syntaxTrees ~= t;
    }

    public bool ModuleExists(Token name, out SyntaxTree target) {
        for (int d = 0; d < syntaxTrees.length; d++) {
            if (syntaxTrees[d].filename == name.text) {
                target = syntaxTrees[d];
                return true;
            }
        }
        return false;
    }

    public void MapImports(int index) {
        SyntaxTree t = syntaxTrees[index];

        for (int g = 0; g < t.imports.length; g++) {
            ImportNode im = cast(ImportNode) t.imports[g];
            SyntaxTree importedTree;
            if (ModuleExists(im.mod, importedTree)) {
                FunctionMap map = FunctionMap(im.mod.text);
                
                for (int c = 0; c < importedTree.functions.length; c++) {
                    map.functions ~= importedTree.functions[c].name.text;
                }
            } else {
                CompileError(t.filename, im.mod.line, "The module '%s' doesn't exists. are you missing a reference?", im.mod.text);
            }
        }
    }

    public void CompileAll() {
        for (int i = 0; i < syntaxTrees.length; i++) {
            modules ~= CompileModule(i);
        }
    }

    public AngelModule CompileModule(int index) {
        SyntaxTree things = syntaxTrees[index];
        MapImports(index);
        for (int i = 0; i < things.variables.length; i++) {
            Compiler cd = new Compiler(things.filename, things.variables[i], this);
            GlobalValue gv = cd.CompileField();
            currentModule.globals[gv.name] = gv;
        }

        for (int d = 0; d < things.functions.length; d++) {
            Compiler cc = new Compiler(things.filename, things.functions[d], this);
            AngelFunction e = cc.CompileFunction();
            currentModule.functions[e.name] = e;
        }

        return currentModule;
    }

    void CompileError(Char, Args...)(string file, int line, in Char[] fmt, Args args) {
        writeln("Error in file '",file, "' at line ", line, " > ", format(fmt, args));
    }
}

public class Compiler {

    public ModuleCompiler moduleCompiler;

    private ByteChunk finalBytecode;
    private Node tree;
    public bool hadErrors = false;

    public Local[string] locals;
    public int localLength = 0;

    public ObjectAlloc[] allocObjects;

    // current scope depth
    public int currentDepth = -1;

    public AngelFunction currentFunction;

    public string file;
    public bool wroteReturn = false;

    public this(string file, Node tree, ModuleCompiler cc) {
        this.file = file;
        this.tree = tree;
        this.moduleCompiler = cc;
    }

    public GlobalValue CompileField() {
        LetNode nd = cast(LetNode) this.tree;

        GlobalValue e = new GlobalValue();
        e.name = nd.name.text;

        ObjectType tt;
        if (IsObject(nd.type, tt)) {
            e.type = ValueType.Object;
        } else {
            e.type = TypeToVType(nd.type);
        }
        e.objectType = tt;

        e.value.type = e.type;

        if (nd.assign !is null) {
            StartScope();
            TranslateNode(nd.assign);
            EndScope(nd.name.line);
        }
//        e.assignOperation = finalBytecode;
        
        return e;
    }

    public AngelFunction CompileFunction() {
        FunctionNode fn = cast(FunctionNode) this.tree;

        currentFunction = new AngelFunction();
        currentFunction.name = fn.name.text;

        ObjectType tt;
        if (IsObject(fn.returnType, tt)) {
            currentFunction.returnType = ValueType.Object;
            currentFunction.returnObjType = tt;
        } else {
            currentFunction.returnType = TypeToVType(fn.returnType);
        }

        currentFunction.accessLevel = fn.levelModifier.text == "public" ? AccessLevel.Public : AccessLevel.Private;

        StartScope();
        foreach (FunctionParameter e ; fn.parameters) {
            string name = e.name.text;
            ObjectType otype;
            ValueType vtype;

            if (IsObject(e.type, otype)) {
                vtype = ValueType.Object;
            } else {
                vtype = TypeToVType(e.type);
            }

            Parameter p = new Parameter();
            p.name = name;
            p.paramType = vtype;
            p.objectType = otype;

            Local ll = new Local(p.name, p.paramType);
            AddLocal(ll);

            currentFunction.parameters ~= p;
        }
        TranslateNode(fn.block, false);

        bool wroteR = wroteReturn;
        EndScope(fn.block.end.line);

        if (!wroteR && currentFunction.returnType == ValueType.Void) {
            finalBytecode.Write(OpSet.Return, fn.block.end.line);
        } else if (!wroteR) {
            CompileError(fn.block.end.line, "This function should return a value of type '%s'.", fn.returnType.text);
        }
        
        currentFunction.source = finalBytecode;

        debug {
            writeln("func '", currentFunction.name, "' bytecode--");
            Dissasemble(currentFunction.source);
        }

        //finalBytecode.Write(OpSet.Return, this.tree.eof.line);

        if (hadErrors) {
            writeln("Function '" ~ currentFunction.name ~"' Compilation finished with compiler errors, the program may not work as intended.");
        } else {
            writeln("Compilation Succeded!");
        }

        return currentFunction;
    }

    public bool IsObject(Token t, out ObjectType type) {
        switch(t.text) {
            case "string":
                type= ObjectType.String;
                return true;
            case "function":
                type= ObjectType.Function;
                return true;
            default:
                //CompileError(t.line, "Unrecognized type '%s'. are you missing a module?", t.text);
                return false;        
        }
    }

    public ValueType TypeToVType(Token t) {
        switch(t.text) {
            case "float": return ValueType.FloatingPoint;
            
            case "integer":
            case "int": return ValueType.Integer;
            
            case "boolean":
            case "bool": return ValueType.Boolean;

            case "void":
                return ValueType.Void;

            default:
                CompileError(t.line, "Unrecognized type '%s'. are you missing a module?", t.text);
                return ValueType.Integer;
        }
    }

    public void StartScope() {
        this.currentDepth++;
    }

    public void EndScope(int lastLine) {
        wroteReturn = false;
        this.currentDepth -= 1;

        for (int i = cast(int) allocObjects.length - 1; i > 0;i--) {
            ObjectAlloc oa = allocObjects[i];
            if (oa.depth > currentDepth) {
                finalBytecode.Write(OpSet.DeleteObject, lastLine);
                finalBytecode.Write(finalBytecode.WriteConstant(NewInt(i)), lastLine);
            }
        }

        foreach(string name ; locals.byKey()) {
            Local c = locals[name];
            //remove references to locals we don't want.
            if (c.depth > currentDepth) {
                locals.remove(name);
                finalBytecode.Write(OpSet.DeleteLet, lastLine);
                finalBytecode.Write(finalBytecode.WriteConstant(NewInt(c.position)), lastLine);
                this.localLength--;                
            }
        }
    }

    public void AddLocal(Local lc) {
        lc.position = this.localLength++;
        lc.depth = currentDepth;
        lc.objId = -1;
        locals[lc.name] = lc;
    }

    public void TranslateNode(Node t, bool canOpenScope = true) {
        switch(t.type) {
            case NodeType.FunctionCall: {
                FunctionCallNode nd = cast(FunctionCallNode) t;

                for (ulong i = 0; i < nd.parameters.length; i++) {
                    TranslateNode(nd.parameters[i], false);
                }

                finalBytecode.Write(OpSet.FindMethodAndInvoke, nd.funcName.line);
                finalBytecode.Write(finalBytecode.WriteObject(new AngelString(nd.funcName.text)), nd.funcName.line);
                finalBytecode.Write(finalBytecode.WriteConstant(NewInt(cast(int) nd.parameters.length)), nd.funcName.line);

                break;
            }
            case NodeType.Return: {
                ReturnNode nd = cast(ReturnNode) t;
                TranslateNode(nd.returnThingy);
                finalBytecode.Write(OpSet.Return, nd.ln.line);
                wroteReturn = true;
                break;
            }
            case NodeType.Block: {
                BlockNode bn = cast(BlockNode) t;
                if (canOpenScope)
                    StartScope();
          
                foreach (Node n ; bn.code) {
                    TranslateNode(n);
                }
            
                if (canOpenScope)
                    EndScope(bn.end.line);
                break;
            }
            debug {
                case NodeType.Assert: {
                    AssertNode nd = cast(AssertNode) t;
                    TranslateNode(nd.expr);
                    finalBytecode.Write(OpSet.Assert, nd.current.line);
                    break;
                }
                case NodeType.Print: {
                    PrintNode nd = cast(PrintNode) t;
                    TranslateNode(nd.expr);
                    finalBytecode.Write(OpSet.Print, nd.current.line);
                    break;
                }
            }
            case NodeType.For: {
                ForNode nd = cast(ForNode) t;
                StartScope();

                if (nd.initializer !is null)
                    TranslateNode(nd.initializer);

                int loopStart = cast(int) finalBytecode.bytes.length;

                int exitJump = -1;
                if (nd.condition !is null) {
                    TranslateNode(nd.condition);

                    exitJump = EmitJump(OpSet.JumpCaseFalse, nd.line);
                    finalBytecode.Write(OpSet.Pop, nd.line);
                }
/*
                if (nd.increment !is null) {
                    int bodyJump = EmitJump(OpSet.Jump, nd.line);
                    int incrementStart = cast(int) finalBytecode.bytes.length;
                    TranslateNode(nd.increment);
                    finalBytecode.Write(OpSet.Pop, nd.line);

                    EmitLoop(loopStart, nd.line);
                    loopStart = incrementStart;
                    PatchJump(bodyJump, nd.line);
                }*/

                TranslateNode(nd.block);
                if (nd.increment !is null) {
                    TranslateNode(nd.increment);
                }
            
                EmitLoop(loopStart, nd.line);

                if (exitJump != -1) {
                    PatchJump(exitJump, nd.line);
                    finalBytecode.Write(OpSet.Pop, nd.line);
                }

                EndScope(nd.line);
                break;
            }
            case NodeType.While: {
                WhileNode nd = cast(WhileNode) t;

                int loopStart = cast(int) finalBytecode.bytes.length;
                TranslateNode(nd.condition);

                int exitJump = EmitJump(OpSet.JumpCaseFalse, nd.line);
                finalBytecode.Write(OpSet.Pop, nd.line);
                TranslateNode(nd.block);

                EmitLoop(loopStart, nd.line);

                PatchJump(exitJump, nd.line);
                finalBytecode.Write(OpSet.Pop, nd.line);
                break;
            }
            case NodeType.If: {
                IfNode nd = cast(IfNode) t;
                TranslateNode(nd.condition);

                // emit a jump from the begining of the if statment
                int thenJump = EmitJump(OpSet.JumpCaseFalse, nd.line);
                finalBytecode.Write(OpSet.Pop, nd.line);

                TranslateNode(nd.block);

                int elseJump = EmitJump(OpSet.Jump, nd.elseLine);

                // and then we set the end of the jump after the statement.
                PatchJump(thenJump, nd.line);
                finalBytecode.Write(OpSet.Pop, nd.line);
                int elseLine = nd.line;

                if (nd.elseClause !is null) {
                    TranslateNode(nd.elseClause);
                    elseLine = nd.line;
                }
                PatchJump(elseJump, elseLine);
                break;
            }
            case NodeType.LetAssign: {
                LetAssignNode nd = cast(LetAssignNode) t;

                if ((nd.varName.text in moduleCompiler.currentModule.globals) !is null) {
                    TranslateNode(nd.expr);
                    finalBytecode.Write(OpSet.FindAndSetLet, nd.varName.line);
                    finalBytecode.Write(finalBytecode.WriteObject(new AngelString(nd.varName.text)), nd.varName.line);
                    break;
                } else if ((nd.varName.text in locals) is null) {
                    CompileError(nd.varName.line, "Unrecognized variable '%s'", nd.varName.text);
                    return;
                }

                TranslateNode(nd.expr);
                finalBytecode.Write(OpSet.SetLet, nd.varName.line);
                finalBytecode.Write(finalBytecode.WriteConstant(NewInt(locals[nd.varName.text].position)), nd.varName.line);
                break;
            }
            case NodeType.Let: {
                LetNode nd = cast(LetNode) t;
                
                if (nd.name.text.length == 0) {
                    CompileError(nd.name.line, "Invalid variable name.");
                    return;
                }
                
                Local lc;
                
                ObjectType objType;
                if (IsObject(nd.type, objType)) {
                    lc = new Local(nd.name.text, ValueType.Object);
                } else {
                    lc = new Local(nd.name.text, TypeToVType(nd.type));
                }
                AddLocal(lc);

                finalBytecode.Write(OpSet.AllocLet, nd.name.line);
                finalBytecode.Write(finalBytecode.WriteConstant(NewInt(cast(int)lc.type)), nd.name.line);

                if (nd.assign !is null) {
                    TranslateNode(nd.assign);
                    finalBytecode.Write(OpSet.SetLet, nd.type.line);
                    finalBytecode.Write(finalBytecode.WriteConstant(NewInt(lc.position)), nd.type.line);
                }
                break;
            }
            case NodeType.Access: {
                Token thingName = (cast(AccessNode) t).thing;

                if ((thingName.text in moduleCompiler.currentModule.globals) !is null) {
                    finalBytecode.Write(OpSet.FindAndGetLet, thingName.line);
                    finalBytecode.Write(finalBytecode.WriteObject(new AngelString(thingName.text)), thingName.line);
                    break;
                } else if ((thingName.text in locals) is null) {
                    CompileError(thingName.line, "Unknown variable '%s'", thingName.text);
                    return;
                }

                finalBytecode.Write(OpSet.GetLet, thingName.line);
                finalBytecode.Write(finalBytecode.WriteConstant(NewInt(locals[thingName.text].position)),thingName.line);
                break;
            }
            case NodeType.Unary: {
                UnaryNode nd = cast(UnaryNode) t;
                TranslateNode(nd.operand);
                if (nd.type.type == TokenType.Minus) {
                    finalBytecode.Write(OpSet.Negate, nd.type.line);
                } else {
                    finalBytecode.Write(OpSet.Not, nd.type.line);
                }
                break;
            }
            case NodeType.Literal: {
                LiteralNode ln = cast(LiteralNode) t;
                if (ln.token.type == TokenType.Float) {
                    finalBytecode.Write(OpSet.Constant, ln.token.line);
                    finalBytecode.Write(finalBytecode.WriteConstant(NewFloat(ln.token.text.to!float)), 0);
                } else if (ln.token.type == TokenType.Keyword_True) {
                    finalBytecode.Write(OpSet.True, ln.token.line);
                } else if (ln.token.type == TokenType.Keyword_False) {
                    finalBytecode.Write(OpSet.False, ln.token.line);
                } else if (ln.token.type == TokenType.Keyword_Null) {
                    finalBytecode.Write(OpSet.Null, ln.token.line);
                } else if (ln.token.type == TokenType.String) {
                    ObjectAlloc e = new ObjectAlloc(ObjectType.String, currentDepth);
                    allocObjects ~= e;
                    finalBytecode.Write(OpSet.AllocObject, ln.token.line);
                    finalBytecode.Write(finalBytecode.WriteObject(new AngelString(ln.token.text)), ln.token.line);
                } else {
                    finalBytecode.Write(OpSet.Constant, ln.token.line);
                    finalBytecode.Write(finalBytecode.WriteConstant(NewInt(ln.token.text.to!int)), 0);
                }
                break;
            }
            case NodeType.BinaryOP: {
                BinaryOpNode bn = cast(BinaryOpNode) t;
                TranslateNode(bn.left);
                TranslateNode(bn.right);
                switch(bn.operator.type) {
                    case TokenType.Plus:       finalBytecode.Write(OpSet.Add, bn.operator.line); break;
                    case TokenType.Minus:      finalBytecode.Write(OpSet.Subtract, bn.operator.line); break;
                    case TokenType.Divide:     finalBytecode.Write(OpSet.Divide, bn.operator.line); break;
                    case TokenType.Multiply:   finalBytecode.Write(OpSet.Multiply, bn.operator.line); break;
                    case TokenType.EqualEqual: finalBytecode.Write(OpSet.Equal, bn.operator.line); break;
                    case TokenType.NotEqual:   finalBytecode.Write(OpSet.Different, bn.operator.line); break;
                    case TokenType.Less:       finalBytecode.Write(OpSet.Less, bn.operator.line); break;
                    case TokenType.Greater:    finalBytecode.Write(OpSet.Greater, bn.operator.line); break;
                    case TokenType.Or:         finalBytecode.Write(OpSet.Or, bn.operator.line); break;
                    case TokenType.And:        finalBytecode.Write(OpSet.And, bn.operator.line); break;
                    case TokenType.LessEqual: { 
                        finalBytecode.Write(OpSet.Greater, bn.operator.line);
                        finalBytecode.Write(OpSet.Not, bn.operator.line);
                        break;
                    }
                    case TokenType.GreaterEqual: {
                        finalBytecode.Write(OpSet.Less, bn.operator.line);
                        finalBytecode.Write(OpSet.Not, bn.operator.line);
                        break;
                    }
                    default: CompileError(bn.operator.line, "Invalid Binary Operator.");break;
                }
                break;
            }
            default:
                CompileError(-1, "Unrecognized NodeType.");
                return;
        }
    }
    private:
        void EmitLoop(int loopStart, int ln) {
            finalBytecode.Write(OpSet.Loop, ln);
            
            int offset = cast(int) finalBytecode.bytes.length - loopStart + 2;
            if (offset > UINT16_MAX) {
                CompileError(ln, "too big of a while loop.");
                return;
            }

            finalBytecode.Write((offset >> 8) & 0xff, ln);
            finalBytecode.Write(offset & 0xff, ln);
        }

        void PatchJump(int offset, int line) {
            int jump = cast(int) finalBytecode.bytes.length - offset - 2;
            if (jump > UINT16_MAX) {
                CompileError(line, "too big of an if-statement.");
                return;
            }

            finalBytecode.bytes[offset] = (jump >> 8) & 0xff;
            finalBytecode.bytes[offset + 1] = jump & 0xff;
        }
    
        int EmitJump(uint8_t inst, int line) {
            finalBytecode.Write(inst, line);
            finalBytecode.Write(0xFF, line);
            finalBytecode.Write(0xFF, line);
            return cast(int) finalBytecode.bytes.length - 2;
        }

        void CompileError(Char, Args...)(int line, in Char[] fmt, Args args) {
            writeln("Error in file '",file, "' at line ", line, " > ", format(fmt, args));
            hadErrors = true;
        }
}