module angel.bytes;

import std.stdint;

public import angel.object;

auto NewObject(int obj) {
    valuedata d = valuedata();
    d.object = obj;
    return Value(ValueType.Object, d);
}

auto NewFloat(float de) {
    valuedata d = valuedata();
    d.floatingpoint = de;
    return Value(ValueType.FloatingPoint, d);
}

auto NewInt(int de) {
    valuedata d = valuedata();
    d.integer = de;
    return Value(ValueType.Integer, d);
}

auto NewBool(bool de) {
    valuedata d = valuedata();
    d.boolean = de;
    return Value(ValueType.Boolean, d);
}

auto NewValue(ValueType t) {
    switch (t) {
        case ValueType.Object: {
            Value v = Value(ValueType.Object);
            v.isNull = true;
            return v;
        }
        case ValueType.FloatingPoint: return NewFloat(0);
        case ValueType.Integer: return NewInt(0);
        case ValueType.Boolean: return NewBool(false);
        default:
            return NewInt(0);
    }
}

auto AsObject(Value v) {return v.data.object;}
auto AsFloat(Value v) {return v.data.floatingpoint;}
auto AsInt(Value v) {return v.data.integer;}
auto AsBool(Value v) {return v.data.boolean;}

auto IsNumeric(Value a) {return a.type == ValueType.FloatingPoint || a.type == ValueType.Integer;}

public union valuedata {
    float floatingpoint;
    int integer;
    bool boolean;
   
    int object;
}

public enum ValueType {
    FloatingPoint, Integer, Boolean, Object, Void,
}

public struct Value {
    ValueType type;
    valuedata data;
    bool isNull = false;
    bool isConstant = false;
}

public struct Line {
    int offset, line;
}

public struct ByteChunk {
    Value[] constants;
    uint8_t[] bytes;
    Line[] lines;
    AngelObject[] objects;

    uint8_t WriteObject(AngelObject object) {
        this.objects ~= object;
        return cast(uint8_t) (cast(int) objects.length - 1);
    }   

    void Write(uint8_t b, int line) {
        bytes ~= b;

        if (lines.length > 0 && lines[cast(int)lines.length - 1].line == line)
            return;
        
        Line ln = Line(cast(int) bytes.length - 1, line);
        lines ~= ln;
    }

    uint8_t WriteConstant(Value constant) {
        constants ~= constant;
        return cast(uint8_t) (cast(int)constants.length - 1);
    }

    int GetLine(int b) {
        int start = 0;
        int end = cast(int) lines.length - 1;

        for (;;) {
            int mid = (start + end) / 2;
            Line* ln = &lines[mid];
            if (b < ln.offset) {
                end = mid - 1;
            } else if (mid == cast(int) lines.length - 1 ||
                b < lines[mid + 1].offset) {
                return ln.line;
            } else {
                start = mid + 1;
            }
        }
    } 
}

enum OpSet {
    Constant, 
    Add, Subtract, Multiply, Divide,
    Negate, True, False, Null, Not,
    Greater, Less,
    Equal, Different,
    Or,And, Pop,

    AllocObject,
    DeleteObject,

    DeleteLet,
    AllocLet,
    GetLet,
    SetLet,

    Jump,
    Loop,
    JumpCaseFalse,

    // warning: these are only avaliable in DEBUG mode! check lexer.d lines 9-12
    Assert, Print,

    Return,
}

import std.format;

void WriteBytecode(ByteChunk bc, string name) {
    auto opSize = bc.bytes.length;
    auto ctSize = bc.constants.length;

    string bnFile = format("%g %g\n-ops\n", opSize, ctSize);

    // line information + opcodes
    int i = 0;
    foreach (uint8_t op ; bc.bytes) {
        bnFile ~= format("%d %d\n", bc.GetLine(i), op);
        i++;
    }
    bnFile ~= "-cos\n";
    //constants
    foreach(Value v ; bc.constants) {
        bnFile ~= format("%d %s\n",cast(int) v.type, ValueAsString(v));
    }

    import std.file : write;
    write(name ~".angelcs", bnFile);
}

enum DecodeMode {
    Operations, Constants        
}

ByteChunk ReadBytecode(string name) {
    ByteChunk bc;

    import std.file : readText;
    import std.string : splitLines, split;
    string[] file = readText(name ~".angelcs").splitLines();

    string[] lengths = file[0].split(" ");
    int operationsCount = lengths[0].to!int;
    int constantsCount = lengths[1].to!int;
    DecodeMode md;

    for (int i = 1; i < file.length; i++) {
        string currentLine = file[i];
        if (currentLine == "-ops") {
            md = DecodeMode.Operations;
        } else if (currentLine == "-cos") {
            md = DecodeMode.Constants;
        } else {
            string[] lineTks = currentLine.split(" ");
            if (md == DecodeMode.Operations) {
                int line = lineTks[0].to!int;
                uint8_t op = lineTks[1].to!uint8_t;
                bc.Write(op, line);
            } else {
                ValueType t = cast(ValueType) lineTks[0].to!int;
                Value cs;
                switch(t) {
                    case ValueType.Boolean: cs = NewBool(lineTks[1].to!bool); break;
                    case ValueType.FloatingPoint: cs = NewFloat(lineTks[1].to!float); break;
                    case ValueType.Integer: cs = NewInt(lineTks[1].to!int); break;
                    default: break;
                }
                bc.WriteConstant(cs);
            }
        }
    }

    return bc;
}

debug {
    import std.stdio;

    public void Dissasemble(ByteChunk b) {
        for (int i = 0; i < b.bytes.length;) {
            i = DissasembleInstruction(i, b);
        }
    }

    /**
        Dissasembles an instruction at the specified offset and 
        returns the next offset.
    */
    public int DissasembleInstruction(int offset, ByteChunk bc) {
        printf("%03d", offset);

        int line = bc.GetLine(offset);

        if (offset > 0 && line == bc.GetLine(offset - 1)) {
            printf("  | ");
        } else {
            printf("%3d ", line);
        }

        uint8_t inst = bc.bytes[offset];
        switch(inst) {
            case OpSet.AllocObject: {
                AngelObject o = bc.objects[bc.bytes[offset + 1]];
                writef("%-12s \'%s\'\n", "allocObject", o.ToString());
                return offset + 2;
            }
            case OpSet.DeleteObject: return ConstantInstruction("deleteObject", offset, bc);

            case OpSet.Constant:  return ConstantInstruction("constant", offset, bc);
            case OpSet.DeleteLet: return ConstantInstruction("deleteLet", offset, bc);
            case OpSet.AllocLet:  return LetInstruction("allocLet", offset, bc);
            case OpSet.SetLet:    return ConstantInstruction("setLet", offset, bc);
            case OpSet.GetLet:    return ConstantInstruction("getLet", offset, bc);
            case OpSet.Equal:     writefln("%-12s","equal");    return offset+1;
            case OpSet.Different: writefln("%-12s","different");return offset+1;
            case OpSet.Not:       writefln("%-12s","not");      return offset+1;
            case OpSet.True:      writefln("%-12s","true");     return offset+1;
            case OpSet.False:     writefln("%-12s","false");    return offset+1;
            case OpSet.Add:       writefln("%-12s","add");      return offset+1;
            case OpSet.Subtract:  writefln("%-12s","subtract"); return offset+1;
            case OpSet.Multiply:  writefln("%-12s","multiply"); return offset+1;
            case OpSet.Divide:    writefln("%-12s","divide");   return offset+1;
            case OpSet.Negate:    writefln("%-12s","negate");   return offset+1;
            case OpSet.Greater:   writefln("%-12s","greater");  return offset+1;
            case OpSet.Less:      writefln("%-12s","less");     return offset+1;
            case OpSet.And:       writefln("%-12s","and");      return offset+1;
            case OpSet.Or:        writefln("%-12s","or");       return offset+1;
            case OpSet.Pop:       writefln("%-12s","pop");       return offset+1;
            case OpSet.Return:    writefln("%-12s", "return");  return offset+1;
            case OpSet.Jump:               return JumpInstruction("jump", 1, bc, offset);
            case OpSet.JumpCaseFalse:      return JumpInstruction("jumpIfFalse", 1, bc, offset);
            case OpSet.Loop:               return JumpInstruction("loop", -1, bc, offset);

            debug {
                case OpSet.Assert:    writefln("%-12s","assert");    return offset+1;
                case OpSet.Print:    writefln("%-12s","print");    return offset+1;
            }

            default:
                printf("unknown\n");
                return offset+1;
        }
    }

    int JumpInstruction(string name, int sign,
                            ByteChunk chunk, int offset) {
        uint16_t jump = cast(uint16_t)(chunk.bytes[offset + 1] << 8);
        jump |= chunk.bytes[offset + 2];
        writefln("%-12s '%d' -> '%d'", name, offset,
                offset + 3 + sign * jump);
        return offset + 3;
    }
    
    int LetInstruction(string name, int ind, ByteChunk c) {
        Value v = c.constants[c.bytes[ind + 1]];
        writef("%-12s \'", name);
        ValueType t = cast(ValueType) AsInt(v);
        write(t);
        writeln("\'");
        return ind + 2;
    }

    int ObjectInstruction(string name, int ind, ByteChunk c) {
        Value v = c.constants[c.bytes[ind + 1]];
        writef("%-12s \'", name);
        ObjectType t = cast(ObjectType) AsInt(v);
        write(t);
        writeln("\'");
        return ind + 2;
    }

    int ConstantInstruction(string name, int ind, ByteChunk c) {
        Value v = c.constants[c.bytes[ind + 1]];
        writef("%-12s \'", name);
        PrintValue(v);
        writeln("\'");
        return ind + 2;
    }

    void PrintValue(Value v) {
        write(ValueAsString(v));
    }
}

import std.conv : to;
string ValueAsString(Value v) {
    switch(v.type) {
        case ValueType.Object:
            return "angelobj";
        case ValueType.Boolean:
            return AsBool(v).to!string;
        case ValueType.FloatingPoint:
            return AsFloat(v).to!string;
        case ValueType.Integer:
            return AsInt(v).to!string;
        default:
            return v.to!string;
    }
}