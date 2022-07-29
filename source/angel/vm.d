module angel.vm;

import angel.bytes;

import std.stdio;
import std.format;
import std.stdint;

import std.algorithm: remove;

public enum VMExitCode {
    InternalError,
    RuntimeError,
    Okay
}

public struct CallFrame {
    public AngelFunction func;
    public Value[] allocLets;
    
    public uint8_t* ip;

    public void Release() {
        this.allocLets = null;
    }
}

public class AngelVM {

    public CallFrame[1520] frames;
    public int callIndex = 0;
    public Value[256] stack;

    public auto currentCallFrame() {return &frames[callIndex];}
    public auto chunk() {return &frames[callIndex].func.source;}

    public AngelObject[] allocatedObjects;

    public Value* stackTop;

    public AngelModule[string] modules;

    debug {
        public int AssertSuccessCount = 0;
    }

    public this() {
        this.ResetStack();
    }

    public void ResetStack() {
        stackTop = stack.ptr;
    }

    public void AddModule(string m,AngelModule e) {
        modules[m] = e;
    }

    public void Invoke(string moduleName, string method) {
        Invoke(modules[moduleName].functions[method]);
    }

    public void Invoke(AngelFunction ef) {
        debug {
            writeln("Invoking Function: ", ef.name);
        }
        frames[callIndex] = CallFrame(ef);
        ResetStack();
        Run();
    }

    public VMExitCode Run() {
        this.currentCallFrame.ip = chunk.bytes.ptr;

        for (;;) {
            debug {
                write("stack > ");
               for (Value* v = this.stack.ptr; v < this.stackTop; v++) {
                    write("[");
                    if (v.type == ValueType.Object) {
                            if (v.isNull) write("null");
                            else write(allocatedObjects[AsObject(*v)].ToString());
                    } else {
                        PrintValue(*v);
                    }
                    write("] ");
                }
                writeln();
                DissasembleInstruction(cast(int)(this.currentCallFrame.ip - this.chunk.bytes.ptr), *this.chunk);
            }
            uint8_t inst = ReadByte();
            switch(inst) {
                debug {
                    case OpSet.Assert: {
                        int ln = GetCurrentLine();
                        Value bR = Pop();

                        if (bR.type != ValueType.Boolean) {
                            writefln("Assertion failure in line %d > Assertion Expression resulted in a non-boolean value.",ln);
                            break;
                        }
                        
                        if (!AsBool(bR)) {
                            writefln("Assertion failure in line %d > condition not matched.", ln);
                            break;
                        } else {
                            AssertSuccessCount++;
                        }
                        break;
                    }
                    case OpSet.Print: {
                        Value v = Pop();

                        if (v.type == ValueType.Object) {
                            if (v.isNull) writeln("null");
                            else writeln(allocatedObjects[AsObject(v)].ToString());
                        } else {
                            PrintValue(v);
                            writeln();
                        }
                        break;
                    }
                }
                case OpSet.AllocObject: {
                    AngelObject obj = ReadObject();
                    allocatedObjects ~= obj;
                    Push(NewObject(cast(int) allocatedObjects.length - 1));
                    break;
                }
                case OpSet.Pop: {
                    Pop();
                    break;
                }
                case OpSet.Loop: {
                    uint16_t offset = ReadShort();
                    this.currentCallFrame.ip = this.currentCallFrame.ip - offset;
                    break;
                }
                case OpSet.Jump: {
                    uint16_t offset = ReadShort();
                    this.currentCallFrame.ip += offset;
                    break;
                }
                case OpSet.JumpCaseFalse: {
                    uint16_t offset = ReadShort();
                    Value a = Peek(0);

                    if (a.type != ValueType.Boolean) {
                        RuntimeError(GetCurrentLine(), "cannot use non-boolean expression here.");
                        return VMExitCode.RuntimeError;
                    }

                    if (!AsBool(a)) {
                        this.currentCallFrame.ip += offset;
                    }

                    break;
                }
                case OpSet.DeleteLet: {
                    int delIndex = AsInt(ReadConstant());
                    currentCallFrame.allocLets.remove(delIndex);
                    break;
                }
                case OpSet.AllocLet: {
                    ValueType type = cast(ValueType) AsInt(ReadConstant());
                    currentCallFrame.allocLets ~= NewValue(type);
                    break;
                }
                case OpSet.SetLet: {
                    Value* targetVariable = &currentCallFrame.allocLets[AsInt(ReadConstant())];
                    Value v = Pop();

                    if (targetVariable.type == ValueType.Object && v.type == ValueType.Object) {
                        if (!v.isNull)
                            targetVariable.isNull = false;
                    } 

                    if (targetVariable.type == v.type) {
                        targetVariable.data = v.data;
                    } else if (targetVariable.type == ValueType.FloatingPoint && v.type == ValueType.Integer) {
                        targetVariable.data = NewFloat(AsInt(v)).data;
                    } else {
                        write("Error in line ",GetCurrentLine()," > Cannot assign a '");
                        PrintType(v);
                        write("' to a '");
                        PrintType(*targetVariable);
                        writeln("'");
                        return VMExitCode.RuntimeError;
                    }
                    break;
                }
                case OpSet.GetLet: {
                    Push(currentCallFrame.allocLets[AsInt(ReadConstant())]);
                    break;
                }
                case OpSet.Add: {
                        Value b = Pop();
                        Value a = Pop();

                        if (a.type == ValueType.Object && b.type == ValueType.Object) {
                            if (allocatedObjects[AsObject(a)].type == ObjectType.String &&
                            allocatedObjects[AsObject(b)].type == ObjectType.String){
                                AngelString as = new AngelString(
                                    (cast(AngelString) allocatedObjects[AsObject(a)]).characters ~
                                    (cast(AngelString) allocatedObjects[AsObject(b)]).characters
                                );
                                allocatedObjects ~= as;
                                Push(NewObject(cast(int) allocatedObjects.length - 1));
                                break;
                            }
                        }
                        
                        if (!IsNumeric(a) || !IsNumeric(b)) {
                            RuntimeError(this.chunk.GetLine(cast(int) (this.currentCallFrame.ip - this.chunk.bytes.ptr) - 2),
                                            "One of the operands isn't Numeric.");
                            return VMExitCode.RuntimeError;
                        } 

                        if (b.type == ValueType.FloatingPoint && a.type == ValueType.FloatingPoint) {
                            Push(NewFloat(AsFloat(a) + AsFloat(b)));
                        } else if (b.type == ValueType.FloatingPoint && a.type == ValueType.Integer) {
                            Push(NewFloat(AsInt(a) + AsFloat(b)));
                        } else if (b.type == ValueType.Integer && a.type == ValueType.FloatingPoint) {
                            Push(NewFloat(AsFloat(a) + AsInt(b)));
                        } else {
                            Push(NewInt(AsInt(a) + AsInt(b)));
                        }
                    break;
                }
                case OpSet.Subtract:  {mixin(ResolveOperator!("-"));         break;}
                case OpSet.Multiply:  {mixin(ResolveOperator!("*"));         break;}
                case OpSet.Divide:    {mixin(ResolveOperator!("/"));         break;}
                case OpSet.Greater:   {mixin(ResolveBooleanOperator!(">")); break;}
                case OpSet.Less:      {mixin(ResolveBooleanOperator!("<")); break;}
                case OpSet.Equal:     {mixin(ResolveBooleanOperator!("==")); break;}
                case OpSet.Different: {mixin(ResolveBooleanOperator!("!=")); break;}
                case OpSet.And:       {mixin(ResolveBooleanOperator!("&&")); break;}
                case OpSet.Or:        {mixin(ResolveBooleanOperator!("||")); break;}

                case OpSet.True: Push(NewBool(true)); break;
                case OpSet.False: Push(NewBool(false)); break;
                case OpSet.Null: {
                    Value a = Value(ValueType.Object);
                    a.isNull = true;
                    Push(a);
                    break;
                }
                case OpSet.Constant: {Push(ReadConstant());break;}
                case OpSet.Negate:   {
                    Value a = Pop();

                    switch(a.type) {
                        case ValueType.FloatingPoint:
                            Push(NewFloat(-AsFloat(a))); break;
                        case ValueType.Integer:
                            Push(NewInt(-AsInt(a))); break;
                        default:
                            return VMExitCode.RuntimeError;
                    }
                    break;
                }
                case OpSet.Not: {
                    Value a = Pop();
                    if (a.type != ValueType.Boolean) {
                        RuntimeError(cast(int)(this.currentCallFrame.ip - this.chunk.bytes.ptr) - 1, "Invalid Operator!");
                        return VMExitCode.RuntimeError;
                    } else {
                        Push(NewBool(!AsBool(a)));
                    }
                    break;
                }
                default:
                    InternalError("unrecognized OpSet '%03d' at offset '%03d'", inst,
                                            cast(int)(this.currentCallFrame.ip - this.chunk.bytes.ptr) - 1);
                    return VMExitCode.InternalError;
                case OpSet.Return: {
                    return VMExitCode.Okay;
                }
            }
        }
    }

    Value Peek(int distance) {return this.stackTop[-1 - distance];}
    void Push(Value v) {*this.stackTop = v; this.stackTop++;}
    Value Pop() {this.stackTop--; return *this.stackTop;}

    auto ReadShort() {
        this.currentCallFrame.ip += 2;
        return cast(uint16_t)((this.currentCallFrame.ip[-2] << 8) |  this.currentCallFrame.ip[-1]);
    }

    auto GetCurrentLine() {
        return chunk.GetLine(cast(int)(this.currentCallFrame.ip - this.chunk.bytes.ptr) - 1);
    }

    auto ReadByte() {
        return *(this.currentCallFrame.ip++);
    }

    auto ReadConstant() {
        return this.chunk.constants[ReadByte()];
    }

    auto ReadObject() {
        return this.chunk.objects[ReadByte()];
    }

    void InternalError(Char, Args...)(in Char[] fmt, Args args){
        writeln("VM Internal Error > ", format(fmt, args));
    }
    
    void RuntimeError(Char, Args...)(int line, in Char[] fmt, Args args){
        writeln("Error in line %d > ", format(fmt, args));
    }

    void PrintType(Value v) {
        if (v.type == ValueType.Object) {
            write(allocatedObjects[AsObject(v)].type);
        } else {
            write(v.type);
        }
    } 
}


template ResolveOperator(string operator) {
    const char[] ResolveOperator = "
        Value b = Pop();
        Value a = Pop();
        
        if (!IsNumeric(a) || !IsNumeric(b)) {
            RuntimeError(this.chunk.GetLine(cast(int) (this.currentCallFrame.ip - this.chunk.bytes.ptr) - 2),
                            \"One of the operator isn't Numeric.\");
            return VMExitCode.RuntimeError;
        } 

        if (b.type == ValueType.FloatingPoint && a.type == ValueType.FloatingPoint) {
            Push(NewFloat(AsFloat(a) " ~ operator ~ " AsFloat(b)));
        } else if (b.type == ValueType.FloatingPoint && a.type == ValueType.Integer) {
            Push(NewFloat(AsInt(a) " ~ operator ~ " AsFloat(b)));
        } else if (b.type == ValueType.Integer && a.type == ValueType.FloatingPoint) {
            Push(NewFloat(AsFloat(a) " ~ operator ~ " AsInt(b)));
        } else {
            Push(NewInt(AsInt(a) " ~ operator ~ " AsInt(b)));
        }
    "; 
}


template ResolveBooleanOperator(string operator) {
    const char[] ResolveBooleanOperator = "
        Value b = Pop();
        Value a = Pop();

        if (b.type == ValueType.FloatingPoint && a.type == ValueType.FloatingPoint) {
            Push(NewBool(AsFloat(a) " ~ operator ~ " AsFloat(b)));
        } else if (b.type == ValueType.FloatingPoint && a.type == ValueType.Integer) {
            Push(NewBool(AsInt(a) " ~ operator ~ " AsFloat(b)));
        } else if (b.type == ValueType.Integer && a.type == ValueType.FloatingPoint) {
            Push(NewBool(AsFloat(a) " ~ operator ~ " AsInt(b)));
        } else if (b.type == ValueType.Boolean && a.type == ValueType.Boolean) {
            Push(NewBool(AsBool(a) " ~ operator ~ " AsBool(b)));
        } else if (b.type == ValueType.Integer && a.type == ValueType.Integer) {
            Push(NewBool(AsInt(a) " ~ operator ~ " AsInt(b)));
        } else {
            RuntimeError(this.chunk.GetLine(cast(int) (this.currentCallFrame.ip - this.chunk.bytes.ptr) - 2),
                            \"One of the operator isn't Numeric.\");
            return VMExitCode.RuntimeError;
        }
    "; 
}