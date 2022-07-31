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
    public uint8_t* ip;

    public Value[512] allocLets;
    public Value[256] stack;
    public Value* stackTop;

    public int letPointer;

    public void Release() {
        this.stack = new Value[256];
        this.allocLets = new Value[512];
    }
}

public class AngelVM {

    public CallFrame[5] frames;
    public int callIndex = -1;

    public CallFrame* currentCallFrame() {return &frames[callIndex];}
    public auto chunk() {           return &frames[callIndex].func.source;}
    public auto stack() {           return &frames[callIndex].stack;}

    public AngelObject[] allocatedObjects;

    public AngelModule[string] modules;

    debug {
        public int AssertSuccessCount = 0;
    }

    public this() {

    }

    public void ResetStack() {
        currentCallFrame.stackTop = stack.ptr;
    }

    public void AddModule(string m,AngelModule e) {
        modules[m] = e;
    }

    public bool Invoke(string moduleName, string method, int argCount) {
        AngelModule em = modules[moduleName];
        
        return Invoke(em.functions[method], argCount);
    }

    public bool Invoke(AngelFunction ef, int argCount) {
        debug {
            writeln("Invoking Function: ", ef.name);
        }

        int ln = 0;
        if (callIndex != -1)
            ln = GetCurrentLine();

        if (ef.parameters.length != argCount) {
            RuntimeError(ln, "Wrong argument count for function: '%s' expected %d parameters, not %d", ef.name, ef.parameters.length, argCount);
            return false;
        }
        
        Value[] stackVals;

        for (int i = 0; i < argCount; i++) {
            stackVals ~= Pop();
        }

        callIndex = callIndex + 1;
        frames[callIndex] = CallFrame(ef);
        this.currentCallFrame.ip = chunk.bytes.ptr;
        ResetStack();
        
        for (int d = 0; d < argCount; d++) {
            Value v = stackVals[d];
            if (v.type == ValueType.Integer && ef.parameters[d].paramType == ValueType.FloatingPoint) {
                Value ab = NewFloat(AsInt(v));
                v = ab;
            } else if (v.type != ef.parameters[d].paramType ||
                (v.type == ValueType.Object &&
                 ef.parameters[d].paramType == ValueType.Object &&
                 allocatedObjects[AsObject(v)].type != ef.parameters[d].objectType)){
                write("Error in line ",ln," > Expected parameter ", d, " to be of type '");
                write(ef.parameters[d].paramType);
                write("' not '");
                PrintType(v);
                writeln("'");
                return false;
            }
            currentCallFrame.allocLets[currentCallFrame.letPointer++] = v;
        }
        
        auto exitCode = Run();
        if (exitCode != VMExitCode.Okay) {
            writeln("AngelVM Failed to execute function '", ef.name ,"' properly.");
            return false;
        }

        debug {
            writeln("Leaving Function: ", ef.name);
        }
        return true;
    }

    public VMExitCode Run() {
        if (callIndex < 0)
            return VMExitCode.RuntimeError;

        for (;;) {
            debug {
                write("stack > ");
               for (Value* v = this.stack.ptr; v < this.currentCallFrame.stackTop; v++) {
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
                case OpSet.FindAndGetLet: {
                    AngelString obj = cast(AngelString) ReadObject();
                    string targetVar = obj.characters;

                    bool found = false;
                    foreach (string k ; modules.byKey()) {
                        AngelModule m = modules[k];
                        foreach (string nm ; m.globals.byKey()) {
                            if (nm == targetVar) {
                                Push(m.globals[nm].value);
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                    if (!found) {
                        RuntimeError(GetCurrentLine(), "the variable: %s wasn't found on the current context, are you missing a mod?", targetVar);
                    }
                    break;
                }
                case OpSet.FindAndSetLet: {
                    AngelString obj = cast(AngelString) ReadObject();
                    string targetVar = obj.characters;

                    bool found = false;
                    foreach (string k ; modules.byKey()) {
                        AngelModule m = modules[k];
                        foreach (string nm ; m.globals.byKey()) {
                            if (nm == targetVar) {
                                GlobalValue targetVariable = m.globals[targetVar];
                                Value v = Pop();

                                if (targetVariable.type == ValueType.Object && v.type == ValueType.Object) {
                                    if (!v.isNull)
                                        targetVariable.value.isNull = false;
                                } 

                                if (targetVariable.type == v.type) {
                                    targetVariable.value.data = v.data;
                                } else if (targetVariable.type == ValueType.FloatingPoint && v.type == ValueType.Integer) {
                                    targetVariable.value.data = NewFloat(AsInt(v)).data;
                                } else {
                                    write("Error in line ",GetCurrentLine()," > Cannot assign a '");
                                    PrintType(v);
                                    write("' to a '");
                                    PrintType(targetVariable.value
                                    );
                                    writeln("'");
                                    return VMExitCode.RuntimeError;
                                }
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                    if (!found) {
                        RuntimeError(GetCurrentLine(), "the variable: %s wasn't found on the current context, are you missing a mod?", targetVar);
                    }
                    break;
                }
                case OpSet.FindMethodAndInvoke: {
                    AngelString obj = cast(AngelString) ReadObject();
                    int argCount = AsInt(ReadConstant());
                    string targetFunc = obj.characters;

                    bool foundFunction = false;
                    foreach (string k ; modules.byKey()) {
                        AngelModule m = modules[k];
                        foreach (string nm ; m.functions.byKey()) {
                            if (nm == targetFunc) {
                                if(!Invoke(k, targetFunc, argCount)) {
                                    return VMExitCode.RuntimeError;
                                }
                                foundFunction = true;
                                break;
                            }
                        }
                        if (foundFunction)
                            break;
                    }
                    if (!foundFunction) {
                        RuntimeError(GetCurrentLine(), "the method: %s wasn't found on the current context, are you missing a mod?", targetFunc);
                    }
                    break;
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
                    currentCallFrame.letPointer = delIndex - 1;
                    break;
                }
                case OpSet.AllocLet: {
                    ValueType type = cast(ValueType) AsInt(ReadConstant());
                    currentCallFrame.allocLets[currentCallFrame.letPointer++] = NewValue(type);
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
                    int i = AsInt(ReadConstant());

                    if (i >= currentCallFrame.allocLets.length) {
                        RuntimeError(GetCurrentLine(), "Tried to access a variable that doesn't exists.");
                        return VMExitCode.RuntimeError;
                    }

                    Push(currentCallFrame.allocLets[i]);
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
                    if (callIndex >= 0) {
                        Value returnVal;
                        bool crashed = false;
                        if (currentCallFrame().func.returnType != ValueType.Void) {
                            returnVal = Pop();

                            if (returnVal.type == ValueType.Integer && currentCallFrame.func.returnType == ValueType.FloatingPoint) {
                                Value e = NewFloat(AsInt(returnVal));
                                returnVal = e;
                            } else if (returnVal.type != currentCallFrame.func.returnType) {
                                write("Error in line ",GetCurrentLine()," > Cannot return a '");
                                PrintType(returnVal);
                                write("' in a function that should return an '", currentCallFrame.func.returnType);
                                writeln("'");
                                crashed = true;
                            } else if (returnVal.type == ValueType.Object &&
                                currentCallFrame.func.returnObjType != allocatedObjects[AsObject(returnVal)].type) {
                                write("Error in line ",GetCurrentLine()," > Cannot return a '");
                                PrintType(returnVal);
                                write("' in a function that should return an '", currentCallFrame.func.returnObjType);
                                writeln("'");
                                crashed = true;
                            }
                        } else {
                            returnVal = Value(ValueType.Object);
                            returnVal.isNull = true;
                        }

                        frames[callIndex] = CallFrame();
                        callIndex--;   
                        if (callIndex > -1)
                            Push(returnVal);

                        return crashed ? VMExitCode.RuntimeError : VMExitCode.Okay;
                    } else
                        return VMExitCode.Okay;
                }
            }
        }
    }

    Value Peek(int distance) {return this.currentCallFrame.stackTop[-1 - distance];}
    void Push(Value v) {*this.currentCallFrame.stackTop = v; this.currentCallFrame.stackTop++;}
    Value Pop() {this.currentCallFrame.stackTop--; return *this.currentCallFrame.stackTop;}

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