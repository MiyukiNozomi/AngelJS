module angel.object;

import angel.bytes;

public enum ObjectType {
    String, Function
}

public abstract class AngelObject {
    public ObjectType type;
    public bool destroyed;

    public this(ObjectType t) {
        this.type = t;
    }

    public string ToString();
}

public class AngelString : AngelObject {
    public string characters;

    public this(string characters) {
        super(ObjectType.String);
        this.characters = characters;
    } 

    public override string ToString() {
        return "\"" ~ characters~"\"";
    }
}

public class Parameter {
    public ValueType paramType;
    public ObjectType objectType;
    public string name;
}

public enum AccessLevel {
    Public, Private, Protected
}

public class AngelFunction : AngelObject {
    public ByteChunk source;
    public string name;
    public Parameter[] parameters;
    public AccessLevel accessLevel;

    public ValueType returnType;
    public ObjectType returnObjType;

    public this() {
        super(ObjectType.Function);
    }

    public override string ToString() {
        return "AngelFunc#" ~name;
    }
}


public class AngelModule {
    
    public string moduleName;
    public Value[string] globals;
    public AngelFunction[string] functions;

    public this(string moduleName) {
        this.moduleName = moduleName;
    }
}

import std.stdio;
public void PrintObject(AngelObject eb) {
    write(eb.ToString());
}