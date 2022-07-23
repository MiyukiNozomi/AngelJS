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

    public int inStackID;
}

public class AngelFunction : AngelObject {
    public ByteChunk func;
    public string name;
    public Parameter[] parameters;

    public Value[256] stack;
    public Value[] allocLets;

    public this() {
        super(ObjectType.Function);
    }

    public override string ToString() {
        return "AngelFunc#" ~name;
    }
}

import std.stdio;
public void PrintObject(AngelObject eb) {
    write(eb.ToString());
}