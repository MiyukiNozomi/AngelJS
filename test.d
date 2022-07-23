module test;

import std.stdio;

union test {
    int a;
    float b;
    bool c;
    string d;
}

void main() {
    test a = test();
    test b = test();
    test c = test();
    test d = test();

    a.a = 1;
    b.b = 2.51314114f;
    c.c = false;
    d.d = "Somewhat a long string, their sizes in memory should be different for each one of these instances.";

    writeln("size of a: ", a.sizeof);
    writeln("size of b: ", b.sizeof);
    writeln("size of c: ", c.sizeof);
    writeln("size of d: ", d.sizeof);
}