import std.stdio;
import std.file : readText;

import angel.vm;
import angel.bytes;

import angel.compiler.ast;
import angel.compiler.lexer;
import angel.compiler.parser;
import angel.compiler.compiler;

void main()
{

	Compiler compiler = new Compiler(readText("test.angel"));
	ByteChunk bc = compiler.Compile();

	if (compiler.hadErrors) {
		return;
	}	

	Dissasemble(bc);

	writeln("Executing...");
	AngelVM vm = new AngelVM();

	vm.Run(bc);

	debug {
		writeln( vm.AssertSuccessCount, " Asserts ended up without errors!.");
	}
}
/*
void main() {
	Lexer l = new Lexer(readText("test.angel"));
	for (Token t = l.NextToken(); t.type != TokenType.EndOfFile; t = l.NextToken()) {
		writeln(t);
	}
}*/