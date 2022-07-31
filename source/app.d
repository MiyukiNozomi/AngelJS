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
	ModuleCompiler cc = new ModuleCompiler("test");
	AngelModule am = cc.CompileModule("test.angel");
	ModuleCompiler stdcc = new ModuleCompiler("stdcpp");
	AngelModule stdam = stdcc.CompileModule("stdcpp.angel");

	AngelVM avm = new AngelVM();
	avm.AddModule(am.moduleName, am);
	avm.AddModule(stdam.moduleName, stdam);
	
	avm.Invoke("test","main", 0);
/*
	foreach (string s ; am.functions.byKey()) {
		AngelFunction f = am.functions[s];
		writeln(f.accessLevel, " #function '",f.name,"' : ", f.returnType, " | Otype#", f.returnObjType);
		for(int j = 0; j < f.parameters.length; j++) {
			Parameter p = f.parameters[j];
			writeln("   Param #",j, "> ", p.name ~ " : ", p.paramType, " | Otype#", p.objectType);
		}
		writeln("bytecode----");
		Dissasemble(f.source);
	}*/
/*
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
	}*/
}
/*
void main() {
	Lexer l = new Lexer(readText("test.angel"));
	for (Token t = l.NextToken(); t.type != TokenType.EndOfFile; t = l.NextToken()) {
		writeln(t);
	}
}*/