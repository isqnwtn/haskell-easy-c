compilerfiles = Compiler/Grammar.hs Compiler/Tokens.hs
libfiles = Calc.hs lib/Translate.hs

all: Calc

Compiler/Tokens.hs : Compiler/Tokens.x
	alex $^ -o $@

Compiler/Grammar.hs : Compiler/Grammar.y
	happy $^ -o $@
    
Calc : ${compilerfiles} ${libfiles}
	ghc --make Calc

c:
	rm -f Grammar.hs Tokens.hs *.o *.hi
    
clean:
	rm -f Calc ${compilerfiles} *.o *.hi Compiler/*.o Compiler/*.hi Lib/*.o Lib/*.hi
    
