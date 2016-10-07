#test2: 

PLSQL3Parser.class :PLSQL3Parser.java PLSQL3.tokens PLSQL3Lexer.java
	javac \
	-cp /usr/share/java/antlr3-3.2.jar:/usr/share/java/antlr3-runtime-3.2.jar \
	PLSQL3Parser.java  PLSQL3Lexer.java

PLSQL3Parser.java : PLSQL3.g 
	antlr3.2 PLSQL3.g



