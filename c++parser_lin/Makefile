LL=lex
YY=bison
CC=g++

CFLAGS=-g -O0 -Wall -Wno-write-strings -pthread

CODE_GENERATORS=Symbols.cpp \
		ast_nodes_func.cpp \
		ast_nodes_impl.cpp \
		ast_nodes_flow.cpp \
		ast_nodes_ops.cpp \
		ast_internal.cpp

HEADERS=ast_nodes_flow.hpp \
	ast_node.h \
	ast_nodes_func.hpp \
	ast_nodes_impl.hpp \
	xmlwriter.h \
	ast_nodes_ops.hpp

OBJECTS=ast_node_global_def_include.o \
	factory_ast_node_global_def.o \
	factory_ast_node.o \
	main.o lex.LanAB_.o LanAB.tab.o lex.LanCD_.o LanCD.tab.o

compiler: ${OBJECTS}
	@${CC} ${CFLAGS} -o compiler ${CODE_GENERATORS} ${OBJECTS}

ast_node_global_def_include.o: ast_node_global_def_include.cpp ast_node_global_def_include.h
	@${CC} ${CFLAGS} -o ast_node_global_def_include.o -c ast_node_global_def_include.cpp

factory_ast_node_global_def.o: factory_ast_node_global_def.cpp factory_ast_node_global_def.h
	@${CC} ${CFLAGS} -o factory_ast_node_global_def.o -c factory_ast_node_global_def.cpp

factory_ast_node.o: factory_ast_node.cpp factory_ast_node.h
	@${CC} ${CFLAGS} -o factory_ast_node.o -c factory_ast_node.cpp

main.o: main.cpp
	@${CC} ${CFLAGS} -c -o main.o main.cpp

lex.LanAB_.o: lex.LanAB_.c LanAB.tab.h
	@${CC} ${CFLAGS} -c -o lex.LanAB_.o lex.LanAB_.c

lex.LanAB_.c: LanAB.l
	flex LanAB.l

LanAB.tab.o: LanAB.tab.c
	@${CC} ${CFLAGS} -c -o LanAB.tab.o LanAB.tab.c

LanAB.tab.c LanAB.tab.h: LanAB.y
	bison LanAB.y

lex.LanCD_.o: lex.LanCD_.c LanCD.tab.h
	@${CC} ${CFLAGS} -c -o lex.LanCD_.o lex.LanCD_.c

lex.LanCD_.c: LanCD.l
	flex LanCD.l

LanCD.tab.o: LanCD.tab.c
	@${CC} ${CFLAGS} -c -o LanCD.tab.o LanCD.tab.c

LanCD.tab.c LanCD.tab.h: LanCD.y
	bison LanCD.y --report=solved --report-file=bison_report

.PHONY: clean
clean:
	rm -f compiler *.o
	rm -f LanAB.tab.c LanAB.tab.h
	rm -f lex.LanAB_.c lex.LanAB_.o
	rm -f LanCD.tab.c LanCD.tab.h
	rm -f lex.LanCD_.c lex.LanCD_.o
