LL=lex
YY=bison
CC=g++

CFLAGS=-g -O0 -Wall -Wno-write-strings

CODE_GENERATORS=Symbols.cpp \
                ast_nodes_func.cpp \
                ast_nodes_impl.cpp \
                ast_nodes_flow.cpp \
                ast_nodes_ops.cpp \
                ast_internal.cpp \
                main.cpp

HEADERS=ast_nodes_flow.hpp \
        ast_node.h \
        ast_nodes_func.hpp \
        ast_nodes_impl.hpp \
        xmlwriter.h \
        ast_nodes_ops.hpp

OBJECTS=ast_node_global_def_include.o \
        factory_ast_node_global_def.o \
        factory_ast_node.o

all: compiler

clean:
	@rm -f compiler lex.yy.c minic.tab.cpp minic.tab.hpp *.o

ast_node_global_def_include.o: ast_node_global_def_include.cpp ast_node_global_def_include.h
	@${CC} ${CFLAGS} -o ast_node_global_def_include.o -c ast_node_global_def_include.cpp

factory_ast_node_global_def.o: factory_ast_node_global_def.cpp factory_ast_node_global_def.h
	@${CC} ${CFLAGS} -o factory_ast_node_global_def.o -c factory_ast_node_global_def.cpp

factory_ast_node.o: factory_ast_node.cpp factory_ast_node.h
	@${CC} ${CFLAGS} -o factory_ast_node.o -c factory_ast_node.cpp

lex.yy.c: minic.l
	@${LL} minic.l

minic.tab.cpp: minic.ypp
	@${YY} --report=solved --report-file=bison_report -d minic.ypp

compiler: minic.tab.cpp lex.yy.c ${CODE_GENERATORS} ${HEADERS} ${OBJECTS}
	@${CC} ${CFLAGS} minic.tab.cpp lex.yy.c nr2tokens.c ${CODE_GENERATORS} ${OBJECTS} -lfl -o compiler -I ../Zwischencode/
