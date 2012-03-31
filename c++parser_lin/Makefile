LL=lex
YY=bison
CC=g++

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

all: compiler

clean:
	@rm -f compiler lex.yy.c minic.tab.cpp minic.tab.hpp

lex.yy.c: minic.l
	@${LL} minic.l

minic.tab.cpp: minic.ypp
	@${YY} --report=solved --report-file=bison_report -d minic.ypp

compiler: minic.tab.cpp lex.yy.c ${CODE_GENERATORS} ${HEADERS}
	@${CC} -g -O0 -Wall -Wno-write-strings minic.tab.cpp lex.yy.c nr2tokens.c ${CODE_GENERATORS} -lfl -o compiler -I ../Zwischencode/
