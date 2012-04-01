# -------------------------------------------------
# Project created by QtCreator 2012-02-08T19:07:46
# -------------------------------------------------
QT -= core \
    gui
TARGET = Sprachumfang
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += ../Codeerzeugung/ast_nodes_asm_func.cpp \
    ../Codeerzeugung/ast_nodes_asm_flow.cpp \
    ../Codeerzeugung/ast_nodes_asm.cpp \
    ../Codeerzeugung/ast_nodes_asm_vector.cpp \
    ../Codeerzeugung/ast_nodes_asm_math.cpp \
	Symbols.cpp \
    ast_nodes_impl.cpp \
    ast_nodes_func.cpp \
    ast_nodes_flow.cpp \
    ast_nodes_ops.cpp \
    ast_internal.cpp \
    test_uli_vector.c \
    test_uli.c \
    test_normal.c \
    test.c \
    ast_node.cpp \
    xmlnode.cpp \
    xmlwriter.cpp \
    ../Codeerzeugung/main.cpp \
    main.cpp \
    ast_node_global_def_include.cpp \
    factory_ast_node_global_def.cpp \
    factory_ast_node.cpp
HEADERS += \
    ast_internal.hpp \
    Symbols.h \
    nr2tokens.h \
    Global.h \
    ast_node_types.hpp \
    ast_nodes_impl.hpp \
    ast_nodes_flow.hpp \
    ast_nodes_func.hpp \
    ast_nodes_ops.hpp \
    ast_node.h \
    auto_ptr_vector.h \
    xmlnode.h \
    xmlwriter.h \
    xmlparser/html_parser.h \
    ast_node_global_def_include.h \
    factory_ast_node_global_def.h \
    factory_ast_node.h
OTHER_FILES += minic.ypp \
    minic.l \
    Makefile