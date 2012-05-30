TEMPLATE = app
CONFIG += console
CONFIG -= qt

LIBS += -lpthread
QMAKE_CXXFLAGS += -pthread

SOURCES += \
    ast_nodes_flow.cpp \
    ast_node_global_def_include.cpp \
    ast_node.cpp \
    ast_internal.cpp \
    ast_nodes_ops.cpp \
    ast_nodes_impl.cpp \
    ast_nodes_func.cpp \
    factory_ast_node_global_def.cpp \
    factory_ast_node.cpp \
    creax_thread.cpp \
    creax_mutex.cpp \
    xmlwriter.cpp \
    Symbols.cpp \
    main.cpp \
    Languages/LanComment.tab.cpp \
    Languages/LanCD.tab.cpp \
    Languages/LanAB.tab.cpp \
    Languages/lex.LanComment_.cpp \
    Languages/lex.LanCD_.cpp \
    Languages/lex.LanAB_.cpp \
    Languages/lex.LanCF_.cpp \
    Languages/LanCF.tab.cpp \
    Languages/parser_class.cpp \
    Languages/lex.LanCC_.cpp \
    Languages/LanCC.tab.cpp \
    cpp_parser.cpp

HEADERS += \
    ast_nodes_flow.hpp \
    ast_node_global_def_include.h \
    ast_node.h \
    ast_internal.hpp \
    auto_ptr_vector.h \
    ast_node_types.hpp \
    ast_nodes_ops.hpp \
    ast_nodes_impl.hpp \
    ast_nodes_func.hpp \
    Global.h \
    factory_ast_node_global_def.h \
    factory_ast_node.h \
    creax_thread.h \
    creax_mutex.h \
    creax_fd.h \
    xmlwriter.h \
    creax_threadfifo.h \
    Symbols.h \
    Languages/LanComment_Context.h \
    Languages/LanComment.ypp \
    Languages/LanComment.l \
    Languages/LanCF_Context.h \
    Languages/LanCF.ypp \
    Languages/LanCF.l \
    Languages/LanCD_Context.h \
    Languages/LanCD.ypp \
    Languages/LanCD.l \
    Languages/LanAB_Context.h \
    Languages/LanAB.ypp \
    Languages/LanAB.l \
    Languages/basic_types.h \
    Languages/LanFS_Context.h \
    Languages/LanFS.ypp \
    Languages/LanFS.l \
    Languages/LanCC_Context.h \
    Languages/LanCC.ypp \
    Languages/LanCC.l \
    Languages/parser_class.h \
    creax_stringinput.h \
    cpp_parser.h \
    creax_filenamepath.h

OTHER_FILES += \
    Languages/Makefile
