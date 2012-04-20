TEMPLATE = app
CONFIG += console
CONFIG -= qt

CONFIG += link_pkgconfig
PKGCONFIG += opencv

SOURCES += main.cpp \
    creax_extern_model.cpp

HEADERS += \
    creax_extern_model.h \
    ../compiler_export/Sprachumfang/creax_fd.h \
    ../compiler_export/Sprachumfang/creax_mutex.h

