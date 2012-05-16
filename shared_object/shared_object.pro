TEMPLATE = app
CONFIG += console
CONFIG -= qt

LIBS += -fopenmp
QMAKE_CXXFLAGS += -fopenmp

SOURCES += main.cpp \
    shared_object.cpp \
    shared_data.cpp

HEADERS += \
    shared_object.h \
    shared_data.h \
    shared_object_ext.h

