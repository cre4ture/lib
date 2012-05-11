TEMPLATE = app
CONFIG += console
CONFIG -= qt

LIBS += -fopenmp
QMAKE_CXXFLAGS += -fopenmp

SOURCES += main.cpp \
    shared_object.cpp

HEADERS += \
    shared_object.h

