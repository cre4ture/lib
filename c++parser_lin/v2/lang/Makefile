
SOURCES = $(wildcard *.l)
OBJECTS = $(SOURCES:.l=.tab.cpp)
OBJECTS_FLEX = $(SOURCES:%.l=lex.%_.cpp)

all: $(OBJECTS) $(OBJECTS_FLEX)

# define a suffix rule for .ypp -> .tab.cpp
%.tab.cpp %.tab.hpp : %.ypp
	@bison $< --report=solved --report-file=bison_report_$<

# define a suffix rule for .l -> _.c
lex.%_.c : %.l
	@flex $<

# define a suffix rule for _.c -> _.cpp
lex.%_.cpp : lex.%_.c 
	@cp $< $<pp

.PHONY: clean
clean:
	rm -f *.tab.cpp *.tab.hpp
	rm -f bison_report*.tab.cpp
	rm -f lex.*.cpp

