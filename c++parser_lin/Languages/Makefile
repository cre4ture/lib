
all: lex.LanAB_.cpp \
     lex.LanCD_.cpp \
     lex.LanCF_.cpp \
     lex.LanComment_.cpp \
     LanAB.tab.cpp \
     LanAB.tab.hpp \
     LanCD.tab.cpp \
     LanCD.tab.hpp \
     LanCF.tab.cpp \
     LanCF.tab.hpp \
     LanComment.tab.cpp \
     LanComment.tab.hpp

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
	rm -f *.tab.cpp
	rm -f bison_report*.tab.cpp
	rm -f lex.*.cpp

