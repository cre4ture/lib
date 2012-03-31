#!/bin/sh
grep  \#define y.tab.h | awk '
BEGIN { print "char * nr2token (int nr){";
        print "  switch (nr) {";
      }
      {
        print "    case "$3" : return \""$2"\";"
      }
END   { print "  }"
        print " return \"unknown symbol\";"
        print "}"
      }
' > nr2tokens.cpp

