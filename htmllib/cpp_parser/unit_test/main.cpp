#include "parser.h"
#include <iostream>
#include <fstream>
#include <time.h>

#define TESTFILE
#define ONLY_TIME

int main(int argc, char* argv[])
{
#ifdef TESTFILE
	std::ifstream file("testdata.txt", std::ios_base::binary | std::ios_base::in);
	std::ostringstream data;
	char c[2];
	while (file.good()) { 
		file.read(c, 1);
		c[1] = 0;
		if (file.good())
			data << c;
	}

	std::string strdata = data.str();
	creax::htmlparser parser(strdata.c_str());
#else
	std::string strdata = std::string("Edit1<test name=\"wert\" ohne=quote>bLA "
		"Edit1<test2 name=\"wert\" ohne=quote/>bLA"
		"Edit1<test name=wert ohne=\"quote\">bLA"
		"Edit1<test name=wert ohne=\"quote\"/>bLA"
		"Edit1<test><test/>bLA"
		"Edit1<test ><test />bLA"
		"Edit1</test></test>bLA"
		"<!-- bla --><![CDATA[ <test> ]]>");
	creax::htmlparser parser(strdata.c_str());
#endif

#ifndef ONLY_TIME
	while (parser.parse()) {
		switch (parser.curTagType) {
		case creax::tt_StartTag:
		case creax::tt_EmptyTag:
			parser.initpAttrs();

			std::cout << "<[" << parser.curTagName << "]";
			for (size_t i = 0; i < parser.curTagAttributes.size(); i++) {
				std::cout << " [" << parser.curTagAttributes[i]->name << 
					"]=[" << parser.curTagAttributes[i]->value << "]";
			}
			if (parser.curTagType == creax::tt_EmptyTag) 
				std::cout << "/";
			std::cout << ">" << std::endl;
			break;
		case creax::tt_EndTag:
			std::cout << "</[" << parser.curTagName << "]>" << std::endl;
			break;
		case creax::tt_Content:
			std::cout << "content: [" << parser.curContent << "]" << std::endl;
			break;
		case creax::tt_Comment:
			std::cout << "comment: [" << parser.curContent << "]" << std::endl;
			break;
		case creax::tt_CDATA:
			std::cout << "CDATA: [" << parser.curContent << "]" << std::endl;
			break;
		}
	}

#endif

	{
		creax::htmlparser parser2(strdata.c_str());
		clock_t start = clock();
		int c = 0;
		while (parser2.parse()) { c++; }
		clock_t duration = clock() - start;
		std::cout << "time for testfile(" << c << " elements): " << ((double)duration / CLOCKS_PER_SEC) << std::endl;
	}

	return 0;
}