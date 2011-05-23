#include "parser.h"
#include <iostream>
#include <fstream>

//#define TESTFILE

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
	creax::htmlparser parser(
		"Edit1<test name=\"wert\" ohne=quote>bLA "
		"Edit1<test2 name=\"wert\" ohne=quote/>bLA"
		"Edit1<test name=wert ohne=\"quote\">bLA"
		"Edit1<test name=wert ohne=\"quote\"/>bLA"
		"Edit1<test><test/>bLA"
		"Edit1<test ><test />bLA"
		"Edit1</test></test>bLA"
		"<!-- bla --><![CDATA[ <test> ]]>");
#endif

	while (parser.parse()) {
		switch (parser.curTagType) {
		case creax::tt_StartTag:
		case creax::tt_EmptyTag:
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

	return 0;
}