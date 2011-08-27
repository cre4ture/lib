// c++_interpreter.cpp : Definiert den Einstiegspunkt für die Konsolenanwendung.
//

#include "stdafx.h"
#include "c++_parser.h"

using namespace creax;

int _tmain(int argc, _TCHAR* argv[])
{
	try {
	
		{
			char *example = "int main(int argc, char* argv[]) { return 0; }";
			cpp_token_parser parser(example);

			while (parser.parse() != ctt_end)
			{
				std::cout << "parse():" << parser.token_str << std::endl;
			}
		}
	
		{
			char *example = "int main(int argc, char* argv[]) { return 0; }";
			cpp_token_parser parser(example);
			cpp_treebuilder builder(parser);

			while (builder.parse_toplevel())
			{
				std::cout << "parse_toplevel(): " << builder.toString() << std::endl;
			}
		}

	} catch (std::runtime_error &e)
	{
		std::cout << "Runtime Error: " << e.what() << std::endl;
		return -1;
	}

	return 0;
}

