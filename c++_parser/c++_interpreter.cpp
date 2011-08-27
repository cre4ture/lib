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
			cpp_type_bib bib(NULL);
			cpp_identifier_bib idBib(NULL);
			cpp_treebuilder builder(parser, bib, idBib);

			while (builder.parse_toplevel())
			{
				std::cout << "parse_toplevel(): " << builder.toString() << std::endl;
			}
		}

		{
			char *example = "typedef int main_result; main_result main(int argc, char* argv[]) { return 0; }";
			cpp_token_parser parser(example);
			cpp_type_bib bib(NULL);
			cpp_identifier_bib idBib(NULL);
			cpp_treebuilder builder(parser, bib, idBib);

			while (builder.parse_toplevel())
			{
				std::cout << "parse_toplevel(): " << builder.toString() << std::endl;
			}
		}

		{
			char *example = "typedef int main_result; main_result main(int argc, char* argv[]) { main_result *i;; return 0; }";
			cpp_token_parser parser(example);
			cpp_type_bib bib(NULL);
			cpp_identifier_bib idBib(NULL);
			cpp_treebuilder builder(parser, bib, idBib);

			while (builder.parse_toplevel())
			{
				std::cout << "parse_toplevel(): " << builder.toString() << std::endl;
			}
		}

		{
			char *example = "typedef int main_result; main_result main(int argc, char* argv[]) { main_result *i; char* name; int nummer; float *f; double* d; return 0; }";
			cpp_token_parser parser(example);
			cpp_type_bib bib(NULL);
			cpp_identifier_bib idBib(NULL);
			cpp_treebuilder builder(parser, bib, idBib);

			while (builder.parse_toplevel())
			{
				std::cout << "parse_toplevel(): " << builder.toString() << std::endl;
			}
		}

		{
			char *example = "typedef int main_result; main_result main(int argc, char* argv[]) { main_result i; char* name; int nummer; float *f; double* d; return i; }";
			cpp_token_parser parser(example);
			cpp_type_bib bib(NULL);
			cpp_identifier_bib idBib(NULL);
			cpp_treebuilder builder(parser, bib, idBib);

			while (builder.parse_toplevel())
			{
				std::cout << "parse_toplevel(): " << builder.toString() << std::endl;
			}
		}

		{
			char *example = "typedef int main_result; main_result* main(int argc, char* argv[]) { main_result i[]; char* name; int nummer; float *f; double* d; return i; }";
			cpp_token_parser parser(example);
			cpp_type_bib bib(NULL);
			cpp_identifier_bib idBib(NULL);
			cpp_treebuilder builder(parser, bib, idBib);

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

