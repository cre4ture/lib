// c++_interpreter.cpp : Definiert den Einstiegspunkt für die Konsolenanwendung.
//

#include "stdafx.h"
#include "cpp_treebuilder.h"

#include <fstream>

using namespace creax;

void getFileContent(const std::string filename, std::string &content)
{
	const int BUFF_SIZE = 255;
	char buffer[BUFF_SIZE+1];
	FILE *fp;
	fopen_s(&fp, filename.c_str(), "rb");
	if (fp == NULL)
	{
		throw std::runtime_error("getFileContent(): Couldn't open file: " + filename);
	}

	int rd_cnt = 0;
	while ((rd_cnt = fread(buffer, 1, BUFF_SIZE, fp)) > 0)
	{
		buffer[rd_cnt] = 0;
		content += std::string(buffer);
	}
	fclose(fp);
}

class verbose
{
	UNDECLARE_DEFAULT(verbose);
public:
	std::string name;

	verbose(std::string &a_name)
	{
		name = a_name;
		std::cout << "verbose(" << name << ");" << std::endl;
	}

	~verbose()
	{
		std::cout << "~verbose(" << name << ");" << std::endl;
	}
};

std::auto_ptr<verbose> createVerbose(std::string name)
{
	std::auto_ptr<verbose> ptr(new verbose(name));
	if (name == "explode") throw std::runtime_error("createVerbose: explode");
	return ptr;
}

int _tmain(int argc, _TCHAR* argv[])
{

	// test

	try {

		std::cout << "vorher"  << std::endl;

		std::auto_ptr<verbose> ptr = createVerbose("test");

		std::cout << "nachher"  << std::endl;

	} catch (std::runtime_error &e)
	{
		std::cout << "Runtime Error: " << e.what() << std::endl;
		return -1;
	}

	std::cout << "ende" << std::endl;

	//return 0;

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

		{/*
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
		*/}

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

		{
			char *example = "bool isZero(int i); typedef int main_result; main_result* main(int argc, char* argv[]) { main_result i[]; char* name; int nummer; float *f; double* d; isZero(nummer); return i; }";
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
			char *example = "bool isZero(int i); int getResultCount(); typedef int main_result; int main(int argc, char* argv[]) { main_result i[]; char* name; int nummer; float *f; double* d; isZero(nummer); return getResultCount(); }";
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
			char *example = "bool isZero(int i); int* getResultCount(); typedef int main_result; int main(int argc, char* argv[]) { main_result i[]; char* name; int nummer; float *f; double* d; isZero(nummer); return *getResultCount(); }";
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
			char *example = "bool isZero(int i); int* getResultCount(); typedef int main_result; int main(int argc, char* argv[]) { main_result i[]; char* name; int nummer; float *f; double* d; isZero(nummer); return *getResultCount(); }";
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
			char *example = "int global_int; bool isZero(int i); int* getResultCount(); typedef int main_result; int* main(int argc, char* argv[]) { main_result i[]; char* name; int nummer; float *f; double* d; isZero(nummer); return &global_int; }";
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
			std::string content;
			getFileContent("main.cpp", content);
			cpp_token_parser parser(content.c_str());
			cpp_type_bib bib(NULL);
			cpp_identifier_bib idBib(NULL);
			cpp_treebuilder builder(parser, bib, idBib);

			while (builder.parse_toplevel())
			{
				std::cout << "parse_toplevel(): " << builder.toString() << std::endl;
				switch (builder.element_tp)
				{
				case cbe_preproc_include_lib:
				case cbe_preproc_include_src:
					{
						
					}
					break;
				}
			}
		}

	} catch (std::runtime_error &e)
	{
		std::cout << "Runtime Error: " << e.what() << std::endl;
		return -1;
	}

	return 0;
}

