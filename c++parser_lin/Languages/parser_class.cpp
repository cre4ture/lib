#include "parser_class.h"

#include "LanCC_Context.h"

void LanXX_Context::class_decl_body(const std::string& block_code)
{
    std::cout << "[class]" << c_name << ": ";
    for (int i = 0; i < inheritance.size(); i++)
    {
        std::cout << inheritance[i];
    }
    std::cout << std::endl;

    creax::threadfifo<code_piece> fifo;
    fifo.push_data(code_piece(block_code, blockstart));
    fifo.close_fifo();
    LanCC_Context tmp(fifo, blockstart, c_name, parent);
    LanCC_parse(&tmp);
}

