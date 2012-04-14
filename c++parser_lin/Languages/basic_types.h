#ifndef BASIC_TYPES_H
#define BASIC_TYPES_H

class code_piece
{
public:
    int line;
    std::string code;

    code_piece()
    {}

    code_piece(const std::string& a_code, int a_line)
        : line(a_line), code(a_code)
    {}

    code_piece(const code_piece& a)
        : line(a.line), code(a.code)
    {}

};

class text_type
{
public:
    std::string text;
    int type;
    int line;

    text_type()
    {}

    text_type(const std::string& a_text, int a_type, int a_line)
        : text(a_text), type(a_type), line(a_line)
    {}

    text_type(const text_type& a)
        : text(a.text), type(a.type), line(a.line)
    {}
};


#endif // BASIC_TYPES_H
