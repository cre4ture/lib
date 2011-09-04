#pragma once

#include "html_parser.h" // for myOStrStream
#include "auto_ptr_map.h"
#include "auto_ptr_vector.h"

#define SYMBOL_CHAR_CASE \
	';':\
	case '.':\
	case ',':\
	case '(':\
	case ')':\
	case '[':\
	case ']':\
	case '{':\
	case '}':\
	case '-':\
	case '+':\
	case '*':\
	case '/':\
	case '&':\
	case '#':\
	case '<':\
	case '>':\
	case '"':\
	case '%':\
	case '='

/* this macro can be used to disable the default
   constructor, the copy constructor and copy-operator */
#define UNDECLARE_DEFAULT(NAME) \
	private: \
		NAME(); \
		NAME(const NAME &a); \
		NAME& operator = (const NAME &a)

namespace creax {

	enum cpp_token_type 
	{ 
		ctt_start,
		ctt_special_char,
		ctt_name,
		ctt_const,
		ctt_end
	};

	class cpp_token_parser
	{
		UNDECLARE_DEFAULT(cpp_token_parser);
	private:
		const char *code_start;
		const char *cpos;
		bool unread_last_token;

		void getTockenTillDelimitedCharInLine(myOStrStream &token, const char delimiter)
		{
			// skip spaces at start
			while (*cpos != 0) {
				switch (*cpos) {
				case 13: // return
				case 10: // return
					throw std::runtime_error(std::string("getTockenTillDelimitedCharInLine: End of line reached while searching for: ") + delimiter);
					break; // just ignore
				case '"': // typicall delimiter
				case '\'': // typicall delimiter
				case '>': // typicall delimiter
					if (*cpos == delimiter)
					{
						cpos++;
						return;
					}
					// fallthrough is intended!
				default:
					token << *cpos;
				}
				cpos++;
			}
			throw std::runtime_error(std::string("getTockenTillDelimitedCharInLine: End of File reached while searching for: ") + delimiter);
		}

		cpp_token_type getNextToken(myOStrStream &token)
		{
			// skip spaces at start
			while (*cpos != 0) {
				switch (*cpos) {
				case 9: // tab
				case 13: // return
				case 10: // return
				case ' ':
					break; // just ignore
				default:
					goto token_start;
				}
				cpos++;
			}
			token_str = "End Of File";
			return ctt_end;

token_start:
			// read token

			switch (*cpos) {
			case SYMBOL_CHAR_CASE:
				token << *cpos;
				cpos++;
				goto token_spec_char_end;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				token << *cpos;
				cpos++;
				return ctt_const; 
			default:; // nothing
			}

			// read token name
			while (*cpos != 0) {
				switch (*cpos) {
				case 9: // tab
				case 13: // return
				case ' ':
				case SYMBOL_CHAR_CASE:
					goto token_name_end;
				default:
					token << *cpos;
				}
				cpos++;
			}
token_name_end:
			return ctt_name;
token_spec_char_end:
			return ctt_special_char;

		}

	public:

		std::string token_str;
		cpp_token_type token_type;

		cpp_token_parser(const char* code)
			: code_start(code),
			  cpos(code),
			  token_type(ctt_start),
			  unread_last_token(false)
		{
	
		}

		~cpp_token_parser()
		{

		}

		cpp_token_type parse()
		{
			if (unread_last_token)
			{
				unread_last_token = false;
				return token_type;
			}

			if (token_type == ctt_end)
				throw std::runtime_error("Unexpected end of file");

			myOStrStream token;
			token_type = getNextToken(token);
			token_str = token.str();
			return token_type;
		}

		cpp_token_type parse_include_filename(const char delimiter)
		{
			if (token_type == ctt_end)
				throw std::runtime_error("Unexpected end of file");

			myOStrStream token;
			getTockenTillDelimitedCharInLine(token, delimiter);
			token_type = ctt_const;
			token_str = token.str();
			return token_type;
		}

		void back_parse()
		{
			if (!unread_last_token)
			{
				unread_last_token = true;
			}
			else
			{
				throw std::runtime_error("internal error: unread already done!");
			}
		}
	};

	enum cpp_keyword {
		ckw_class,
//		ckw_void,
		ckw_return,
		ckw_static,
		ckw_for,
		ckw_do,
		ckw_while,
		ckw_typedef,
		ckw_none // this must be the last!!
	};

	const char* cpp_keywords[] = {
		"class", 
// 		"void", no keyword! it is a (special kind of) type!
		"return",
		"static",
		"for",
		"do",
		"while",
		"typedef",
		NULL // this must be the last!!
	};

	cpp_keyword identifyKeyword(const std::string &name)
	{
		for (size_t i = 0; i < ckw_none; i++)
		{
			if (name == cpp_keywords[i])
				return (cpp_keyword)i;
		}
		return ckw_none;
	}

}