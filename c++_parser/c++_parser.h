#pragma once

#include "parser.h"
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
	case '/'

namespace creax {

	enum cpp_token_type { ctt_start, ctt_special_char, ctt_name, ctt_end };

	class cpp_token_parser
	{
	private:
		const char *code_start;
		const char *cpos;
		bool unread_last_token;

		cpp_token_type getNextToken(myOStrStream &token)
		{
			// skip spaces at start
			while (*cpos != 0) {
				switch (*cpos) {
				case 9: // tab
				case 13: // return
				case ' ':
					break; // just ignore
				default:
					goto token_start;
				}
				cpos++;
			}
			return ctt_end;

token_start:
			// read token

			switch (*cpos) {
			case SYMBOL_CHAR_CASE:
				token << *cpos;
				cpos++;
				goto token_spec_char_end;
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

	class cpp_type
	{
	public:
		std::string name;
		cpp_type *ptr_type;
	};

	class cpp_ptr_type: public cpp_type
	{
	public:
		const cpp_type *ztype;

		cpp_ptr_type(const cpp_type *aZtype)
			:ztype(aZtype)
		{
			name = name + '*';
		}
	};

	const cpp_type typeList[] = {
		"char", NULL,
		"bool", NULL,
		"int", NULL,
		"", NULL
	};

	class cpp_type_bib
	{
	private:
		const cpp_type_bib *parent;
		mefu::auto_ptr_vector<cpp_type> types;

	public:
		cpp_type_bib(cpp_type_bib *a_parent)
			:parent(a_parent)
		{

		}

		~cpp_type_bib()
		{

		}

		void addType(cpp_type *atype)
		{
			types.push_back(atype);
		}

		cpp_type* getPtrTypeOf(cpp_type *atype)
		{
			if (atype->ptr_type)
			{
				return atype->ptr_type;
			}
			else
			{
				cpp_ptr_type *ptr = new cpp_ptr_type(atype);
				addType(ptr);
				return ptr;
			}
		}

		cpp_type* identifyType(std::string name)
		{
			for (size_t i = 0; i < types.size(); i++)
			{
				if (name == types[i]->name)
				{
					return types[i];
				}
			}
			return NULL;
		}
	};

	enum cpp_builder_elementtype
	{
		cbe_none,
		cbe_function_implementation,
		cbe_function_prototype
	};

	enum cpp_command_type
	{
		cct_none,
		cct_return
	};

	const char* cmd_type_list[] = {
		"none",
		"return"
	}; 

	class cpp_command_parser
	{
	private:
		cpp_token_parser &parser;

	public:
		cpp_command_parser(cpp_token_parser &a_parser)
			:parser(a_parser)
		{
			
		}

		~cpp_command_parser()
		{
			
		}

		cpp_command_type cmd_type;
		std::string cmd_object;

		bool parse()
		{
			switch (parser.parse())
			{
			case ctt_name:
				// zuweisung, funktionsaufruf, variablendeklaration, schleifenstart, ...
				{
					cpp_keyword kw = identifyKeyword(parser.token_str);
					switch (kw)
					{
					case ckw_return:
						cmd_type = cct_return;
						switch (parser.parse())
						{
						case ctt_special_char:
							{
								switch (parser.token_str[0])
								{
								case ';':
									// return nothing
									cmd_object = "";
									break;
								default:
									throw std::runtime_error("Unexpected: " + parser.token_str);
								}
							}
							cmd_object = "";
							break;
						case ctt_name:
							// value to be returned TODO: read full cmd
							if (parser.token_str == "0")
							{
								cmd_object = "0";
							}
							else
							{
								throw std::runtime_error("Unsupported returnvalue: " + parser.token_str);
							}
							break;
						default:
							throw std::runtime_error("Unexpected: " + parser.token_str);
						}
						break;
					default:
						throw std::runtime_error("Unexpected: " + parser.token_str);
					}
				}
				break;
			case ctt_special_char:
				{
					switch (parser.token_str[0])
					{
					case ';':
						cmd_type = cct_none;
						break;
					case '}':
						// end of command block reached
						return false;
					}
				}
				break;
			default:
				throw std::runtime_error("Unexpected: " + parser.token_str);
			}

			return true;
		}
	};

	class cpp_cmd
	{
	private:
		std::string typetostr()
		{
			return cmd_type_list[tp];
		}

	public:
		cpp_command_type tp; 
		std::string op;
		std::string name;

		std::string toString()
		{
			std::ostringstream ostr;
			ostr << "cmd: '" << name << "':" << typetostr() << " op:" << op;
			return ostr.str();
		}
	};

	class cpp_treebuilder 
	{
	private:
		cpp_token_parser &parser;

		const cpp_type *identifyType(const std::string &name)
		{
			for (size_t i = 0; typeList[i].name.length() != 0; i++)
			{
				if (name == typeList[i].name)
					return &typeList[i];
			}
			return NULL;
		}

		struct cpp_function_parameter
		{
			const cpp_type *tp;
			std::string name;
		};

		std::vector<cpp_function_parameter> func_params;

		void readFunctionParameter()
		{
			func_params.clear();
			while (true) 
			{
				cpp_token_type tp = parser.parse();
				switch (tp)
				{
				case ctt_name:
					// typname
					{
						cpp_function_parameter param;
						param.tp = identifyType(parser.token_str);
						if (param.tp != NULL)
						{
							// read * if exists
							cpp_token_type ctp = parser.parse();
							if (ctp == ctt_special_char)
							{
								switch (parser.token_str[0])
								{
								case '*':
									// pointer type
									param.tp = new cpp_ptr_type(param.tp); // TODO: memory leak -> need better type management!!!
									ctp = parser.parse();
									break;
								case '&':
									// reference type
									// TODO
									ctp = parser.parse();
									break;
								default:
									throw std::runtime_error("Unexpected: " + parser.token_str);
								}
							}

							// read name
							switch (ctp)
							{
							case ctt_name:
								// got name
								param.name = parser.token_str;
								{
									switch (parser.parse())
									{
									case ctt_special_char:
										// [] or , or )
										{
											// handle []
											switch (parser.token_str[0]) 
											{
											case '[':
												// pointer type TODO
												{
													parser.parse();
													if (parser.token_type != ctt_special_char || 
														parser.token_str[0] != ']')
													{
														throw std::runtime_error("Unexpected: " + parser.token_str);
													}
													parser.parse();
												}
												break;
											}


											switch (parser.token_str[0])
											{
											case ',':
												// param finished, read next!
												func_params.push_back(param);
												break;
											case ')':
												// param finished, function finished!
												func_params.push_back(param);
												return;
											default:
												throw std::runtime_error("Unexpected: " + parser.token_str);
											}
										}
										break;
									default:
										throw std::runtime_error("Unexpected " + parser.token_str);
									}
								}
								break;
							default:
								throw std::runtime_error("Unexpected: " + parser.token_str);
							}
						}
						else
						{
							throw std::runtime_error("Unknown type: " + parser.token_str);
						}
					}
					break;
				default:
					throw std::runtime_error("Unexpected " + parser.token_str);
				}
			}
		}

	public:

		cpp_builder_elementtype element_tp;
		std::vector<cpp_cmd> impl_cmds;

		std::string toString()
		{
			switch (element_tp)
			{
			case cbe_function_prototype:
				return "function prototype";
			case cbe_function_implementation:
				{
					std::ostringstream ostr;
					ostr << "{" << std::endl;
					for (size_t i = 0; i < impl_cmds.size(); i++)
					{
						ostr << impl_cmds[i].toString() << std::endl;
					}
					ostr << "}";
					return "function implementation: " + ostr.str();
				}
			}
			return "";
		}

		cpp_treebuilder(cpp_token_parser &a_parser)
			: parser(a_parser)
		{
		}

		bool parse_toplevel()
		{
			cpp_token_type tp = parser.parse();

			switch (tp) 
			{
			case ctt_end:
				// on this position it is ok to get the end of file!
				return false; // no more elements
			case ctt_special_char:
				throw std::runtime_error("Unexpected special char");
			case ctt_name:
				// must be a type or an keyword like "class", "typedef", "volatile", ...
				{
					cpp_keyword key = identifyKeyword(parser.token_str);
					switch (key)
					{
					case ckw_class:
						// class definition or prototype!
						// TODO
						break;
					case ckw_return:
						// return keyword is not allowed here!
						throw std::runtime_error("Return not allowed here!");
					case ckw_none:
						// no keyword => so we must have a type!
						{
							const cpp_type* tp = identifyType(parser.token_str);
							if (tp != NULL)
							{
								// valid type, now get the name of the variable or funktion, ...
								cpp_token_type ttp = parser.parse();
								switch (ttp)
								{
								case ctt_name:
									{
										std::string name = parser.token_str;
										cpp_token_type ttp = parser.parse();
										switch (ttp)
										{
										case ctt_special_char:
											{
												switch (parser.token_str[0])
												{
												case ';':
													// we have a simple variable defined! End Reached!
													break;
												case '=':
													// we have a simple variable defined. But there is an initialisation value to be parsed!
													// TODO
													break;
												case '(':
													// we have a funktion implementation or prototype!
													readFunctionParameter();
													// read if prototype or not:
													parser.parse();
													switch (parser.token_type)
													{
													case ctt_special_char:
														{
															switch (parser.token_str[0])
															{
															case ';':
																element_tp = cbe_function_prototype;
																return true;
																break;
															case '{':
																element_tp = cbe_function_implementation;
																{
																	cpp_command_parser cmdparser(parser);
																	while (cmdparser.parse())
																	{
																		cpp_cmd acmd;
																		acmd.name = "";
																		acmd.op = cmdparser.cmd_object;
																		acmd.tp = cmdparser.cmd_type;
																		impl_cmds.push_back(acmd);
																	}
																}
																break;
															case ':': // initialise list TODO
																break;
															default:
																throw std::runtime_error("Unexpected: " + parser.token_str);
															}
														}
														break;
													default:
														throw std::runtime_error("Unexpected: " + parser.token_str);
													}
													break;
												case '[':
													// we have an array!
													// TODO
													break;
												default:
													throw std::runtime_error("Unexpected '" + parser.token_str + "'. We need a ';' or something like that!");
												}
											}
											break;
										default:
											throw std::runtime_error("Unexpected '" + parser.token_str + "'. We need a ';' or something like that!");
										}
									}
									break;
								default:
									throw std::runtime_error("Unexpected '" + parser.token_str + "'. We need a name!");
								}
							}
							else
							{
								throw std::runtime_error("Unknown Type: " + parser.token_str);
							}
						}
						break;
					default:
						// internal error
						throw std::runtime_error("Internal Error: Unknown keyword: " + parser.token_str);
					}
				}
				break;
			default:
				throw std::runtime_error("Internal Error: Unknown token type!");
			};

			return true;
		}
	};

}