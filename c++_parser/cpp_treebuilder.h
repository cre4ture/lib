#pragma once

#include "cpp_command_builder.h"
#include "cpp_identifier.h"

namespace creax {

	class cpp_treebuilder
	{
		UNDECLARE_DEFAULT(cpp_treebuilder);
	private:
		cpp_token_parser &parser;
		cpp_type_bib &typeBib;
		cpp_identifier_bib &idBib;
		cpp_operator_bib &op_bib;

		cpp_type* identifyType(const std::string &name)
		{
			return typeBib.identifyType(name);
		}

		struct cpp_function_parameter
		{
			cpp_type *tp;
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
									param.tp = typeBib.getPtrTypeOf(param.tp);
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
				case ctt_special_char:
					if (parser.token_str == ")")
					{
						// no parameters!
						return;
					}
					// no break -> fallthrough ! same exception!
				default:
					throw std::runtime_error("Unexpected " + parser.token_str);
				}
			}
		}

		void toplevel_parse_typedef()
		{
			cpp_type* ztp = NULL;

			parser.parse(); // original typ
			switch (parser.token_type)
			{
			case ctt_name:
				{
					ztp = typeBib.identifyType(parser.token_str);
				}
				break;
			default:
				throw std::runtime_error("Typedef: expected type, NOT: " + parser.token_str);
			}
			
			if (ztp == NULL)
			{
				throw std::runtime_error("Unkown type in typedef: " + parser.token_str);
			}

			parser.parse(); // alias name
			switch (parser.token_type)
			{
			case ctt_name:
				{
					cpp_type_typedef *nt = new cpp_type_typedef(ztp, parser.token_str);
					typeBib.addType(nt);
					typedef_type = nt;
				}
				break;
			default:
				throw std::runtime_error("Expected alias name for typedef, NOT: " + parser.token_str);
			}
			parser.parse(); // ;
			if (parser.token_str != ";")
			{
				throw std::runtime_error("Expected ';' , NOT: " + parser.token_str);
			}
		}

		void toplevel_parse_starting_type()
		{
			cpp_type* tp = identifyType(parser.token_str);
			if (tp != NULL)
			{
				// valid type, now get the name of the variable or funktion, ...
				parser.parse();

				// handle pointer
				switch (parser.token_type)
				{
				case ctt_special_char:
					{
						switch(parser.token_str[0])
						{
						case '*':
							// got ptrtype: TODO implement multilevel pointer (int ** ptr_ptr_int)
							tp = typeBib.getPtrTypeOf(tp);
							parser.parse();
							break;
						case '&':
							throw std::runtime_error("References not implemented!!");
							// get reference type TODO: implement
							break;
						}
					}
				}

				switch (parser.token_type)
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
									{
										element_tp = cbe_variable;
										std::auto_ptr<cpp_identifier> new_id(new cpp_identifier(tp, name));
										variable_id = new_id.get();
										idBib.addIdentifier(new_id);
									}
									break;
								case '=':
									// we have a simple variable defined. But there is an initialisation value to be parsed!
									// TODO
									break;
								case '(':
									// we have a funktion implementation or prototype!
									readFunctionParameter();

									// add function definition to bib
									{
										std::auto_ptr<cpp_identifier_function> new_id(new cpp_identifier_function(tp, name, idBib));
										function_id = new_id.get();
										for (size_t i = 0; i < func_params.size(); i++)
										{
											std::auto_ptr<cpp_identifier> param_id(
												new cpp_identifier(func_params[i].tp, func_params[i].name));
											function_id->addParameter(param_id);
										}
										idBib.addIdentifier((std::auto_ptr<cpp_identifier>)new_id);
									}

									// read if prototype or not:
									parser.parse();
									switch (parser.token_type)
									{
									case ctt_special_char:
										{
											switch (parser.token_str[0])
											{
											case ';':
												{
													element_tp = cbe_function_prototype;
												}
												break;
											case '{':
												element_tp = cbe_function_implementation;
												{
													// read implementation of function
													cpp_command_parser cmdparser(parser, typeBib, function_id->localBib, op_bib, tp);
													while (cmdparser.parse())
													{
														cpp_cmd* acmd = new cpp_cmd(cmdparser.cmd_type);
														acmd->exp = cmdparser.cmd_expression; // get expression
														function_id->impl_cmds.push_back(acmd);
													}

													function_id->setDefined();
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

		void parser_preprocessor_cmd()
		{
			parser.parse();
			switch (parser.token_type)
			{
			case ctt_name:
				{
					cpp_preprocessor_cmd_type cmd = identify_preprocessor_cmd(parser.token_str);
					switch (cmd)
					{
					case cpc_include:
						{
							parser.parse(); // " or <
							switch (parser.token_type)
							{
							case ctt_special_char:
								{
									switch (parser.token_str[0])
									{
									case '"':
										parser.parse_include_filename('"');
										element_tp = cbe_preproc_include_src;
										break;
									case '<':
										parser.parse_include_filename('>');
										element_tp = cbe_preproc_include_lib;
										break;
									default:
										throw std::runtime_error("parser_preprocessor_cmd: Expected \" or < Unexpeced: " + parser.token_str);
									}
									include_filename = parser.token_str;
								}
								break;
							default:
								throw std::runtime_error("parser_preprocessor_cmd: Expected \" or < Unexpeced: " + parser.token_str);
							}
						}
						break;
					default:
						throw std::runtime_error("parser_preprocessor_cmd: Not Implemented: " + parser.token_str);
					}
				}
				break;
			default:
				throw std::runtime_error("parser_preprocessor_cmd: Unexpected: " + parser.token_str);
			}
		}

	public:

		cpp_builder_elementtype element_tp;
		std::string include_filename;
		cpp_type_typedef *typedef_type;
		cpp_identifier_function *function_id;
		cpp_identifier *variable_id;

		std::string toString()
		{
			switch (element_tp)
			{
			case cbe_function_prototype:
				if (!function_id) return "<error>";
				return "function prototype: " + function_id->name;
			case cbe_function_implementation:
				{
					if (!function_id) return "<error>";

					std::ostringstream ostr;
					ostr << "{" << std::endl;
					for (size_t i = 0; i < function_id->impl_cmds.size(); i++)
					{
						ostr << function_id->impl_cmds[i]->toString() << std::endl;
					}
					ostr << "}";
					return "function implementation: " + ostr.str();
				}
			case cbe_typedef:
				return "typedef " + typedef_type->ztype->name + " " + typedef_type->name;
			case cbe_variable:
				if (!variable_id) return "<error>";
				return "declaration: " + variable_id->name;
			case cbe_preproc_include_lib:
				return "#include <" + include_filename + ">";
			case cbe_preproc_include_src:
				return "#include \"" + include_filename + "\"";
			}
			return "";
		}

		cpp_treebuilder(cpp_token_parser &a_parser, cpp_type_bib &a_typeBib,
			cpp_identifier_bib &a_idBib, cpp_operator_bib &a_op_bib)
			: parser(a_parser),
			  typeBib(a_typeBib),
			  idBib(a_idBib),
			  op_bib(a_op_bib)
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
				{
					switch (parser.token_str[0])
					{
					case '#':
						parser_preprocessor_cmd();
						break;
					default:
						throw std::runtime_error("Unexpected special char: " + parser.token_str);
					}
				}
				break;
			case ctt_name:
				// must be a type or an keyword like "class", "typedef", "volatile", ...
				{
					cpp_keyword key = identifyKeyword(parser.token_str);
					switch (key)
					{
					case ckw_typedef:
						{ // typdeklaration mit typedef
							element_tp = cbe_typedef;
							toplevel_parse_typedef();
						}
						break;
					case ckw_class:
						// class definition or prototype!
						// TODO
						throw std::runtime_error("Not Implemented: " + parser.token_str);
						break;
					case ckw_none:
						// no keyword => so we must have a type!
						{
							toplevel_parse_starting_type();
						}
						break;
					default:
						// internal error
						throw std::runtime_error("Unexpected Keyword: " + parser.token_str);
					}
				}
				break;
			default:
				throw std::runtime_error("Internal Error: Unknown token type!");
			};

			return true;
		}
	};

} // end namespace creax