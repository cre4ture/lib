#pragma once

#include "cpp_types.h"
#include "cpp_identifier.h"

namespace creax {

	enum cpp_command_type
	{
		cct_none,
		cct_return,
		cct_decl_without_init,
		cct_expression
	};

	const char* cmd_type_list[cct_expression+1] = {
		"none",
		"return",
		"decl_without_init",
		"expression"
	}; 

	enum cpp_expression_type
	{
		cet_none,
		cet_variable,
		cet_operator_call,
		cet_function_call
	};

	class cpp_expression
	{
		UNDECLARE_DEFAULT(cpp_expression);
	public:
		cpp_type *tp;

		virtual cpp_expression_type getType() = NULL;

		virtual std::string toString() = NULL;

		cpp_expression(cpp_type *a_tp)
			: tp(a_tp)
		{}

		virtual ~cpp_expression() {}
	};

	class cpp_expression_variable: public cpp_expression
	{
		UNDECLARE_DEFAULT(cpp_expression_variable);
	public:
		cpp_identifier * const id;

		virtual cpp_expression_type getType() { return cet_variable; }

		virtual std::string toString() { return id->name; }

		cpp_expression_variable(cpp_type *a_tp, cpp_identifier *a_id)
			: cpp_expression(a_tp), 
			  id(a_id)
		{}
	};

	class cpp_expression_operator_call: public cpp_expression
	{
		UNDECLARE_DEFAULT(cpp_expression_operator_call);
	public:
		std::string op_name;
		mefu::auto_ptr_vector<cpp_expression> params;

		virtual cpp_expression_type getType() { return cet_operator_call; }

		virtual std::string toString()
		{ 
			switch (params.size())
			{
			case 1:
				return op_name + params[0]->toString();
			case 2:
				return params[0]->toString() + op_name + params[1]->toString();
			default:
				return "invalid op: " + op_name;
			}
		}

		void addParam(std::auto_ptr<cpp_expression> exp)
		{
			params.push_back(exp.get());
			exp.release(); // auto_ptr_vector(params) is now responsible for pointer!
		}

		cpp_expression_operator_call(cpp_type *a_tp, std::string a_op_name)
			: cpp_expression(a_tp),
			  op_name(a_op_name)
		{}
	};

	class cpp_expression_function_call: public cpp_expression
	{
		UNDECLARE_DEFAULT(cpp_expression_function_call);
	public:
		cpp_identifier * const id;
		mefu::auto_ptr_vector<cpp_expression> params;

		virtual cpp_expression_type getType() { return cet_function_call; }

		void addParameter(std::auto_ptr<cpp_expression> param)
		{
			params.push_back(param.get());
			param.release(); // auto_ptr_vector is now responsible for deletion of pointer!
		}

		virtual std::string toString()
		{
			std::ostringstream ostr;
			ostr << id->name << "(";
			for (size_t i = 0; i < params.size(); i++)
			{
				ostr << "," << params[i]->toString();
			}
			ostr << ")";
			return ostr.str();
		}

		cpp_expression_function_call(cpp_type *a_tp, cpp_identifier *a_id)
			: cpp_expression(a_tp), 
			  id(a_id)
		{}
	};

	class cpp_command_parser
	{
		UNDECLARE_DEFAULT(cpp_command_parser);
	private:
		cpp_token_parser &parser;
		cpp_identifier_bib &local_ids;
		cpp_type *returnType;
		cpp_type_bib &typeBib;

		void parse_cmd_starting_keyword(cpp_keyword kw)
		{
			switch (kw)
			{
			case ckw_return:
				cmd_type = cct_return;
				parser.parse();

				if (parser.token_str != ";")
				{
					cmd_expression = parse_one_expression();

					if (cmd_expression->tp != returnType)
					{
						throw std::runtime_error("Return command: wrong type: '" + cmd_expression->tp->name + "' expected '" + returnType->name + "'");
					}
				}
				else
				{
					cmd_expression.release();
				}

				// check ;
				if (parser.token_str != ";")
				{
					throw std::runtime_error("Return with value: Expected ';'");
				}
				break;
			default:
				throw std::runtime_error("Unexpected: " + parser.token_str);
			}
		}

		std::auto_ptr<cpp_expression_function_call> parse_cmd_starting_function_call(cpp_identifier_function *fid)
		{
			parser.parse(); // "("
			if (parser.token_str != "(")
			{
				throw std::runtime_error("function call: Expected '(' instead of: " + parser.token_str);
			}
			else
			{
				std::auto_ptr<cpp_expression_function_call> fcall(new cpp_expression_function_call(fid->tp, fid));

				parser.parse(); // parse next expression
				bool read_params = (parser.token_str != ")"); // skip if ")"

				if (read_params == false)
				{
					parser.parse();
					return fcall;
				}

				while (read_params)
				{
					fcall->addParameter(parse_one_expression());
					switch (parser.token_type)
					{
					case ctt_special_char:
						{
							switch (parser.token_str[0])
							{
							case ',':
								// read next param in next loop
								break;
							case ')':
								// last param read, -> exit loop
								read_params = false;
								break;
							default:
								throw std::runtime_error("Function call: Unexpected " + parser.token_str);
							}
							// next
							parser.parse();
						}
						break;
					default:
						throw std::runtime_error("Function call: Unexpected " + parser.token_str);
					}
				}
				return fcall;
			}
		}

		std::auto_ptr<cpp_expression> parse_expression_starting_with_variable(cpp_identifier *id)
		{
			parser.parse();
			switch (parser.token_type)
			{
			case ctt_special_char:
				{
					switch (parser.token_str[0])
					{
					case ',':
					case ')':
					case';':
						{ // expression end -> variable expression!
							std::auto_ptr<cpp_expression_variable> exp(new cpp_expression_variable(id->tp, id));
							return exp;
						}
						break;

					case '*':
					case '/':
					case '>':
					case '<':
					case '%':
						// ....
						// TODO: implement operators!
						throw std::runtime_error("Operators not implemented!! ");
						break;
					default:
						throw std::runtime_error("Expression parser: Unexpected " + parser.token_str);
					}
				}
				break;
			default:
				throw std::runtime_error("Expression parser: Unexpected " + parser.token_str);
			}
		}

		std::auto_ptr<cpp_expression> parse_one_expression()
		{
			switch (parser.token_type)
			{
			case ctt_special_char:
				{
					switch (parser.token_str[0])
					{
					case '*':
						{
							std::string op_name = parser.token_str;
							parser.parse();
							std::auto_ptr<cpp_expression> param = parse_one_expression();
							
							if (param->tp->getType() != ctty_ptr_type)
								throw std::runtime_error("E Command parser: dereferencation of none pointer type: " + param->toString());

							std::auto_ptr<cpp_expression_operator_call> exp(new cpp_expression_operator_call(((cpp_type_ptr*)param->tp)->ztype, op_name));
							exp->addParam(param);
							return exp;
						}
					case '&':
						{
							std::string op_name = parser.token_str;
							parser.parse();
							std::auto_ptr<cpp_expression> param = parse_one_expression();
							std::auto_ptr<cpp_expression_operator_call> exp(new cpp_expression_operator_call(param->tp->ptr_type, op_name));
							exp->addParam(param);
							return exp;
						}
						break;
					case '(':
						{
							// klammer-expression
							parser.parse();
							std::auto_ptr<cpp_expression> exp = parse_one_expression();
							if (parser.token_str != ")")
							{
								throw std::runtime_error("Z Command parser: missing ')': found: " + parser.token_str);
							}
							parser.parse();
							return exp;
						}
						break;
					default:
						throw std::runtime_error("F Command parser: Unexpected operator: " + parser.token_str);
					}
				}
				break;
			case ctt_const:
				{
					cpp_identifier *id = local_ids.findIdentifier(parser.token_str);
					if (id == NULL)
					{
						cpp_type *tp = typeBib.identifyType("int"); // TODO: other consts: text float chars ...
						if (tp == NULL) throw std::runtime_error("A Command parser: Internal Error: int not found!" + parser.token_str); 
						std::auto_ptr<cpp_identifier> new_id(new cpp_identifier(tp, parser.token_str));
						id = new_id.get();
						local_ids.addConstIdentifier(new_id);
					}
					return parse_expression_starting_with_variable(id);
				}
				break;
			case ctt_name:
				{
					cpp_identifier *id = local_ids.findIdentifier(parser.token_str);
					if (id == NULL)
					{
						throw std::runtime_error("C Command parser: Unknown identifier: " + parser.token_str);
					}
					switch (id->getType())
					{
					case cit_variable:
						// assignment or ....
						return parse_expression_starting_with_variable(id);
						break;
					case cit_function:
						// function call
						return (std::auto_ptr<cpp_expression>)parse_cmd_starting_function_call((cpp_identifier_function*)id);
						break;
					default:
						throw std::runtime_error("C Command parser: Unknown identifier type: " + parser.token_str);
					}
				}
				break;
			default:
				throw std::runtime_error("D Command parser: Unexpected: " + parser.token_str);
			}
		}

		void parse_cmd_starting_name()
		{
			// could be a declaration or assingment or function call...

			// test for declaration
			cpp_type *tp = typeBib.identifyType(parser.token_str);
			if (tp != NULL)
			{
				// declaration of local varable
				parser.parse(); // name or * or &
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
					break;
				}

				// now read name
				switch (parser.token_type)
				{
				case ctt_name:
					{
						std::string name = parser.token_str; 
						parser.parse();
						switch (parser.token_type)
						{
						case ctt_special_char:
							{
								switch (parser.token_str[0])
								{
								case '[':
									// array var...
									parser.parse(); // ]
									if (parser.token_str != "]")
									{
										throw std::runtime_error("Declaration of local variable: Expected ']' instead of '" + parser.token_str + "'");
									}
									tp = typeBib.getPtrTypeOf(tp);
									parser.parse();
									break;
								}
							}
							break;
						default:
							throw std::runtime_error("Declaration of local variable: Unexpected: '" + parser.token_str + "'");
						}

						if (parser.token_str != ";") 
						{
							throw std::runtime_error("Declaration of local variable: Expected ';' instead of '" + parser.token_str + "'");
						}

						cpp_identifier *i = local_ids.findIdentifier(name, true);
						if (i != NULL)
						{
							throw std::runtime_error("Declaration of local variable: '" + name + "' already exists");
						}
						else
						{
							local_ids.addIdentifier(std::auto_ptr<cpp_identifier>(new cpp_identifier(tp, name)));
							cmd_type = cct_decl_without_init;
						}
					}
					break;
				default:
					throw std::runtime_error("Declaration of local var: Unexpeced: " + parser.token_str);
				}
			}
			else
			{
				// kein typ -> function call, assignment...
				// TODO
				cmd_expression = parse_one_expression();
				if (cmd_expression.get() == NULL)
				{
					// eigentlich unnötig hier, da im falle von einem Fehler eine Exception geworfen wird 
					// und nie NULL zurückgegeben werden sollte.
					throw std::runtime_error("Command Parser: Expression NULL (internal error)");
				}
				cmd_type = cct_expression;
			}
		}

	public:
		cpp_command_parser(cpp_token_parser &a_parser,
							cpp_type_bib &bib, 
							cpp_identifier_bib &local_idBib,
							cpp_type *a_returnType)
			: parser(a_parser),
			  cmd_type(cct_none),
			  typeBib(bib),
			  local_ids(local_idBib),
			  returnType(a_returnType)
		{}

		~cpp_command_parser()
		{}

		cpp_command_type cmd_type;
		std::auto_ptr<cpp_expression> cmd_expression;

		bool parse()
		{
			switch (parser.parse())
			{
			case ctt_name:
				// zuweisung, funktionsaufruf, variablendeklaration, schleifenstart, ...
				{
					cpp_keyword kw = identifyKeyword(parser.token_str);
					if (kw == ckw_none)
					{
						parse_cmd_starting_name();
					}
					else
					{
						parse_cmd_starting_keyword(kw);
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

	enum cpp_preprocessor_cmd_type
	{
		cpc_include,
		cpc_ifdef,
		cpc_none
	};

	char* cpp_preprocessor_cmd_names[] =
	{
		"include",
		"ifdef",
		NULL
	};

	inline cpp_preprocessor_cmd_type identify_preprocessor_cmd(std::string name)
	{
		for (int i = 0; cpp_preprocessor_cmd_names[i] != NULL; i++)
		{
			if (name == cpp_preprocessor_cmd_names[i])
			{
				return (cpp_preprocessor_cmd_type)i;
			}
		}
		return cpc_none;
	}

	class cpp_cmd
	{
		UNDECLARE_DEFAULT(cpp_cmd);
	private:
		std::string typetostr()
		{
			return cmd_type_list[tp];
		}

	public:
		cpp_command_type tp;  // use type? or use different classes?
		std::auto_ptr<cpp_expression> exp;

		cpp_cmd(cpp_command_type a_tp)
			: tp(a_tp)
		{}

		std::string toString()
		{
			std::ostringstream ostr;
			ostr << "cmdtype:" << typetostr();
			if (exp.get() != NULL)
			{
				ostr << ": " << exp->toString();
			}
			return ostr.str();
		}
	};

} // end namespace creax