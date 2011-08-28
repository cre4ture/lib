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
	case '/'

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

	class cpp_type
	{
		UNDECLARE_DEFAULT(cpp_type);
	public:
		std::string name;
		cpp_type *ptr_type;

		cpp_type(std::string a_name)
			: ptr_type(NULL), name(a_name)
		{}
	};

	class cpp_type_ptr: public cpp_type
	{
		UNDECLARE_DEFAULT(cpp_type_ptr);
	public:
		const cpp_type * const ztype;

		cpp_type_ptr(const cpp_type *aZtype)
			: cpp_type(aZtype->name + '*'),
			  ztype(aZtype)
		{}
	};

	class cpp_type_typedef: public cpp_type
	{
		UNDECLARE_DEFAULT(cpp_type_typedef);
	public:
		const cpp_type * const ztype;

		cpp_type_typedef(const cpp_type *aZtype, const std::string &a_name)
			: cpp_type(a_name),
			  ztype(aZtype)
		{
			name = a_name;
		}
	};

	const char* builtin_typeList[] = {
		"void",
		"char",
		"bool",
		"int",
		"float",
		"double",
		NULL
	};

	class cpp_type_bib
	{
		UNDECLARE_DEFAULT(cpp_type_bib);
	private:
		const cpp_type_bib * const parent;
		mefu::auto_ptr_map<std::string, cpp_type> types;

	public:

		cpp_type_bib(cpp_type_bib *a_parent)
			:parent(a_parent)
		{
			if (a_parent == NULL)
			{
				for (int i = 0; builtin_typeList[i] != NULL; i++)
				{
					cpp_type *tpy = new cpp_type(builtin_typeList[i]);
					addType(tpy);
				}
			}
		}

		~cpp_type_bib()
		{
			// types werden automatisch wieder freigegeben
		}

		void addType(cpp_type *atype)
		{
			if (types.find(atype->name) != types.end())
			{
				throw std::runtime_error("TypeBib: Type already exists: " + atype->name);
			}
			types[atype->name] = atype;
		}

		cpp_type* getPtrTypeOf(cpp_type *atype)
		{
			if (atype->ptr_type == NULL)
			{
				atype->ptr_type = new cpp_type_ptr(atype);
				addType(atype->ptr_type);
			}

			return atype->ptr_type;
		}

		cpp_type* identifyType(std::string name)
		{
			mefu::auto_ptr_map<std::string, cpp_type>::iterator i = types.find(name);
			if (i != types.end())
			{
				return i->second;
			}
			return NULL;
		}
	};

	enum cpp_identifier_type
	{
		cit_none,
		cit_variable,
		cit_function
	};

	class cpp_identifier
	{
		UNDECLARE_DEFAULT(cpp_identifier);
	public:
		cpp_type *tp;
		std::string name;
		
		virtual cpp_identifier_type getType() { return cit_variable; }

		cpp_identifier(cpp_type *a_tp, std::string a_name)
			: tp(a_tp),
			  name(a_name)
		{}

		virtual ~cpp_identifier() {}
	};

	class cpp_identifier_bib
	{
		UNDECLARE_DEFAULT(cpp_identifier_bib);
	private:
		cpp_identifier_bib *parent;
		mefu::auto_ptr_map<std::string, cpp_identifier> ids;

	public:

		cpp_identifier_bib(cpp_identifier_bib *a_parent)
			: parent(a_parent)
		{}

		cpp_identifier* findIdentifier(const std::string &name, bool only_local = false)
		{
			mefu::auto_ptr_map<std::string, cpp_identifier>::iterator i = ids.find(name);
			if (i != ids.end())
				return i->second;

			if (only_local == false && parent != NULL)
				return parent->findIdentifier(name);

			return NULL;
		}

		void addIdentifier(cpp_identifier *id)
		{
			// TODO: überprüfen ob schon vorhanden !?
			ids[id->name] = id;
		}

		void addConstIdentifier(cpp_identifier *id)
		{
			// add const ids to top-level-bib!
			if (parent == NULL)
				addIdentifier(id);
			else
				parent->addConstIdentifier(id);
		}
	};

	class cpp_cmd; // prototyp
	class cpp_identifier_bib; // prototyp

	class cpp_identifier_function: public cpp_identifier
	{
	private:
		bool defined;

	public:
		cpp_identifier_bib localBib;
		mefu::auto_ptr_vector<cpp_cmd> impl_cmds;

		bool isDefined() { return defined; }
		
		virtual cpp_identifier_type getType() { return cit_function; }

		void setDefined() { defined = true; }

		cpp_identifier_function(cpp_type *a_tp, std::string a_name, cpp_identifier_bib &parentIdBib)
			: cpp_identifier(a_tp, a_name),
			  defined(false),
			  localBib(&parentIdBib)
		{}
	};

	enum cpp_builder_elementtype
	{
		cbe_none,
		cbe_function_implementation,
		cbe_function_prototype,
		cbe_typedef
	};

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

	class cpp_expression_function_call: public cpp_expression
	{
		UNDECLARE_DEFAULT(cpp_expression_function_call);
	public:
		cpp_identifier * const id;
		mefu::auto_ptr_vector<cpp_expression> params;

		virtual cpp_expression_type getType() { return cet_function_call; }

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
				cmd_expression.reset(parse_one_expression());

				if (cmd_expression->tp != returnType)
				{
					throw std::runtime_error("Return command: wrong type: '" + cmd_expression->tp->name + "' expected '" + returnType->name + "'");
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

		cpp_expression_function_call* parse_cmd_starting_function_call(cpp_identifier_function *fid)
		{
			parser.parse(); // "("
			if (parser.token_str != "(")
			{
				throw std::runtime_error("function call: Expected '(' instead of: " + parser.token_str);
			}
			else
			{
				cpp_expression_function_call *fcall = new cpp_expression_function_call(fid->tp, fid);
				try
				{
					parser.parse(); // parse next expression
					bool read_params = (parser.token_str != ")"); // skip if ")"

					if (read_params == false)
					{
						parser.parse();
						return fcall;
					}

					while (read_params)
					{
						fcall->params.push_back(
							parse_one_expression()
							);
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

				} catch (...)
				{
					// im Fehlerfall den Speicher wieder aufräumen!
					delete fcall;
					throw;
				}
			}
		}

		cpp_expression* parse_expression_starting_with_variable(cpp_identifier *id)
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
							cpp_expression_variable* exp = new cpp_expression_variable(id->tp, id);
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

		cpp_expression* parse_one_expression()
		{
			cpp_identifier *id = local_ids.findIdentifier(parser.token_str);
			
			if (id == NULL)
			{
				// ists ne noch unbekannte konstante?
				if (parser.token_type == ctt_const)
				{
					cpp_type *tp = typeBib.identifyType("int"); // TODO: other consts: text float chars ...
					if (tp == NULL) throw std::runtime_error("A Command parser: Internal Error: int not found!" + parser.token_str); 
					id = new cpp_identifier(tp, parser.token_str);
					local_ids.addConstIdentifier(id);
				}
				else
				{
					// was könnte des den sonst noch sein?
					throw std::runtime_error("B Command parser: Unknown identifier: " + parser.token_str);
				}
			}
			
			switch (id->getType())
			{
			case cit_variable:
				// assignment or ....
				return parse_expression_starting_with_variable(id);
				break;
			case cit_function:
				// function call
				return parse_cmd_starting_function_call((cpp_identifier_function*)id);
				break;
			default:
				throw std::runtime_error("C Command parser: Unknown identifier: " + parser.token_str);
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
							cpp_identifier *var = new cpp_identifier(tp, name);
							local_ids.addIdentifier(var);
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
				cpp_expression *exp = parse_one_expression();
				if (exp == NULL)
				{
					// eigentlich unnötig hier, da im falle von einem Fehler eine Exception geworfen wird 
					// und nie NULL zurückgegeben werden sollte.
					throw std::runtime_error("Command Parser: Expression NULL (internal error)");
				}
				cmd_expression.reset(exp);
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

	class cpp_treebuilder 
	{
		UNDECLARE_DEFAULT(cpp_treebuilder);
	private:
		cpp_token_parser &parser;
		cpp_type_bib &typeBib;
		cpp_identifier_bib &idBib;

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
			parser.parse(); // original typ
			switch (parser.token_type)
			{
			case ctt_name:
				{
					cpp_type* tp = typeBib.identifyType(parser.token_str);
					if (tp == NULL)
					{
						throw std::runtime_error("Unkown type in typedef: " + parser.token_str);
					}
					// TODO: save the type of the new type!!
				}
				break;
			default:
				throw std::runtime_error("Typedef: expected type, NOT: " + parser.token_str);
			}
			parser.parse(); // alias name
			switch (parser.token_type)
			{
			case ctt_name:
				{
					cpp_type *nt = new cpp_type(parser.token_str);
					opname = parser.token_str; // for debug output
					typeBib.addType(nt);
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
												{
													element_tp = cbe_function_prototype;
													cpp_identifier_function *id = new cpp_identifier_function(tp, name, idBib);
													idBib.addIdentifier(id);
													element_fcall = id;
												}
												break;
											case '{':
												element_tp = cbe_function_implementation;
												{
													cpp_identifier_function *id = new cpp_identifier_function(tp, name, idBib);
													idBib.addIdentifier(id);

													cpp_command_parser cmdparser(parser, typeBib, idBib, tp);
													while (cmdparser.parse())
													{
														cpp_cmd* acmd = new cpp_cmd(cmdparser.cmd_type);
														acmd->exp = cmdparser.cmd_expression; // get expression
														id->impl_cmds.push_back(acmd);
													}

													id->setDefined();
													element_fcall = id;
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

	public:

		cpp_builder_elementtype element_tp;
		std::string opname;
		cpp_identifier_function *element_fcall;

		std::string toString()
		{
			switch (element_tp)
			{
			case cbe_function_prototype:
				return "function prototype";
			case cbe_function_implementation:
				{
					if (!element_fcall) return "<error>";

					std::ostringstream ostr;
					ostr << "{" << std::endl;
					for (size_t i = 0; i < element_fcall->impl_cmds.size(); i++)
					{
						ostr << element_fcall->impl_cmds[i]->toString() << std::endl;
					}
					ostr << "}";
					return "function implementation: " + ostr.str();
				}
			case cbe_typedef:
				return "typedef [] " + opname;
			}
			return "";
		}

		cpp_treebuilder(cpp_token_parser &a_parser, cpp_type_bib &a_typeBib, cpp_identifier_bib &a_idBib)
			: parser(a_parser),
			  typeBib(a_typeBib),
			  idBib(a_idBib)
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
				throw std::runtime_error("Unexpected special char: " + parser.token_str);
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

}