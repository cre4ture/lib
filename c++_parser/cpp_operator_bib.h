#pragma once

#include "c++_parser.h"
#include "auto_ptr_map.h"
#include <string>

namespace creax {

	class cpp_operator
	{
		UNDECLARE_DEFAULT(cpp_operator);
	public:
		const int op_count;
		const std::string name;

		cpp_operator(const std::string a_name, const int a_op_count)
			: op_count(a_op_count),
			  name(a_name)
		{}
	};

	class cpp_operator_bib
	{
		UNDECLARE_DEFAULT(cpp_operator_bib);
	private:
		mefu::auto_ptr_map<std::string, cpp_operator> op_list;
		
	public:

		cpp_operator_bib(const int dummy)
		{
			cpp_operator *op;

			op = new cpp_operator("+", 2);
			op_list[op->name] = op;
			op = new cpp_operator("-", 2);
			op_list[op->name] = op;
			op = new cpp_operator("*", 2);
			op_list[op->name] = op;
			op = new cpp_operator("/", 2);
			op_list[op->name] = op;
			op = new cpp_operator("%", 2);
			op_list[op->name] = op;
			op = new cpp_operator("=", 2);
			op_list[op->name] = op;
		}

		cpp_operator* getOperator(const std::string op)
		{
			if (op_list.find(op) != op_list.end())
				return op_list[op];
			else
				return NULL;
		}

	};

} // end of namespace creax