#pragma once

#include "c++_parser.h"

namespace creax {

	enum cpp_type_type
	{
		ctty_basic_type,
		ctty_ptr_type
	};

	class cpp_type
	{
		UNDECLARE_DEFAULT(cpp_type);
	public:
		std::string name;
		cpp_type *ptr_type;

		virtual cpp_type_type getType() { return ctty_basic_type; }

		cpp_type(std::string a_name)
			: ptr_type(NULL),
			  name(a_name)
		{}
	};

	class cpp_type_ptr: public cpp_type
	{
		UNDECLARE_DEFAULT(cpp_type_ptr);
	public:
		cpp_type * const ztype;

		virtual cpp_type_type getType() { return ctty_ptr_type; }

		cpp_type_ptr(cpp_type *aZtype)
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

} // end namespace creax