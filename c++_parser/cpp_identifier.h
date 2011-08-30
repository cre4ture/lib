#pragma once

#include "cpp_types.h"

namespace creax {

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

		void addIdentifier(std::auto_ptr<cpp_identifier> id)
		{
			// TODO: überprüfen ob schon vorhanden !?
			ids[id->name] = id.get();
			id.release(); // auto_ptr_map now is responsible!
		}

		void addConstIdentifier(std::auto_ptr<cpp_identifier> id)
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
		cbe_typedef,
		cbe_variable,
		cbe_preproc_include_lib,
		cbe_preproc_include_src
	};

} // end namespace creax
