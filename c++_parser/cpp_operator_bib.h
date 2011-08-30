#pragma once

/* this macro can be used to disable the default
   constructor, the copy constructor and copy-operator */
#define UNDECLARE_DEFAULT(NAME) \
	private: \
		NAME(); \
		NAME(const NAME &a); \
		NAME& operator = (const NAME &a)

namespace creax {

	class cpp_operator_bib
	{
		UNDECLARE_DEFAULT(cpp_operator_bib);
	private:

	public:

		cpp_operator_bib()
			: 
		{
			sdasd
		}

	}

}