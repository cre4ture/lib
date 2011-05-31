
#include "parser.h"

extern "C" {

__declspec(dllexport)
void* creax_createParser(const char* const htmlcode)
{
	creax::htmlparser* parser = new creax::htmlparser(htmlcode);
	return parser;
}

__declspec(dllexport)
void creax_freeParser(void* pparser)
{
	creax::htmlparser* parser = (creax::htmlparser*)pparser;
	delete parser;
}

__declspec(dllexport)
bool creax_parse(void* pparser, 
	creax::tag_type* tagType,
	const char** tagName, 
	const char** tagContent,
	size_t* tagAttributeCount,
	const char*** attrNames,
	const char*** attrValues)
{
	creax::htmlparser* parser = (creax::htmlparser*)pparser;
	bool result = parser->parse();
	if (result) {
		*tagType = parser->curTagType;
		*tagName = parser->curTagName.c_str();
		*tagContent = parser->curContent.c_str();
		*tagAttributeCount = parser->curTagAttributes.size();
		if (parser->initpAttrs()) {
			*attrNames = &parser->pAttrNames[0];
			*attrValues = &parser->pAttrValues[0];
		}
	}
	return result;
}

__declspec(dllexport)
bool creax_getAttribute(void* pparser,
	const size_t index, const char** name, const char** value) 
{
	creax::htmlparser* parser = (creax::htmlparser*)pparser;
	bool result = (parser->curTagAttributes.size() > index);
	if (result) {
		*name = parser->curTagAttributes[index]->name.c_str();
		*value = parser->curTagAttributes[index]->value.c_str();
	}
	return result;
}

}