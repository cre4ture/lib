#ifndef CPP_PARSER_H_INCLUDED
#define CPP_PARSER_H_INCLUDED

#include <string>
#include <sstream>
#include <vector>
#include "auto_ptr_vector.h"
#include "dllinterface.h"

namespace creax {

	enum tag_type {
		tt_None,
		tt_StartTag,
		tt_EndTag,
		tt_EmptyTag,
		tt_Content,
		tt_Comment,
		tt_CDATA
	};

	struct htmlattribute {
		std::string name;
		std::string value;
	};

	class htmlparser {
	private:

		const char* const htmlcode;
		const char* cpos;

		bool findTagBegin() 
		{
			while (*cpos != 0) {
				if (*cpos == '<') return true;
				cpos++;
			}
			return false;
		}

		htmlattribute* makeAttribute(const std::ostringstream& name, const std::ostringstream& value)
		{
			htmlattribute* attr = new htmlattribute;
			attr->name = name.str();
			attr->value = value.str();
			return attr;
		}

		htmlattribute* readAttribute()
		{
			// skip spaces
			while (*cpos != 0) {
				switch (*cpos) {
				case 9: // tab
				case 13: // return
				case ' ':
					break; // just ignore
				case '/':
					if (*(cpos+1) != '>') {
						return readAttribute_name(); // if its not the end of tag	
					}
					curTagType = tt_EmptyTag;
					cpos += 2;
					return NULL; // end reached
				case '>':
					cpos ++;
					return NULL; // end of tag reached
				default:
					return readAttribute_name();
				}
				cpos++;
			}
			return NULL;
		}

		htmlattribute* readAttribute_name()
		{
			std::ostringstream name;
			// read name
			while (*cpos != 0) {
				switch (*cpos) {
				case '/': // a empty-tags end
					if (*(cpos+1) == '>') {
						cpos++;
						curTagType = tt_EmptyTag;
						return makeAttribute(name, name);
					} else {
						name << '/';
						break;
					}
				case '>': // tag ende
				case 9:  // tab
				case 13: // return
				case ' ':
					// attribute without value
					return makeAttribute(name, name);
				case '=':
					cpos++;
					return readAttribute_value(name);
				default:
					name << (*cpos);
				}
				cpos++;
			}
			return NULL;
		}

		htmlattribute* readAttribute_value(const std::ostringstream& name)
		{
			// is a " or ' used to mark the value?
			switch (*cpos) {
			case '\'':  // ' is ok
			case '"':   // " is ok
				cpos++;
				return readAttribute_value_withQ(name, *(cpos-1));
			default:
				return readAttribute_value_withoutQ(name);
			}
			return NULL;
		}

		htmlattribute* readAttribute_value_withQ(const std::ostringstream& name, const char quote)
		{
			std::ostringstream value;
			// read value till quote or #0 or #13
			while ((*cpos != quote)) {
				switch (*cpos) {
				case '\\':
					if (*(cpos+1) == quote) *cpos++; // escape only if quote
					break;
				case 13: // ignore returns in tag value
					break; 
				case 0:  // end of string
					return makeAttribute(name, value); 
				default:
					value << (*cpos);
				}
				cpos++;
			}
			cpos++;
			return makeAttribute(name, value);
		}

		htmlattribute* readAttribute_value_withoutQ(const std::ostringstream& name)
		{
			std::ostringstream value;
			// read value till tab return or space or /
			while ((*cpos != 0)) {
				switch (*cpos) {
				case '/': // an empty-tag's end
					if (*(cpos+1) != '>')
					{
						value << '/';
					} else {
						cpos++;
						curTagType = tt_EmptyTag;
						return makeAttribute(name, value); 
					}
					break;
				case '>':
					return makeAttribute(name, value); 
				case 9: // tab
				case 13: // return
				case ' ':
					cpos++;
					return makeAttribute(name, value); 
				default:
					value << (*cpos);
				}
				cpos++;
			}
			return NULL;
		}

		bool readAttributes() {
			htmlattribute* attr = NULL;
			curTagAttributes.clear();
			while ((attr = readAttribute())) {
				curTagAttributes.push_back(attr);
			}
			return true;
		}

		bool readStartTag_starting_at_its_name()
		{
			std::ostringstream name;
			curTagType = tt_StartTag;
			while (*cpos != 0) {
				switch (*cpos) {
				case 9: // tab
				case 13: // return
				case ' ':
					curTagName = name.str();
					return readAttributes();
				case '>':
					cpos++;
					curTagName = name.str();
					curTagAttributes.clear(); // no attributes!
					return true;
				case '/':
					if (*(cpos+1) == '>') {
						curTagType = tt_EmptyTag;
					} else {
						name << '/';
					}
					break;
				default:
					name << (*cpos);
					break;
				}
				cpos++;
			}
			return false;
		}

		bool readEndTag_starting_at_its_name()
		{
			std::ostringstream name;
			curTagType = tt_EndTag;
			while (*cpos != 0) {
				switch (*cpos) {
				case 13: // return
				case '>': // or end tag
					cpos++;
					curTagName = name.str();
					return true;
				default:
					name << (*cpos);
					break;
				}
				cpos++;
			}
			return false;
		}

		bool readContent() {

			bool run = true;
			std::ostringstream content;
			while (run) {
				switch (*cpos) {
				default:
					content << (*cpos);
					cpos++;
					break;
				case 0:   // end reached
				case '<': // tag begins
					run = false;
					break;
				}
			}

			curTagType = tt_Content;
			curContent = content.str();
			return true;  // content allways extists!
		}

		bool readCDATA() 
		{
			std::stringstream cdata;
			const char* end = strstr(cpos, "]]>");
			while ((*cpos != 0)&&(cpos != end)) {
				cdata << (*cpos);
				cpos++;
			}
			if (*cpos != 0) cpos += 3;
			curContent = cdata.str();
			curTagType = tt_CDATA;
			return true;
		}

		bool readComment()
		{
			std::stringstream cdata;
			const char* end = strstr(cpos, "-->");
			while ((*cpos != 0)&&(cpos != end)) {
				cdata << (*cpos);
				cpos++;
			}
			if (*cpos != 0) cpos += 3;
			curContent = cdata.str();
			curTagType = tt_Comment;
			return true;
		}

	public:

		tag_type curTagType;
		std::string curTagName;
		std::string curContent;
		mefu::auto_ptr_vector<htmlattribute> curTagAttributes;

		htmlparser(const char* const ahtmlcode)
			: htmlcode(ahtmlcode), 
			cpos(ahtmlcode)
		{}

		bool parse()
		{
			switch (*cpos) {
			case '<':
				cpos++;
				switch (*cpos) {
				case '/': // end tag
					cpos++;
					return readEndTag_starting_at_its_name();
				case '!': // comment or cdata tag
					if (strncmp(cpos, "!--", 3) == 0) {
						cpos += 3;
						return readComment();
					} else
					if (strncmp(cpos, "![CDATA[", 8) == 0) {
						cpos += 8;
						return readCDATA();
					} else {
						return readContent();
					}
					break; 
				default: // start tag
					return readStartTag_starting_at_its_name();
				}
			case 0:
				return false; // end reached!
			default:
				return readContent();
			}
		}

	};

}

#endif