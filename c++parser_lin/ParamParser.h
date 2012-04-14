/*
Pose Estimation with MEFU, Drei Augen Projekt, HS-Augsburg 2010
Copyright (C) 2011 Ulrich Hornung

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#ifndef MEFU_PARAM_PARSER
#define MEFU_PARAM_PARSER

#include <map>
#include <string>
#include <stdexcept>
#include <fstream>
#include <algorithm>
//#include "mefu_helpers.h"

/* @todo: Maybe add support for boolean (if really required!).
		  Please Use 1/0 instead of true/false
*/

namespace mefu
{
	/**
		Class for parsing command line arguments.

		Argument format:
		key=value (e.g width=50)

		The parser can handle three different datatypes for the value:
		Integer,
		Float,
		Non-Numeric (String)

		The key will always be stored as a string
	*/

    inline bool myisspace(char c) {
        return isspace(c);
    }

    template <class _XTp>
    inline _XTp atoX_template(const std::string& string, const std::string& type)
    {
        std::istringstream istr(string);
        _XTp d;
        istr >> d;
        if (istr.fail() || !istr.eof()) {
            std::ostringstream ostr;
            ostr << "convert to <" << type << ">: invalid input string \"" << string << "\"";
            throw std::runtime_error(ostr.str());
        }
        return d;
    }


	class ParamParser
	{
	private:
		/** Container for all non-numeric parameter */
		std::map<std::string, std::string> arguments;

	public:
		/** Stores arguments in the suitable container
			@param argc		- Number of arguments
			@param argv[]	- The arguments
		*/
		ParamParser(int argc, char* argv[])
		{
			for (int i = 1; i < argc; i++)
			{
				storeValue(argv[i]);
			}
		}

		~ParamParser(void)
		{
		}

		/** Reads parameters from file and stores them to the suitable container
			@param ininame : Filename
		*/

		void parseIni(const char* ininame)
		{
			std::ifstream readfile(ininame);
			if (readfile.is_open())
			{
				std::string line;
				while (! readfile.eof() )
				{
					std::getline(readfile,line);
					/* Remove spaces */
					line.erase(remove_if(line.begin(), line.end(), myisspace), line.end());
					/* Ignore empty lines */
					if (line.length() > 0) {
						storeValue(line);
					}
				}
				readfile.close();
			}
			else
			{
				std::string error;
				error.append("Cannot open file: ");
				error.append(ininame);
				error.append("\n");
				throw std::runtime_error(error);
			}
		}

		/** Store the values
			@param param : String containing the name of the parameter and the value
		*/

		void storeValue(const std::string& param)
		{
			std::string key;
			std::string value;
			size_t seppos;

			/* get position of the seperator ('=') */
			seppos = param.find("=");

			/* check param string for errors */
			if (param.length() <= seppos || seppos == std::string::npos)
			{
				std::string error;
				error.append("Param error: ");
				error.append(param);
				error.append("\n");
				throw std::runtime_error(error);
			}

			key = param.substr(0, seppos);
			value = param.substr(seppos+1);

			arguments[key] = value;
		}

		/** Get a string argument
			@param value : Name (Key) of the argument
			@return string : Value refering to the given Key
		*/
		std::string getStringVal(const std::string& value)
		{
			std::map<std::string,std::string>::iterator it;
			it = arguments.find(value);
			if (it == arguments.end())
			{
				std::string error;
				error.append("Param not found: ");
				error.append(value);
				error.append("\n");
				throw std::runtime_error(error);
			}
			return it->second;
		}
		/** If argument does not exist, the committed defaultValue will be returned
			@param value :			Name (Key) of the argument
			@param defaultValue :	Return value if key does not exist
			@return string :		Value refering to the given Key or defaultValue
		*/
		std::string getStringVal(const std::string& value,
								 const std::string& defaultValue)
		{
			try
			{
				return getStringVal(value);
			}
			catch (std::runtime_error)
			{
				return defaultValue;
			}
		}

		/** Get a string argument
			@param value : Name (Key) of the argument
			@return float : Value refering to the given Key
		*/
		float getFloatVal(const std::string& value)
		{
            return atoX_template<float>(getStringVal(value), "float");
		}

		/** If argument does not exist, the committed defaultValue will be returned
			@param value :			Name (Key) of the argument
			@param defaultValue :	Return value if key does not exist
			@return float :			Value refering to the given Key or defaultValue
		*/

		float getFloatVal(const std::string& value, const float defaultValue)
		{
			try
			{
				return getFloatVal(value);
			}
			catch (std::runtime_error)
			{
				return defaultValue;
			}
		}

		/** Get a string argument
			@param value : Name (Key) of the argument
			@return int : Value refering to the given Key
		*/
		int getIntVal(const std::string& value)
		{
            return atoX_template<int>(getStringVal(value), "int");
		}

		/** If argument does not exist, the committed defaultValue will be returned
			@param value :			Name (Key) of the argument
			@param defaultValue :	Return value if key does not exist
			@return int :			Value refering to the given Key or defaultValue
		*/
		int getIntVal(const std::string& value, const int defaultValue)
		{
			try
			{
				return getIntVal(value);
			}
			catch (std::runtime_error)
			{
				return defaultValue;
            }
		}
	};

    /* helper functions for parameter processing */

    inline void readIntegerListFromString(const std::string input, std::vector<size_t> &outList, const char seperator = ',')
    {
        std::string nr;

        for (size_t i = 0; i < input.size(); i++)
        {
            if (input[i] == seperator)
            {
                outList.push_back(atoi(nr.c_str()));
                nr = "";
            }
            else
            {
                nr += input[i];
            }
        }

        if (nr.size() > 0)
        {
            outList.push_back(atoi(nr.c_str()));
        }
    }

    inline void readStringListFromString(const std::string input, std::vector<std::string> &outList, const char seperator = ',')
    {
        std::string part;

        for (size_t i = 0; i < input.size(); i++)
        {
            if (input[i] == seperator)
            {
                outList.push_back(part);
                part = "";
            }
            else
            {
                part += input[i];
            }
        }

        if (part.size() > 0)
        {
            outList.push_back(part.c_str());
        }
    }
}

#endif
