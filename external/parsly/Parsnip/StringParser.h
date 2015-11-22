/*
    Parsnip Parser Libary
	http://parsnip-parser.sf.net
	Copyright 2007 Alex Rubinsteyn
	-----------------------------------------------------------------------
	This file is part of Parsnip.

    Parsnip is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Parsnip is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Parsnip.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef PARSNIP_STRING_PARSER_H
#define PARSNIP_STRING_PARSER_H

#include <string>
#include <memory>

#include "Parser.h"

namespace Parsnip
{

struct StringParser : public IParser<std::string, std::string>
{
	StringParser(const std::string& _str)
	: str(_str) 
	{
		this->setName(std::string("StringParser(\"") + str + "\")");
	}

protected:
	virtual Result<std::string> eval()
	{
		unsigned i = 0;
		char currChar;
		while (i < str.length() && Reader<std::string>::hasNext())
		{
			currChar = Reader<std::string>::get();
			if (currChar != str[i]) 
			{	
				Reader<std::string>::rewind();
				break;	
			}
			++i;
		}

		//if string was exhausted, we succeeded
		if (i == str.length())
			return Result<std::string>::succeed(str);
		return Result<std::string>::fail();
	}
	
private:
	std::string str;
};

std::shared_ptr<IParser<std::string, std::string>>  str(const std::string& s)
{	
	static Memoizer<std::string, std::shared_ptr<IParser<std::string, std::string>>> memoizer;
	if (memoizer.contains(s))
		return memoizer.get(s);
	return memoizer.insert(s, new StringParser(s));
}

}

#endif
