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

#ifndef PARSNIP_CHAR_PARSER_H
#define PARSNIP_CHAR_PARSER_H

#include <string>
#include <memory>

#include "Parser.h"
#include "Memoizer.h"

namespace Parsnip
{

struct CharParser :public IParser<std::string, std::string>
{
	CharParser(char c)
	: myChar(c) 
	{
		this->setName(std::string("CharParser('") + c + "')");
	}

protected:
	virtual Result<std::string> eval()
	{
		if (Reader<std::string>::hasNext())
		{
			Reader<std::string>::IndexT lastPos = Reader<std::string>::pos();
			char currChar = Reader<std::string>::get();
			
			if (currChar == myChar)
				return Result<std::string>::succeed(to_string(myChar));

			// token-level parsers mustn't advance the cursor past where they succeed
			Reader<std::string>::set_pos(lastPos);
		}
		return Result<std::string>::fail();
	}
	
private:
	char myChar;	
};

std::shared_ptr<IParser<std::string, std::string>> ch(char c)
{
	static Memoizer<char, std::shared_ptr<IParser<std::string, std::string>>> memoizer;
	if (memoizer.contains(c))
		return memoizer.get(c);
	return memoizer.insert(c, new CharParser(c));
}

}

#endif
