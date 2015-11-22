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

#ifndef PARSNIP_STRING_CONCAT_PARSER_H
#define PARSNIP_STRING_CONCAT_PARSER_H

#include <string>
#include <memory>

#include "Parser.h"
#include "ManyParser.h"

namespace Parsnip
{
	
struct StringConcatParser : public IParser<std::string, std::string>
{
	StringConcatParser(std::shared_ptr<IParser<std::string, std::string>> _a, std::shared_ptr<IParser<std::string, std::string>> _b)
	: a(_a)
	, b(_b) 
	{
		this->setName("str_concat");
	}

protected:
	virtual Result<std::string> eval()
	{
		Result<std::string> result1 = a->parse();

		if (result1)
		{
			Result<std::string> result2 = b->parse();
			if (result2) 
			{
				return Result<std::string>::succeed(result1.data() + result2.data());
			}
			
			return Result<std::string>::fail();
		}
		
		return Result<std::string>::fail();	
	}

	std::shared_ptr<IParser<std::string, std::string> > a;
	std::shared_ptr<IParser<std::string, std::string> > b;
};

std::shared_ptr<IParser<std::string, std::string> > concat (std::shared_ptr<IParser<std::string, std::string> > a, std::shared_ptr<IParser<std::string, std::string> > b)
{
	typedef IParser<std::string, std::string> StrParser;
	static Memoizer<std::pair<StrParser*, StrParser*>, std::shared_ptr<StrParser>> memoizer;
	std::pair<StrParser*, StrParser*> p_pair = std::make_pair(a.GetRawPointer(), b.GetRawPointer());
	if (memoizer.contains(p_pair))
		return memoizer.get(p_pair);
	return memoizer.insert(p_pair, new StringConcatParser(a,b));
}

std::shared_ptr<IParser<std::string, std::string> > operator+ (std::shared_ptr<IParser<std::string, std::string> > a, std::shared_ptr<IParser<std::string, std::string> > b)
{
	return concat(a, b);
}

std::shared_ptr<IParser<std::string, std::string> > operator+ (std::shared_ptr<IParser<std::string, void> > a, std::shared_ptr<IParser<std::string, std::string> > b)
{
	return a >> b;
}

std::shared_ptr<IParser<std::string, std::string> > operator+ (std::shared_ptr<IParser<std::string, std::string> > a, std::shared_ptr<IParser<std::string, void> > b)
{
	return a >> b;
}

std::shared_ptr<IParser<std::string, void> > operator+ (std::shared_ptr<IParser<std::string, void> > a, std::shared_ptr<IParser<std::string, void> > b)
{
	return a >> b;
}

}

#endif
