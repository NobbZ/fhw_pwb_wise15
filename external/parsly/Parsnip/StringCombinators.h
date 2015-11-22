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

#ifndef PARSNIP_STRING_PARSER_BASE_H
#define PARSNIP_STRING_PARSER_BASE_H

#include "StringConcatParser.h"

namespace Parsnip
{

// sepBy should build lists of strings
std::shared_ptr<IParser<std::string, std::list<std::string>>> sepBy(std::shared_ptr<IParser<std::string, std::string>> data, std::shared_ptr<IParser<std::string, std::string>> sep, int min = 0, int max = INT_MAX)
{
	return sepBy<BuildList<std::string>, std::string, std::string>(data, sep, min, max);
}

std::shared_ptr<IParser< std::string, std::list<std::string>>> sepBy1(std::shared_ptr<IParser<std::string, std::string>> data, std::shared_ptr<IParser<std::string, std::string>> sep, int max = INT_MAX)
{
	return sepBy(data, sep, 1, max);
}

std::shared_ptr<IParser< std::string, std::list<std::string>>> sepByAtleast(std::shared_ptr<IParser<std::string, std::string>> data, std::shared_ptr<IParser<std::string, std::string>> sep, int min)
{
	return sepBy(data, sep, min);
}

//optional string parsers return the empty string
std::shared_ptr<IParser<std::string, std::string>> optional(std::shared_ptr<IParser<std::string, std::string>> parser)
{
	return optional(parser, std::string(""));
}

// many should concatenate strings by default 
std::shared_ptr<IParser<std::string, std::string>> many(std::shared_ptr<IParser<std::string, std::string>> a, int min = 0, int max = INT_MAX)
{
	return many<Concat<std::string>, std::string, std::string>(a, min, max);
}

std::shared_ptr<IParser<std::string, std::string>> many1(std::shared_ptr<IParser<std::string, std::string>> a)
{
	return many(a, 1);
}

std::shared_ptr<IParser<std::string, std::string>> atleast(std::shared_ptr<IParser<std::string, std::string>> a, int min)
{
	return many(a, min);
}

}

#endif
