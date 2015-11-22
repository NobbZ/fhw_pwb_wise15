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

#ifndef PARSNIP_CHAR_COMBINATORS_H
#define PARSNIP_CHAR_COMBINATORS_H

#include "CharParser.h"
#include "StringConcatParser.h"

namespace Parsnip
{

// tuple sequencing
std::shared_ptr<IParser<std::string, Tuple2<std::string, std::string>>> operator >> (char c, std::shared_ptr<IParser<std::string, std::string>> p)
{
	return ch(c) >> p;
}

std::shared_ptr<IParser<std::string, Tuple2<std::string, std::string>>> operator >> ( std::shared_ptr<IParser<std::string, std::string>> p, char c)
{
	return p >> ch(c);
}

// alternation
std::shared_ptr<IParser<std::string, std::string>> operator | (char c, std::shared_ptr<IParser<std::string, std::string>> p)
{
	return ch(c) | p;
}

std::shared_ptr<IParser<std::string, std::string>> operator | ( std::shared_ptr<IParser<std::string, std::string>> p, char c)
{
	return p | ch(c);
}

// concat sequence
std::shared_ptr<IParser<std::string, std::string>> operator + (char c, std::shared_ptr<IParser<std::string, std::string>> p)
{
	return ch(c)+ p;
}

std::shared_ptr<IParser<std::string, std::string>> operator + ( std::shared_ptr<IParser<std::string, std::string>> p, char c)
{
	return p +  ch(c);
}

}
#endif
