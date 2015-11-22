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

#ifndef NOT_PARSER_H
#define NOT_PARSER_H

#include "Parser.h"

#include <memory>

namespace Parsnip
{

template <typename In, typename Out>
struct NotParser : public IParser<In, void>
{
	NotParser(std::shared_ptr<IParser<In, Out>> p)
	: myParser(p)
	{}

	Result<void> eval()
	{
		Reader<In>::IndexT lastPos = Reader<In>::pos();
		Result<Out> result = myParser->parse();
		
		//restore input stream to previous position
		Reader<In>::set_pos(lastPos);

		return result ? Result<void>::fail() : Result<void>::succeed();
	}
	std::shared_ptr<IParser<In, Out>> myParser;
};

template <typename In, typename Out>
std::shared_ptr<IParser<In, void>> not(std::shared_ptr<IParser<In, Out>>& parser)
{
	return new NotParser<In, Out>(parser);
}

/*
template <typename In, typename Out>
std::shared_ptr<IParser<In, void> > operator!(std::shared_ptr<IParser<In, Out> >& parser)
{
	return not(parser);
}
*/

}

#endif
