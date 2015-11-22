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

#ifndef PARSNIP_PARSER_H
#define PARSNIP_PARSER_H

#include "ParsnipConfig.h"
#include "ParsnipBase.h"
#include "Accumulators.h"
#include "Reducers.h"
#include "Result.h"
#include "Cache.h"

#include "Reader.h"
#include "ParsnipDebug.h"

namespace Parsnip
{
template <typename In, typename Out>
struct IParser
{	
	typedef  typename Reader<In>::IndexT PosT;
	typedef std::pair<Result<Out>, PosT> ResultPosPair;
	typedef Cache<In, Out, PosT, ResultPosPair> CacheT;
	typedef Maybe<ResultPosPair> MaybeValue; 

	iparser() :trace(false)
	{
		this->setname(to_string(this));
	}
	
	virtual ~IParser() 
	{
		cache.removeParser(this);
	}

	virtual Result<Out> parse()
	{
		bool debug = trace || ParsnipConfig::traceAll;
		
		if (debug) 
		{
			typename Reader<In>::IndexT pos = Reader<In>::pos();

			ParsnipDebug::output_indent();

			std::cout << "(" << ParsnipDebug::indent << ") " << "Entering " <<  this->getName() << " at pos " << pos << " ";

			if (!Reader<In>::hasNext())
			{
				std::cout << "on end of stream ";
			}
			else if (pos >= 0)
			{
				std::cout << "on '" << Reader<In>::curr() <<  + "' ";
			}

			std::cout << std::endl;
			ParsnipDebug::increment_indent();
		}

		Result<Out> result; 

		switch(ParsnipConfig::strategy)
		{
		case (ParsnipConfig::SIMPLE):
			result =  this->eval();
			break;

		case (ParsnipConfig::PACKRAT):
			result = parse_packrat();
			break;
		}
		
		if (debug)
		{
			ParsnipDebug::decrement_indent();
		
			PosT pos = Reader<In>::pos();

			ParsnipDebug::output_indent();

			std::cout << "(" << ParsnipDebug::indent << ") " <<  "Exiting " << this->getName() << " at pos " << pos << " ";

			
			if (!Reader<In>::hasNext())
			{
				std::cout << "on end of stream ";
			}
			else if (pos >= 0)
			{
				std::cout << "on '" <<Reader<In>::curr() <<  + "' ";
			}

			if (result)
			{	
				std::cout << "OK "; 
			}
			else
			{	
				std::cout << "FAIL ";
			}

			std::cout << std::endl;			
		}

		return result;
	}

	Result<Out> parse_packrat()
	{
		PosT P = Reader<In>::pos();
		
		//see if there is a memo for this parser at the current position
		MaybeValue saved = cache.get(this, P);
		
		//if already memoized, return the memo
		if (saved) 
		{
			ResultPosPair rp = saved.get();
			Reader<In>::set_pos(rp.second);	
			return rp.first; 
		}

		//if not memoized do the parse
		Result<Out> ans = this->eval();
		
		//how far did we get?
		PosT newPos = Reader<In>::pos();

		//...and memoize it!
		cache.insert(this, P, std::make_pair(ans, newPos));

		return ans;
	}

	virtual std::string getName() { return myName; }
	virtual void setName(const std::string& str) { myName = str; }

	virtual std::string toString(unsigned int depth = 0)
	{
		std::string str;
		for (unsigned int i = 0; i < depth; ++i)
			str += "\t";
		str += getName();
		str += "\n";
		return str;
	}

	void setTrace(bool b) { trace = b; }

protected:
	virtual Result<Out> eval() = 0;

private:
	static CacheT cache;

	std::string myName;
	bool trace;
};

template <typename In, typename Out>
typename IParser<In, Out>::CacheT IParser<In, Out>::cache;

/* 
	use Parser<O, I>::type 
	because C++ doesn't yet
	have template typedefs
*/

template <typename In, typename Out>
using Parser = std::shared_ptr<IParser<In, Out>>;


template<typename In, typename Out>
std::shared_ptr<IParser<In, Out>> operator >>= (const std::string& name, std::shared_ptr<IParser<In, Out>> p)
{
	p->setName(name);
	return p;
}

template<typename In, typename Out>
std::shared_ptr<IParser<In, Out>> operator >>= (std::shared_ptr<IParser<In, Out>> p, const std::string& name)
{
	p->setName(name);
	return p;
}

} // namespace

#endif
