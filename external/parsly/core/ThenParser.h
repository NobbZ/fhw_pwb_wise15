#pragma once

#include "Parser.h"

template<typename T, typename U>
class ThenParser : public Parser<std::pair<T, U>> {
	Parser<T>& first;
	Parser<U>& last;
public:
	ThenParser(Parser<T>& pt, Parser<U>& pu) :first(pt), last(pu) {}

	virtual Maybe<std::pair<std::pair<T, U>, std::string>> operator() (std::string input) override
	{
		auto firstResult = first(input);
		if (!firstResult)
			return None<std::pair<std::pair<T, U>, std::string>>();

		auto lastResult = last((*firstResult).second);
		if (!firstResult)
			return None<std::pair<std::pair<T, U>, std::string>>();

		return Just(
				std::make_pair(
					std::make_pair(
						(*firstResult).first,
						(*lastResult).first),
					(*lastResult).second));
	}
};

