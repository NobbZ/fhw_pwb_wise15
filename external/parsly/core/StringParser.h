/*
 *
 */
#pragma once

#include "Parser.h"

//------------------------------------------------------------------------------
inline constexpr Parser<std::string> String(std::string matchStr) {
	return [=] (std::string input) -> ParserResultT<std::string> {
		if (!input.empty()
				and std::equal(
					input.begin(),
					input.begin() + std::min(input.size(), matchStr.size()),
					matchStr.begin()))
		{
			return Just(std::make_pair(matchStr, input.substr(matchStr.size())));
		}
		return None(std::make_pair(std::string(""), input));
	};
}
