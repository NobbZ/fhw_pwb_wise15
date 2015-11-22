/*
 *
 */

#pragma once

#include "Parser.h"

#ifdef CLASSES
class Char : public Parser<char> {
public:
	Char(char c)
	:matchchar(c)
	{}

	virtual Maybe<std::pair<char, std::string>> operator() (std::string input) {
		if (!input.empty() and input[0] == matchchar)
			return Just(std::make_pair(matchchar, input.substr(1)));
		return Maybe<std::pair<char, std::string>>(std::make_tuple('\0', input), false);
	}
protected:
private:
	char matchchar;
};

#else

Parser<char> Char(char matchchar) {
	return [=] (std::string input) -> ParserResultT<char> {
		if (!input.empty() and input[0] == matchchar)
			return Just(std::make_pair(matchchar, input.substr(1)));
		return Maybe<std::pair<char, std::string>>(std::make_pair('\0', input), false);
	};
}

#endif
