/*
 *
 */

#pragma once

#include "Parser.h"


template <class T1, class T2>
class Either
{
    bool isLeft;
    union
    {
        T1 left;
        T2 right;
    };
};

#ifdef CLASSES
/*
template<typename T>
class Option : Parser<T> {
	vector<Parser<T>&> lhs;
	Parser<T>& rhs;

public:
	Option(Parser<T>& p1, Parser<T>& p2) :lhs(p1), rhs(p2) {}

	virtual Maybe<std::pair<T, std::string>> operator() (std::string input) override {
		auto result = lhs(input);
		if (!result)
			result = rhs(input);

		return result;
	}
};
*/
 
#elif 0
template<typename T, typename U>
Parser<Either<T,U>> Option(Parser<T>, Parser<U>) {
	return [=] (std::string input) -> ParserResultT<Either<T,U>> {
		if (!input.empty())
		{
			return Just(std::make_pair(matchStr, input.substr(matchStr.size())));
		}
		return None(std::make_pair(Either<T,U>, input));
	};
}
#endif

/*
 * auto p = (char('c') + number(4)) | string("derp") | string("herp");
 */
