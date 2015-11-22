/*
 *
 */

#pragma once

////////////////////////////////////////////////////////////////////////////////
template<typename ParserOutput, typename ParserInput = std::string>
using ParserResultT = Maybe<std::pair<ParserOutput, ParserInput>>;

template<typename ParserOutput, typename ParserInput = std::string>
using Parser = std::function<ParserResultT<ParserOutput, ParserInput>(ParserInput)>;

/*template<typename ParserOutput>
using ParserResultT = typename Parser<ParserOutput>::result_type;
*/

