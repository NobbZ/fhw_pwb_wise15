/*
 *
 */

#pragma once

class InputBuffer {
public:
	typedef typename std::string::const_iterator IteratorT;

	InputBuffer(const std::string& string)
	:buffer(string)
	, currentChar(buffer.begin())
	{}

	InputBuffer(std::string&& string)
	:buffer(string)
	, currentChar(buffer.begin())
	{}

	InputBuffer(const char* string)
	:buffer(string)
	, currentChar(buffer.begin())
	{}

	InputBuffer(const char* string, std::size_t count)
	:buffer(string, count)
	, currentChar(buffer.begin())
	{}

	IteratorT
protected:
private:
	std::string buffer;
	iteratorT currentChar;
};
