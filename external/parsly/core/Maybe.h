/*
 *
 */
#pragma once

#include <functional>

/////////////////////////////////////////////////////////////////////////////////
template <typename T>
struct Maybe {
public:
	Maybe(T value, bool valid)
	:value(value)
	, hasValue(valid)
	{}

	operator bool() const	{ return hasValue; }
	bool operator!() const	{ return !hasValue; }

	template<typename T1, typename T2>
	Maybe<T2> operator>>=(std::function<Maybe<T2>(T1)>& f)
	{
		if(!hasValue)
			return {T2(), false};
		else
			return f(value);
	}

	const T& operator*() const { return value; }
	//const T& operator->() const { return value; }

private:
	T value;
	bool hasValue;
};

/////////////////////////////////////////////////////////////////////////////////
template<typename T>
Maybe<T> None()
{
	return {T(), false};
}

/////////////////////////////////////////////////////////////////////////////////
template<typename T>
Maybe<T> None(T value)
{
	return {value, false};
}

/////////////////////////////////////////////////////////////////////////////////
template<typename T>
Maybe<T> Just(T value)
{
	return {value, true};
}

