
#ifndef PARSNIP_REDUCERS_H
#define PARSNIP_REDUCERS_H

#include "ParsnipBase.h"

namespace Parsnip
{

template <typename In> In concat	(const In& x1, const In& x2) { return x1 + x2; }
template <typename In> In add		(const In& x1, const In& x2) { return x1 + x2; }
template <typename In> In subtract 	(const In& x1, const In& x2) { return x1 - x2; }
template <typename In> In mult 		(const In& x1, const In& x2) { return x1 * x2; }
template <typename In> In div 		(const In& x1, const In& x2) { return x1 / x2; }
template <typename In> In mod 		(const In& x1, const In& x2) { return x1 % x2; }

template <typename In>
std::list<In> make_list (const In& x1, const In& x2)
{
	return {x1, x2};
}

}


#endif
