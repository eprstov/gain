#ifndef __GAIN_CALENDAR_HEADER__
#define __GAIN_CALENDAR_HEADER__

#include "basic.h"
#include "enum.h"
#include "ranges/accumulate.h"
#include "ranges/views/iota.h"
#include "ranges/views/drop.h"
#include "ranges/views/repeat.h"
#include "ranges/views/transform.h"
#include "ranges/views/zip_transform.h"

#include <array>
#include <algorithm>
#include <bit>
#include <cassert>
#include <compare>
#include <functional>
#include <type_traits>
#include <tuple>
#include <variant>
#include <vector>
#include <utility>

namespace gain::calendar
{

enum Unit
{
	MINUTE, HOUR, DAY, WEEK, MONTH, YEAR
};

constexpr Unit operator+( Unit unit, std::integral auto shift )
{
	return Unit( to_underlying(unit) + shift );
}

constexpr Unit operator-( Unit unit, std::integral auto shift )
{
	return Unit( to_underlying(unit) - shift );
}

} // namespace gain::calendar

template<> struct std::numeric_limits< ::gain::calendar::Unit > : ::gain::EnumLimits< ::gain::calendar::MINUTE, ::gain::calendar::YEAR >
{
};

namespace gain::calendar
{

constexpr auto UNITS = std::size_t( std::numeric_limits<Unit>::max() ) + 1;

struct Span;
struct Recurrence;
struct Interval;

struct Shift
{
	using Value = int;

	Unit unit = MINUTE;
	Value value = 0;

	constexpr Shift& operator+=( auto shift ) requires requires { value += shift; }
	{
		value += shift;
		return *this;
	}

	constexpr Shift& operator-=( auto shift ) requires requires { value -= shift; }
	{
		value -= shift;
		return *this;
	}

	constexpr Shift& operator+=(Shift);
	constexpr Shift& operator-=(Shift);

	constexpr Shift& operator++()
	{
		return operator+=( std::integral_constant< Value, 1 >{} );
	}

	constexpr Shift operator++(int)
	{
		return {unit, value++};
	}

	constexpr Shift& operator--()
	{
		return operator-=( std::integral_constant< Value, 1 >{} );
	}

	constexpr Shift operator--(int)
	{
		return {unit, value--};
	}

	constexpr explicit operator bool() { return value || unit != MINUTE; }

	constexpr bool operator==( const Shift& ) const = default;
};

Shift operator+( const Shift& shift, std::integral auto offset )
{
	return {shift.unit, shift.value + offset};
}

Shift operator-( const Shift& shift, std::integral auto offset )
{
	return {shift.unit, shift.value - offset};
}

template< std::size_t i = 0, std::size_t N, typename... Args >
constexpr std::size_t Fill( std::array< Shift, N >& array, Unit unit, Shift::Value value, Args... args )
requires (sizeof...(Args) == 0 || i < N && requires { Fill< i+1 >( array, args... ); } )
{
	array[i] = Shift{ unit, value };

	if constexpr( sizeof...(Args) )
	{
		return Fill< i+1 >( array, args... );
	}

	return i+1;
}

using Duration = Shift;

struct Span
{
	using Value = Shift::Value;

	Unit unit;
	Value from, to;

	constexpr Span() : unit(YEAR), from(0), to(0)
	{
	}

	constexpr Span( Unit unit, std::convertible_to<Value> auto from, std::convertible_to<Value> auto to ) : unit(unit), from(from), to(to)
	{
	}

	constexpr Span( Shift shift ) : unit( shift.unit ), from( shift.value ), to( shift.value + 1 )
	{
	}

	constexpr Span& operator+=( const Span& );
	constexpr Span& operator-=( const Span& );

	constexpr bool empty() const { return from == to; }
	constexpr Value length() const { return to - from; }

	constexpr operator Shift() const
	{
		return { unit, to - from };
	}
};

struct Recurrence
{
	using Rate = unsigned;

	Unit unit;
	unsigned rate = 1;
};

struct Path : std::array< Shift, UNITS >
{
	static constexpr auto CAPACITY = UNITS;
	using Base = std::array< Shift, CAPACITY >;

	using Base::Base;

	template< typename... Args >
	constexpr Path( Args... args ) requires (sizeof...(Args) > 0) && requires( Base base ) { Fill( base, args... ); }
	{
		auto i = Fill( *this, args... );
		assert( i > 0 );

		const auto unit = operator[]( i-1 ).unit;
		for( ; i < CAPACITY; ++i )
		{
			operator[](i).unit = unit;
		}
	}

	constexpr operator Unit() const
	{
		return operator[]( CAPACITY-1 ).unit;
	}

	constexpr std::size_t size() const
	{
		const auto term = operator[]( CAPACITY-1 ).unit;

		for( auto i = capacity() - 1; i; )
		{
			auto& shift = operator[]( --i );
			if( shift.unit != term )
			{
				return i + 2;
			}
			else if( shift.value != 0 )
			{
				return i + 1;
			}
		}

		return 1;
	}

	constexpr std::size_t capacity() const
	{
		return CAPACITY;
	}
};

struct Interval
{
	constexpr Interval() : from( YEAR, 0 ), to( YEAR, 0 )
	{
	}

	constexpr Interval( const Path& from, const Path& to ) : from(from), to(to)
	{
	}

	constexpr Interval( Span span ) : from( span.unit, span.from ), to( span.unit, span.to )
	{
	}

	constexpr Interval( Shift shift ) : Interval( Span{shift} )
	{
	}

	Path from, to;
};

template< int step, Unit unit > constexpr Recurrence EVERY{ unit, step };
constexpr Recurrence YEARLY{YEAR};
constexpr Recurrence MONTHLY{MONTH};
constexpr Recurrence WEEKLY{WEEK};
constexpr Recurrence DAYLY{DAY};
constexpr Recurrence HOURLY{HOUR};
constexpr Recurrence MINUTELY{MINUTE};

template< int from, int to = from > constexpr Span YEARS{ YEAR, from, to+1 };
template< int from, int to = from > constexpr Span MONTHS{ MONTH, from, to+1 };
template< int from, int to = from > constexpr Span WEEKS{ WEEK, from, to+1 };
template< int from, int to = from > constexpr Span DAYS{ DAY, from, to+1 };
template< int from, int to = from > constexpr Span HOURS{ HOUR, from, to+1 };
template< int from, int to = from > constexpr Span MINUTES{ MINUTE, from, to+1 };

template< int length > constexpr Shift YEARS< length, length >{ YEAR, length };
template< int length > constexpr Shift MONTHS< length, length >{ MONTH, length };
template< int length > constexpr Shift WEEKS< length, length >{ WEEK, length };
template< int length > constexpr Shift DAYS< length, length >{ DAY, length };
template< int length > constexpr Shift HOURS< length, length >{ HOUR, length };
template< int length > constexpr Shift MINUTES< length, length >{ MINUTE, length };

enum class CE : Shift::Value
{
};

namespace detail
{
namespace leap
{

constexpr bool Check( int year )
{
	year += 1970;
	return (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;
}

constexpr auto Count( CE year )
{
	const auto y = to_underlying(year);
	return y / 4 - y / 100 + y / 400;
}

constexpr int Count( int year )
{
	return Count( CE{ year + 1970 } ) - Count( CE{1970} );
}

template< typename T > constexpr std::array< std::tuple< T, T >, 4 > thresholds{ {{ 400, 97 }, { 100, 24 }, { 4, 1 }, { 1, 0 }} };

} // namespace leap

template< std::integral T > constexpr std::tuple< T, T > day2year( T day )
{
	T year = 1;

	constexpr T day1970 = 365*1969 + leap::Count( CE{1970} );
	day += day1970;

	for( auto i = 0; i < leap::thresholds<T>.size(); ++i )
	{
		auto [years, xdays] = leap::thresholds<T>[i];
		if( auto div = std::div( std::make_signed_t<T>(day), std::make_signed_t<T>( years * 365 + xdays ) ); div.quot )
		{
			year += div.quot * years;
			day = div.rem;

			if( i && div.quot * years == std::get<0>( leap::thresholds<T>[ i-1 ] ) )
			{
				year -= years;
				day += 365 * years;
			}
		}
	}

	return { year - 1970, day };
}

} // namespace detail

template< std::integral T > constexpr T Convert( T value, Unit from, Unit to )
{
	if( from == to )
	{
		return value;
	}

	constexpr Unit path[UNITS][UNITS] =
	{
		// from MINUTE
		{ MINUTE, HOUR, HOUR, HOUR, HOUR, HOUR },
		// from HOUR
		{ MINUTE, HOUR, DAY, DAY, DAY, DAY },
		// from DAY
		{ HOUR, HOUR, DAY, WEEK, MONTH, YEAR },
		// from WEEK
		{ DAY, DAY, DAY, WEEK, DAY, DAY },
		// from MONTH
		{ DAY, DAY, DAY, DAY, MONTH, YEAR },
		// from YEAR
		{ DAY, DAY, DAY, DAY, MONTH, YEAR }
	};

	if( const auto next = path[from][to]; next != to )
	{
		return Convert( Convert( value, from, next ), next, to );
	}

	const auto step = [] (Unit from, Unit to) constexpr { return from << std::bit_width(UNITS) | to; };

	auto mdays = [] (T year) constexpr -> std::array< T, 12 >
	{
		constexpr std::array< T, 12 > y365 = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };
		constexpr std::array< T, 12 > y366 = { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 };

		return detail::leap::Check(year) ? y366 : y365;
	};

	switch( step( from, to ) )
	{
	case step( YEAR, MONTH ):
		return value * 12;

	case step( MONTH, YEAR ):
		return value / 12;

	case step( YEAR, DAY ):
		return value * 365 + detail::leap::Count( value - 1 );

	case step( DAY, YEAR ):
		return std::get<0>( detail::day2year(value) );

	case step( MONTH, DAY ):
		{
			value += 1969 * 12;

			T year = value / 12 - 1969;
			T month = value % 12;

			T day = Convert( year, YEAR, DAY );
			return Convert( year, YEAR, DAY ) + mdays(year)[month];
		}

	case step( DAY, MONTH ):
		{
			auto [year, day] = detail::day2year(value);

			auto days = mdays(year);
			return year * 12 + T( std::distance( days.begin(), std::upper_bound( days.begin(), days.end(), day ) ) ) - 1;
		}

	case step( DAY, WEEK ):
		return (value - 4) / 7;

	case step( WEEK, DAY ):
		return value * 7 + 4;

	case step( DAY, HOUR ):
		return value * 24;

	case step( HOUR, DAY ):
		return value / 24;

	case step( HOUR, MINUTE ):
		return value * 60;

	case step( MINUTE, HOUR ):
		return value / 60;
	}

	assert(false);
	return value;
}

constexpr Shift Convert( Shift shift, Unit unit )
{
	return { unit, Convert( shift.value, shift.unit, unit ) };
}

constexpr Span Convert( const Span& span, Unit unit )
{
	const auto convert = [] (const Span& span, Unit unit) constexpr
	{
		const auto from = span.from != -INF ? Convert( span.from, span.unit, unit ) : span.from;
		const auto to = span.to != INF ? (unit <= span.unit ? Convert( span.to, span.unit, unit ) : Convert( span.to - 1, span.unit, unit ) + 1) : span.to;

		return Span{ unit, from, to };
	};

	if( span.unit == WEEK && !(unit <= span.unit) || unit == WEEK && !(span.unit <= unit) )
	{
		return convert( convert( span, DAY ), unit );
	}

	return convert( span, unit );
}

inline auto operator/( const Span& span, const Recurrence& recurrence )
{
	using namespace ranges::views;

	auto [_, from, to] = Convert( span, recurrence.unit );
	return iota( from, to, Shift::Value( recurrence.rate ) ) | transform( [unit = recurrence.unit] (auto value) { return Shift{ unit, value }; } );
}

inline auto operator/( Shift shift, const ::gain::ranges::sized_range_of<Unit> auto& units )
{
	using namespace ::gain::ranges;
	assert( !std::empty(units) && std::adjacent_find( units.begin(), units.end(), std::greater_equal<>{} ) == units.end() );

	return views::repeat( nullptr, units.size() ) | views::transform() << [&, it = units.begin(), curr = Convert( shift, *units.begin() )] (auto) mutable
	{
		assert( it != units.end() );

		if( ++it != units.end() )
		{
			auto result = curr;
			curr = Convert( curr, *it );

			return result -= curr;
		}

		return curr;
	};
}

constexpr Span operator+( const Shift& shift, const Interval& interval )
{
	enum Bound { FROM, TO };
	constexpr auto apply = [] (Bound bound) consteval
	{
		return [bound] (const Shift& shift, const Path& path, Unit unit)
		{
			Shift result = shift;

			const auto apply = [&result, bound] (const Shift& shift)
			{
				if( shift.unit != result.unit )
				{
					if( shift.unit == WEEK && result.unit > WEEK )
					{
						result = Convert( result, DAY );
					}

					result = bound == FROM || shift.unit <= result.unit ? Convert( result, shift.unit ) : ++Convert( --result, shift.unit );
				}

				if( shift.value )
				{
					result += shift.value;
				}
			};

			for( const auto shift : path )
			{
				apply(shift);
			}

			apply( {unit, 0} );

			return result.value;
		};
	};

	const Unit unit = std::min<Unit>( interval.from, interval.to );

	const auto from = apply(FROM)( shift, interval.from, unit );
	const auto to = apply(TO)( shift, interval.to, unit );

	return { unit, from, to };
}

constexpr Span operator+( const Shift& shift, const Span& span )
{
	const auto add = [] (const Shift& shift, const Span& span)
	{
		assert( shift.unit == span.unit );
		return Span{ shift.unit, shift.value + span.from, shift.value + span.to };
	};

	if( shift.unit < span.unit )
	{
		return add( shift, Convert( span, shift.unit ) );
	}
	else if( span.unit < shift.unit )
	{
		return add( Convert( shift, span.unit ), span );
	}

	return add( shift, span );
}

constexpr Span operator/( const Span& outter, const Span& inner )
{
	assert( inner.unit <= outter.unit );

	const auto extract = [] (const Span& outter, const Span& inner)
	{
		assert( inner.unit == outter.unit );
		return Span{ inner.unit, outter.from + inner.from, outter.to + inner.to };
	};

	if( outter.unit != inner.unit )
	{
		return extract( Convert( outter, inner.unit ), inner );
	}

	return extract( outter, inner );
}

constexpr Span operator&( const Span& left, const Span& right )
{
	const auto intersect = [] (const Span& left, const Span& right)
	{
		assert( left.unit == right.unit );

		const auto from = std::max( left.from, right.from );
		const auto to = std::min( left.to, right.to );

		return Span{ left.unit, from, std::max( from, to ) };
	};

	if( left.unit < right.unit )
	{
		return intersect( left, Convert( right, left.unit ) );
	}
	else if( left.unit > right.unit )
	{
		return intersect( Convert( left, right.unit ), right );
	}

	return intersect( left, right );
}

constexpr bool operator&( const Span& span, const Shift& shift )
{
	const auto contains = [] (const Span& span, const Shift& shift)
	{
		assert( span.unit == shift.unit );
		return span.from <= shift.value && shift.value < span.to;
	};

	if( span.unit < shift.unit )
	{
		return contains( span, Convert( shift, span.unit ) );
	}
	else if( span.unit > shift.unit )
	{
		return contains( Convert( span, shift.unit ), shift );
	}

	return contains( span, shift );
}

constexpr Shift& Apply( Shift& left, Shift right, auto op )
{
	if( left.unit < right.unit )
	{
		op( left.value, Convert( right.value, right.unit, left.unit ) );
	}
	else
	{
		if( right.unit < left.unit )
		{
			left.value = Convert( left.value, left.unit, right.unit );
			left.unit = right.unit;
		}

		op( left.value, right.value );
	}

	return left;
}

constexpr Shift& Shift::operator+=( Shift other )
{
	constexpr auto op = [] (auto& left, auto right) { left += right; };
	return Apply( *this, other, op );
}

constexpr Shift& Shift::operator-=( Shift other )
{
	constexpr auto op = [] (auto& left, auto right) { left -= right; };
	return Apply( *this, other, op );
}

constexpr Shift operator+( Shift left, Shift right )
{
	return left += right;
}

constexpr Shift operator-( Shift left, Shift right )
{
	return left -= right;
}

constexpr Span& Apply( Span& left, Span right, auto op )
{
	if( left.unit < right.unit )
	{
		right = Convert( right, left.unit );
	}
	else if( right.unit < left.unit )
	{
		left = Convert( left, right.unit );
	}

	op( left.from, right.from );
	op( left.to, right.to );

	return left;
}

constexpr Span& Span::operator+=( const Span& other )
{
	constexpr auto op = [] (auto& left, auto right) { left += right; };
	return Apply( *this, other, op );
}

constexpr Span& Span::operator-=( const Span& other )
{
	constexpr auto op = [] (auto& left, auto right) { left -= right; };
	return Apply( *this, other, op );
}

constexpr Duration operator*( const Shift& shift, auto scale ) requires
requires { {shift.value * scale} -> std::convertible_to< Shift::Value >; }
{
	return { shift.unit, Shift::Value( shift.value * scale ) };
}

constexpr Shift::Value Approximate( Unit from, Unit to )
{
	if( to < from )
	{
		switch(from)
		{
		case HOUR:
			if( to == MINUTE )
			{
				return 60;
			}

			break;

		case DAY:
			if( to == HOUR )
			{
				return 24;
			}

			break;

		case WEEK:
			if( to == DAY )
			{
				return 7;
			}

			break;

		case MONTH:
			switch(to)
			{
			case WEEK:
				return 5;

			case DAY:
				return 31;
			}

			break;

		case YEAR:
			switch(to)
			{
			case DAY:
				return 366;

			case WEEK:
				return 53;

			case MONTH:
				return 12;
			}

			break;
		}

		const auto next = from - 1;
		return Approximate( from, next ) * Approximate( next, to );
	}

	return from == to ? 1 : 0;
}

constexpr auto Approximate( const Shift& shift, Unit unit )
{
	const auto length = shift.value;

	if( unit < shift.unit )
	{
		if( shift.unit == MONTH && length > 12 )
		{
			return length / 12 * Approximate( YEAR, unit ) + length % 12 * Approximate( MONTH, unit );
		}

		return length * Approximate( shift.unit, unit );
	}

	return length / Approximate( unit, shift.unit );
}

constexpr Duration Approximate( const Interval& interval )
{
	const auto unit = std::min<Unit>( interval.from, interval.to );

	const auto approximate = [unit] (const auto& ref) -> Shift::Value
	{
		return ref | ranges::views::transform( [unit] (auto& shift) { return Approximate( shift, unit ); } ) | ranges::accumulate();
	};

	return { unit, approximate( interval.to ) - approximate( interval.from ) };
}

} // namespace gain::calendar

#endif // !__GAIN_CALENDAR_HEADER__
