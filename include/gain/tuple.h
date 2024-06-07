#ifndef __GAIN_TUPLE_HEADER__
#define __GAIN_TUPLE_HEADER__

#include "basic.h"

#include <cassert>
#include <concepts>
#include <tuple>
#include <type_traits>
#include <utility>

namespace gain
{

inline constexpr struct {} skip;

template< auto... > struct StaticTuple{};

namespace detail
{
namespace tuple
{

template< typename T, std::size_t idx >
struct Piece : T
{
	template< typename Tuple >
	requires requires
	{
		{ std::tuple_size< std::remove_cvref_t<Tuple> >::value } -> std::same_as< const std::size_t& >;
	}
	static constexpr std::size_t size = std::tuple_size_v< std::remove_cvref_t<Tuple> >;

	template< typename Tuple >
	requires requires
	{
		size<Tuple>;
	}
	using Indexes = std::make_index_sequence< size<Tuple> >;

	template< typename Tuple, typename = Indexes<Tuple> > static constexpr bool constructible = false;

	template< class Tuple, std::size_t... I >
	static constexpr bool constructible< Tuple, std::index_sequence< I... > > = requires( Tuple tuple )
	{
		std::is_constructible_v< T, decltype( std::get<I>(tuple) )... >;
	};

	template< class Tuple, std::size_t... I >
	requires constructible< Tuple, std::index_sequence< I... > >
	Piece( std::piecewise_construct_t, Tuple&& tuple, std::index_sequence< I... > ) : T( std::get<I>( std::forward<Tuple>(tuple) )... )
	{
	}

	template< typename Tuple >
	requires constructible<Tuple> && requires
	{
		std::tuple_size< std::remove_cvref_t<Tuple> >::value;
	}
	Piece( std::piecewise_construct_t, Tuple&& tuple ) : Piece( std::piecewise_construct, std::forward<Tuple>(tuple), Indexes<Tuple>{} )
	{
	}

	template< typename Arg >
	requires std::is_constructible_v< T, Arg >
	Piece( Arg&& arg ) : T( std::forward<Arg>(arg) )
	{
	}

	Piece()
	requires std::is_default_constructible_v<T>
	{
	}
};

} // namespace tuple

template< std::size_t, typename... > struct Tuple;

template< std::size_t idx, typename T, typename... Ts >
struct Tuple< idx, T, Ts... > : detail::tuple::Piece< T, idx >, Tuple< idx+1, Ts... >
{
	using Piece = detail::tuple::Piece< T, idx >;
	using Base = Tuple< idx+1, Ts... >;

	template< typename Tuple, typename... ArgsB >
	requires std::is_constructible_v< Piece, std::piecewise_construct_t, Tuple > && std::is_constructible_v< Base, std::piecewise_construct_t, ArgsB... >
	Tuple( std::piecewise_construct_t, Tuple&& tuple, ArgsB&&... args ) : Piece( std::piecewise_construct, std::forward<Tuple>(tuple) ), Base( std::piecewise_construct, std::forward<ArgsB>(args)... )
	{
	}

	template< typename... Args >
	requires std::is_default_constructible_v<Piece> && std::is_constructible_v< Base, std::piecewise_construct_t, Args... >
	Tuple( std::piecewise_construct_t, Args&&... args ) : Base( std::piecewise_construct, std::forward<Args>(args)... )
	{
	}

	template< typename... Args >
	requires std::is_default_constructible_v<Piece> && std::is_constructible_v< Base, std::piecewise_construct_t, Args... >
	Tuple( std::piecewise_construct_t, decltype(skip), Args&&... args ) : Base( std::piecewise_construct, std::forward<Args>(args)... )
	{
	}

	template< typename Arg, typename... Args >
	requires std::is_constructible_v< Piece, Arg > && std::is_constructible_v< Base, Args... >
	Tuple( Arg&& arg, Args&&... args ) : Piece( std::forward<Arg>(arg) ), Base( std::forward<Args>(args)... )
	{
	}

	template< typename... Args >
	requires std::is_default_constructible_v<Piece> && std::is_constructible_v< Base, Args... >
	Tuple( Args&&... args ) : Base( std::forward<Args>(args)... )
	{
	}

	template< typename... Args >
	requires std::is_default_constructible_v<Piece> && std::is_constructible_v< Base, Args... >
	Tuple( decltype(skip), Args&&... args ) : Base( std::forward<Args>(args)... )
	{
	}

	template< std::size_t i > requires (i == idx)
	T& get()
	{
		return static_cast< Piece& >( *this );
	}

	template< std::size_t i > requires (i == idx)
	const T& get() const
	{
		return static_cast< const Piece& >( *this );
	}

	template< std::size_t i > requires requires( Base base )
	{
		base.template get<i>();
	}
	auto& get()
	{
		return Base::template get<i>();
	}

	template< std::size_t i > requires requires( Base base )
	{
		base.template get<i>();
	}
	auto& get() const
	{
		return Base::template get<i>();
	}
};

template< std::size_t i, typename... Ts >
struct Tuple< i, void, Ts... > : Tuple< i+1, Ts... >
{
	using Base = Tuple< i+1, Ts... >;
	using Base::Base;
	using Base::get;
};

template< std::size_t n >
struct Tuple<n>
{
	Tuple() = default;
	Tuple( std::piecewise_construct_t ) {}

	void get();
};

template< std::size_t curr, std::size_t total, typename Visitor, typename... Tuples >
decltype(auto) Visit( Visitor&& visitor, std::size_t i, Tuples&&... tuples )
{
	assert( i < total );

	if constexpr( curr + 1 < total )
	{
		if( curr < i )
		{
			return Visit< curr + 1, total >( std::forward<Visitor>(visitor), i, std::forward<Tuples>(tuples)... );
		}
	}

	return std::forward<Visitor>(visitor)( std::get<curr>( std::forward<Tuples>(tuples) )... );
}

template< auto... > constexpr bool equal = false;
template< auto val > constexpr bool equal<val> = true;

template< auto first, auto second, auto... rest >
constexpr bool equal< first, second, rest... > = first == second && equal< second, rest... >;

} // namespace detail

template< typename... Ts > using Tuple = detail::Tuple< 0, Ts... >;

namespace trait
{

template< typename T, typename I > constexpr bool gettable = false;
template< typename T, std::size_t... idx > constexpr bool gettable< T, std::index_sequence< idx... > > = requires( T tuple )
{
	( std::get<idx>(tuple), ... );
};

template< typename T > concept tuple = gettable< T, std::make_index_sequence< std::tuple_size_v< std::remove_reference_t<T> > > >;

} // namespace trait

template< typename Visitor, typename... Tuples >
requires detail::equal< std::tuple_size_v< std::remove_cvref_t<Tuples> >... >
decltype(auto) Visit( Visitor&& visitor, std::size_t i, Tuples&&... tuples )
{
	static constexpr auto size = std::tuple_size_v< std::remove_cvref_t< trait::First< Tuples... > > >;
	if constexpr(size)
	{
		return detail::Visit< 0, size >( std::forward<Visitor>(visitor), i, std::forward<Tuples>(tuples)... );
	}
}

} // namespace gain

#endif // __GAIN_TUPLE_HEADER__
