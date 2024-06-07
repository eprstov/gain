#ifndef __GAIN_GTUPLE_HEADER__
#define __GAIN_GTUPLE_HEADER__

#include "tuple.h"

namespace gain
{
namespace trait
{

template< typename T > inline constexpr bool is_tagged = false;
template< template< typename, auto > typename Pack, typename T, auto tag > inline constexpr bool is_tagged< Pack< T, tag > > = true;
template< typename T > concept tagged = is_tagged<T>;

} // namespace trait
namespace detail
{

template< std::size_t, typename... > struct TagTuple;

template< std::size_t i, typename... Ts, auto... tags, typename T, auto tag, template< typename, decltype(tag) > typename Match, typename... Matches >
struct TagTuple< i, std::tuple< Ts... >, StaticTuple< tags... >, Match< T, tag >, Matches... > : TagTuple< i+1, std::tuple< Ts..., T >, StaticTuple< tags..., tag >, Matches... >
{
	using Base = TagTuple< i+1, std::tuple< Ts..., T >, StaticTuple< tags..., tag >, Matches... >;
	using Base::Base;

	template< decltype(tag) tg > requires (tg == tag) auto& get()
	{
		return Base::template get< std::integral_constant< std::size_t, i > >();
	}

	template< decltype(tag) tg > requires (tg == tag) auto& get() const
	{
		return Base::template get< std::integral_constant< std::size_t, i > >();
	}

	template< typename Wrapped > requires requires( Base base )
	{
		base.template get<Wrapped>();
	}
	auto& get()
	{
		return Base::template get<Wrapped>();
	}

	template< typename Wrapped > requires requires( Base base )
	{
		base.template get<Wrapped>();
	}
	auto& get() const
	{
		return Base::template get<Wrapped>();
	}

	template< auto tg > requires requires( Base base )
	{
		base.template get<tg>();
	}
	auto& get()
	{
		return Base::template get<tg>();
	}

	template< auto tg > requires requires( Base base )
	{
		base.template get<tg>();
	}
	auto& get() const
	{
		return Base::template get<tg>();
	}
};

template< std::size_t n, typename... Ts, auto... tags >
struct TagTuple< n, std::tuple< Ts... >, StaticTuple< tags... > > : gain::Tuple< Ts... >
{
	static_assert( n == sizeof...(Ts) && n == sizeof...(tags) );

	using Base = gain::Tuple< Ts... >;
	using Base::Base;

	template< typename T > static constexpr bool isindex = std::is_same_v< decltype( T::value ), const std::size_t >;

	template< typename Wrapped > requires isindex<Wrapped> auto& get()
	{
		return Base::template get< Wrapped::value >();
	}

	template< typename Wrapped > requires isindex<Wrapped> auto& get() const
	{
		return Base::template get< Wrapped::value >();
	}

	using Tags = StaticTuple< tags... >;
};

} // namespace detail

template< typename, auto > struct Tagged;
template< std::size_t value > using TupleIdx = std::integral_constant< std::size_t, value >;

template< typename... Ts >
using TagTuple = detail::TagTuple< 0, std::tuple<>, StaticTuple<>, Ts... >;

} // namespace gain

#endif // __GAIN_GTUPLE_HEADER__
