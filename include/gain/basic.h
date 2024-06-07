#ifndef __GAIN_BASIC_HEADER__
#define __GAIN_BASIC_HEADER__

#include <algorithm>
#include <memory>
#include <type_traits>

namespace gain
{
namespace trait
{

template< typename T > constexpr bool is_shared_ptr = false;
template< typename T > constexpr bool is_shared_ptr< std::shared_ptr<T> > = true;

template< typename T > concept shared_ptr = is_shared_ptr<T>;

template< typename T > concept ostreamable = requires( std::ostream& stream, T value )
{
	{ stream << value } -> std::same_as< std::ostream& >;
};

template< typename T1, typename T2 >
requires ( !std::is_reference_v<T1> )
constexpr bool is_ref_of = std::is_same_v< std::remove_cv_t<T1>, std::remove_cvref_t<T2> > && ( std::is_const_v<T1> || !std::is_const_v< std::remove_reference<T2> > ) && ( std::is_volatile_v<T1> || !std::is_volatile_v< std::remove_reference<T2> > );

template< typename T1, typename T2 >
concept ref_of = is_ref_of< T2, T1 >;

template< typename... Ts > using First = std::tuple_element_t< 0, std::tuple< Ts... > >;;
template< typename... Ts > using Last = std::tuple_element_t< sizeof...(Ts) - 1, std::tuple< Ts... > >;;

} // namespace trait

template< typename T, typename U > requires std::is_lvalue_reference_v<T> && std::is_const_v< std::remove_reference_t<T> >
[[nodiscard]] constexpr auto&& forward( U&& u ) noexcept
{
	return std::as_const(u);
}

template< typename T, typename U > requires std::is_lvalue_reference_v<T> && ( !std::is_const_v< std::remove_reference_t<T> > )
[[nodiscard]] constexpr auto&& forward( U&& u ) noexcept
{
	return static_cast< U& >(u);
}

template< typename T, typename U > requires ( !std::is_lvalue_reference_v<T> ) && std::is_const_v< std::remove_reference_t<T> >
[[nodiscard]] constexpr auto&& forward( U&& u ) noexcept
{
	return std::move( std::as_const(u) );
}

template< typename T, typename U >
[[nodiscard]] constexpr auto&& forward( U&& u ) noexcept
{
	return std::move(u);
}

template< typename... Ts >
[[nodiscard]] constexpr auto forward_as_tuple( Ts&... ts ) noexcept
{
	return std::forward_as_tuple( std::forward<Ts>(ts)... );
}

template< typename... Ts > constexpr bool is_same_v = false;
template< typename T > constexpr bool is_same_v<T> = true;
template< typename T, typename U, typename... Ts > constexpr bool is_same_v< T, U, Ts... > = std::is_same_v< T, U > && is_same_v< U, Ts... >;


template< typename... Values > requires (!std::is_reference_v<Values> && ...) && requires
{
	typename std::common_type< Values... >::type;
}
constexpr auto make_array( Values&&... values )
{
	using T = std::common_type_t< Values... >;
	return std::array< T, sizeof...(Values) >{ std::forward<Values>(values)... };
};

template< typename V > constexpr V min( V&& value )
{
	return std::forward<V>(value);
}

template< typename V1, typename V2, typename... Vs > requires requires
{
	typename std::common_type< V1, V2, Vs... >::type;
}
constexpr auto min( const V1& value1, const V2& value2, const Vs&... values )
{
	using T = std::common_type_t< V1, V2, Vs... >;
	return min( std::min( T(value1), T(value2) ), values... );
}

template< template< auto... > typename Sequence, auto... values > constexpr auto min( Sequence< values... > )
{
	return min( values... );
}

template< typename V > constexpr V max( V&& value )
{
	return std::forward<V>(value);
}

template< typename V1, typename V2, typename... Vs > requires requires
{
	typename std::common_type< V1, V2, Vs... >::type;
}
constexpr auto max( const V1& value1, const V2& value2, const Vs&... values )
{
	using T = std::common_type_t< V1, V2, Vs... >;
	return max( std::max( T(value1), T(value2) ), values... );
}

template< template< auto... > typename Sequence, auto... values > constexpr auto max( Sequence< values... > )
{
	return max( values... );
}

constexpr struct
{
	template< std::integral T > constexpr operator T() const
	{
		return std::numeric_limits<T>::max();
	}

	template< std::integral T > constexpr bool operator==( T value ) const
	{
		return value == std::numeric_limits<T>::max();
	}

	struct Negative
	{
		template< std::integral T > constexpr operator T() const
		{
			return std::numeric_limits<T>::min();
		}

		template< std::integral T > constexpr bool operator==( T value ) const
		{
			return value == std::numeric_limits<T>::min();
		}
	};

	constexpr auto operator-() const
	{
		return Negative{};
	}
}
INF;

} // namespace gain

#endif // __GAIN_BASIC_HEADER__
