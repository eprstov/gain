#ifndef __GAIN_CONVERT_HEADER__
#define __GAIN_CONVERT_HEADER__

#include "basic.h"

#include <concepts>
#include <type_traits>
#include <utility>

namespace gain
{
namespace detail::convert
{

template< typename T > concept const_deref = std::is_const_v< std::remove_reference_t<T> >;

template< typename To, typename... From >
constexpr bool is_convertible = requires( From... from, To to )
{
	{ Convert( std::forward<From>(from)..., to ) };
};

template< typename To, typename... From >
constexpr bool is_immediately_convertible = requires( From... from )
{
	{ Convert<To>( std::forward<From>(from)... ) } -> std::same_as<To>;
};

template< typename From, typename To > concept convertible_to = is_convertible< To, From >;

template< typename From, typename To > concept immediately_convertible_to = is_immediately_convertible< To, From >;

constexpr struct {} dummy;

template< typename To, typename... From >
decltype(auto) Convert( From&&... from, decltype(dummy) )
{
	return Convert<To>( std::forward<From>(from)... );
}

template< typename To, typename... From >
void Convert( From&&... from, decltype(dummy), To& to )
{
	Convert( std::forward<From>(from)..., to );
}

template< typename From > struct Result
{
	Result( From&& frm ) : from( std::forward<From>(frm) )
	{
	}

	template< typename To > requires is_convertible< To, From > && ( !is_immediately_convertible< To, From > ) && std::is_default_constructible_v<To> operator To()
	{
		To to;
		Convert< To, From >( std::forward<From>(from), dummy, to );

		return to;
	}

	template< typename To > requires is_immediately_convertible< To, From >
	operator To()
	{
		return Convert< To, From >( std::forward<From>(from), dummy );
	}

	template< typename To > requires is_convertible< To, const From > && ( !is_immediately_convertible< To, const From > ) && std::is_default_constructible_v<To> operator To() const
	{
		To to;
		Convert( from, to, dummy );

		return to;
	}

	template< typename To > requires is_immediately_convertible< To, const From >
	operator To() const
	{
		return Convert< To, const From >( from, dummy );
	}

	template< typename To > requires ( !is_convertible< To, From > && !is_immediately_convertible< To, From > && std::is_convertible_v< From, To > )
	operator To()
	{
		return static_cast<To>( std::forward<From>(from) );
	}

	From from;
};

} // namespace detail::convert

template< typename To, typename... From > requires detail::convert::is_convertible< To, From... > && ( !detail::convert::is_immediately_convertible< To, From... > ) && std::is_default_constructible_v<To>
To Convert( From&&... from )
{
	To to{};
	detail::convert::Convert< To, From... >( std::forward<From>(from)..., detail::convert::dummy, to );

	return to;
}

template< typename To, typename... From > requires detail::convert::is_immediately_convertible< To, From... >
To Convert( From&&... from )
{
	return detail::convert::Convert< To, From... >( std::forward<From>(from)..., detail::convert::dummy );
}

template< typename To, typename From > requires ( !detail::convert::is_convertible< To, From > && !detail::convert::is_immediately_convertible< To, From > && std::is_convertible_v< From, To > )
To Convert( From&& from )
{
	return static_cast<To>( std::forward<From>(from) );
}

template< typename From > auto Convert( From&& from )
{
	return detail::convert::Result<From>( std::forward<From>(from) );
}

template< typename To, typename... From >
constexpr bool convertible = detail::convert::is_convertible< To, From... > && std::is_default_constructible_v<To> || detail::convert::is_immediately_convertible< To, From... > || sizeof...(From) == 1 && std::is_convertible_v< trait::First< From... >, To >;

template< typename From, typename To > concept convertible_to = convertible< To, From >;

} // namespace gain

template< typename T, typename... Args > requires std::is_constructible_v< T, Args... >
inline T Build( Args&&... args )
{
	return T( std::forward<Args>(args)... );
}

#endif // __GAIN_CONVERT_HEADER__
