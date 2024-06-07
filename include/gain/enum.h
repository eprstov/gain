#ifndef __GAIN_ENUM_HEADER__
#define __GAIN_ENUM_HEADER__

#include <limits>
#include <type_traits>
#include <utility>

namespace gain
{

#ifdef __cpp_lib_to_underlying

using std::to_underlying;

#else

template< typename T > requires std::is_enum_v<T>
constexpr auto to_underlying( T value )
{
	return std::underlying_type_t<T>(value);
}

#endif // !__cpp_lib_to_underlying

template< auto MIN, auto MAX > requires std::is_enum_v< decltype(MIN) > && std::is_same_v< decltype(MIN), decltype(MAX) >
struct EnumLimits : std::numeric_limits< std::underlying_type< decltype(MIN) > >
{
	static constexpr auto min() noexcept
	{
		return MIN;
	}

	static constexpr auto max() noexcept
	{
		return MAX;
	}

	static constexpr auto lowest() noexcept
	{
		return MIN;
	}

	static constexpr auto highest() noexcept
	{
		return MAX;
	}
};

} // namespace gain

#endif // !__GAIN_ENUM_HEADER__
