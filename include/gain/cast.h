#ifndef __GAIN_CAST_HEADER__
#define __GAIN_CAST_HEADER__

#include <type_traits>

namespace gain
{
namespace trait
{
namespace detail
{

template< typename From, typename To >
struct CopyRef
{
	using R = std::remove_reference_t<To>;
	using U1 = std::conditional_t< std::is_const_v<From>, std::add_const_t<R>, R >;
	using U2 = std::conditional_t<std::is_volatile_v<From>, std::add_volatile_t<U1>, U1>;
	using U3 = std::conditional_t< std::is_lvalue_reference_v<From>, std::add_lvalue_reference_t<U2>, U2 >;
	using U4 = std::conditional_t< std::is_rvalue_reference_v<From>, std::add_rvalue_reference_t<U3>, U3 >;

	using Type = U4;
};

} // namespace detail

template< typename From, typename To >
using CopyRef = typename detail::CopyRef< From, To >::Type;

} // namespace trait

template< typename To, typename From >
inline auto&& forward_cast( From&& from )
{
	return static_cast< trait::CopyRef< From, To > >(from);
}

namespace detail
{

template< template< typename... > typename Base, typename... Params > Base< Params... >& template_cast( const Base< Params... >& );
void template_cast( const auto& );

} // namespace detail

template< template< typename... > typename Base, typename T >
using template_cast = std::remove_reference_t< decltype( detail::template_cast<Base>( std::declval<T>() ) ) >;

template< typename T, template< typename... > typename Template >
concept from_template = ( not std::is_void_v< template_cast< Template, T > > );

template< bool condition, typename T > using add_const_if = std::conditional_t< condition, std::add_const_t<T>, T >;

template< typename U, typename T > using add_const_as = add_const_if< std::is_const_v< std::remove_reference_t<U> >, T >;

} // namespace gain

#endif // !__GAIN_CAST_HEADER__
