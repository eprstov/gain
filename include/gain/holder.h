#ifndef __GAIN_HOLDER_HEADER__
#define __GAIN_HOLDER_HEADER__

#include <tuple>
#include <type_traits>

namespace gain
{
namespace detail::holder
{
namespace trait
{

template< typename T >
inline constexpr bool is_smart_pointer = std::is_class_v<T> && requires( T held )
{
	not std::is_void_v< decltype( held.operator->() ) >;
};

template< typename T >
inline constexpr bool is_dereferencable = std::is_class_v<T> && requires( T held )
{
	not std::is_void_v< decltype( held.operator*() ) >;
};

} // namespace trait

template< typename T > struct Holder
{
	template< typename... Args >
	requires std::is_constructible_v< T, Args... >
	Holder( Args&&... args ) : held( std::forward<Args>(args)... )
	{
	}

	template< typename... Args >
	requires std::is_constructible_v< T, Args... > && ( !std::is_constructible_v< T, std::tuple< Args... > > )
	Holder( std::tuple< Args... >&& args ) : held( std::make_from_tuple<T>( std::move(args) ) )
	{
	}

	Holder( const Holder& ) = default;
	Holder( Holder&& ) = default;

	Holder& operator=( const Holder& ) = default;
	Holder& operator=( Holder&& ) = default;

	template< typename... Args > requires std::is_invocable_v< T, Args... >
	decltype(auto) operator()( Args&&... args )
	{
		return std::invoke( held, std::forward<Args>(args)... );
	}

	template< typename... Args > requires std::is_invocable_v< const T, Args... >
	decltype(auto) operator()( Args&&... args ) const
	{
		return std::invoke( held, std::forward<Args>(args)... );
	}

	decltype(auto) operator->() requires trait::is_smart_pointer<T>
	{
		return held.operator->();
	}

	decltype(auto) operator->() const requires trait::is_smart_pointer< const T >
	{
		return held.operator->();
	}

	decltype(auto) operator*() requires trait::is_dereferencable<T>
	{
		return held.operator*();
	}

	decltype(auto) operator*() const requires trait::is_dereferencable< const T >
	{
		return held.operator*();
	}

	auto* operator->()
	{
		return &held;
	}

	auto* operator->() const
	{
		return &held;
	}

	auto& operator*()
	{
		return held;
	}

	auto& operator*() const
	{
		return held;
	}

	operator T&()
	{
		return held;
	}

	operator const T&() const
	{
		return held;
	}

	operator T&&() &&
	{
		return std::move(held);
	}

	T held;
};

template< typename T > struct Holder< T& >
{
	template< typename U > requires std::is_convertible_v< U*, T* >
	Holder( U& arg ) : held( &arg )
	{
	}

	Holder() = default;
	Holder( const Holder& ) = default;

	Holder& operator=( const Holder& ) = default;

	template< typename... Args > requires std::is_invocable_v< T, Args... >
	auto operator()( Args&&... args )
	{
		return std::invoke( *held, std::forward<Args>(args)... );
	}

	decltype(auto) operator->() const requires trait::is_smart_pointer<T>
	{
		return held->operator->();
	}

	decltype(auto) operator*() const requires trait::is_dereferencable<T>
	{
		return held->operator*();
	}

	auto* operator->() const
	{
		return held;
	}

	auto& operator*() const
	{
		return *held;
	}

	operator T&() const
	{
		return *held;
	}

	T* held = nullptr;
};

template< typename T > struct Holder< T* >
{
	template< typename U > requires std::is_convertible_v< U*, T* >
	Holder( U* arg ) : held(arg)
	{
	}

	Holder() = default;
	Holder( const Holder& ) = default;

	Holder& operator=( const Holder& ) = default;

	template< typename... Args > requires std::is_invocable_v< T*, Args... >
	auto operator()( Args&&... args )
	{
		return std::invoke( held, std::forward<Args>(args)... );
	}

	auto* operator->() const
	{
		return held;
	}

	auto& operator*() const
	{
		return *held;
	}

	operator T*() const
	{
		return held;
	}

	T* held = nullptr;
};

template<> struct Holder<void>
{
};

template< typename T > Holder(T) -> Holder<T>;

} // namespace detail::holder

using detail::holder::Holder;

} // namespace gain

#endif // __GAIN_HOLDER_HEADER__
